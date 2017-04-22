

;; (require 'ess-site) ; assumed in your init file
 
(defun br-knit-rnw-clear () 
  "Delete intermediate LaTeX files and run `br-knit-rnw'.
These are based on extensions .aux, .blg, .out, .run.xml, .bbl, .log, -blx.bib"
 
  (interactive)
  (br-check-rnw)
  (let
      ((file)
       (stem (file-name-sans-extension (buffer-file-name))))
    (dolist (elt
	     (list ".aux" ".blg" ".out" ".run.xml" ".bbl" ".log" "-blx.bib"))
      (setq file (concat stem elt))
      (if (file-exists-p file) (delete-file file))))  
  (br-knit-rnw))
 
 
(defun br-knit-rnw () 
  "Knit->LaTeX-engine->bibtex-engine->LaTeX-engine->View.
Default LaTeX engine is \"pdflatex\" and can be customised with `br-latex-engine';
default LaTeX arguments are set to nil and can be customised with `br-latex-args';
default PDF viewer is set to nil and can be customised with `br-pdf-viewer'.
Bibliography must be set via \"biblatex\" LaTeX package.
Bibliography engine is obtained from \"backend\" option in \"biblatex\" package.
A reference  LaTeX bib file is obtained from the first LaTeX command \"\addbibresource{foo.bib}\".
The biblatex-engine is run if the bib file is newer of the TeX file. 
If there are multiple \"\addbibresource\" only the first will be used to decide whether to run the biblatex-engine."
  
  (interactive)
 
  ;; Default values
  (defvar br-pdf-viewer nil)
  (defvar br-latex-engine "pdflatex")
  ;; -file-line-error/-c-style-errors are used by AucTeX "C-c `"
  ;; -shell-escape/-enable-write18 is used by \tikzexternalize
  (defvar br-latex-args nil)
 
  (br-check-rnw)
 
  ;;If 1 R-proc found, associate it with buffer;
  ;;if many found, ask to choose one; if none found, launch and associate
  (ess-force-buffer-current "Process to use: ")
 
  ;;Save Rnw buffer
  (save-buffer)
 
  
  (let*
      (;;Set file paths
       (pathstem (file-name-sans-extension (buffer-file-name)))
       (namestem (file-name-nondirectory pathstem))
       (cur-dir	    (file-name-directory pathstem))
       (rnw-name    (concat namestem ".Rnw"))
       (tex-name    (concat namestem ".tex"))
 
       ;;Create LaTeX commmand
       (br-latex-args (concat br-latex-args " " namestem))
       (latex-cmd (concat br-latex-engine " " br-latex-args))
 
       ;;Create knit commmand
       (knit-cmd (format "require(knitr); setwd('%s'); knit('%s')"  cur-dir rnw-name))
     
       ;;Get R buffer proc
       (r-proc (ess-get-process))
       (r-buf (ess-get-process-buffer))
 
       ;;TeX executable process and bibtex engine/file
       (tex-proc)
       (tex-buf)
       (bibfile (bib-getfile))
       (bibengine (bib-getengine))
       (bibfile-updated
	(file-newer-than-file-p
	 (concat cur-dir (file-name-nondirectory bibfile) ".bib") (concat pathstem ".tex")))
 
       
       ;;Command success
       (success nil)
       (error-msg "")
       )
 
 
    (setq default-directory cur-dir)
 
    ;; Exit on error
    (catch 'exit-func
 
      ;;Check bibtex file and engine
      (when (not bibfile)
	(setq error-msg (bib-getfile t))
	(throw 'exit-func nil))	    
      (when (not bibengine)
	(setq error-msg (bib-getengine t))	
	(throw 'exit-func nil))
 
      ;; Biber wants .bib
      (let ((fail (and (string= bibengine "biber")
		      (string= (file-name-nondirectory bibfile) (file-name-base bibfile)))))
	(setq success (not fail)))
      (when (not success)
	(setq error-msg
	      (format "biber wants \\addbibresource{%s%s}" (file-name-base bibfile) ".bib"))
	(throw 'exit-func nil))
 
 
      ;; Knitting
      (switch-to-buffer-other-window r-buf)
      (message knit-cmd)
      (ess-eval-linewise knit-cmd nil t nil t) 
      ;; Foll. 3 lines are an alternative to ess-eval
      ;; (inferior-ess-mark-as-busy r-proc)  ; knit immediately after this line	      
      ;; (process-send-string r-proc (format "cat(\"%s\");%s\n" knit-cmd knit-cmd)) ; real 
      ;; (ess-wait-for-process r-proc nil)
 
      ;; Check for knitting results
      (with-current-buffer r-buf
	;; Parse last 3 lines
	(let ((beg) (end) (out))
	  (goto-char (point-max))
	  (setq end (point))
	  (forward-line -3) 
	  (setq beg (point))
	  (setq out (buffer-substring-no-properties beg end))
	  
	  ;; Knitting successful?
	  (setq success "output file: %s\n\n[1] \"%s\"\n> ")
	  (setq success (string= (format success tex-name tex-name) out))))
 
      (when (not success)
	(setq error-msg (concat "Unable to knit " rnw-name))
	(throw 'exit-func nil))
 
      ;; First LaTeXing
      (setq tex-buf (get-buffer-create "TeX-output")) ; Create output buffer or use existing
      (with-current-buffer tex-buf		     
	(buffer-disable-undo)
	(erase-buffer))
      (message "1st latex ...")
      (br-send-r-mess (format "Starting LaTeX (see \"%s\")" (buffer-name tex-buf)))	 
      (br-send-r-mess latex-cmd)
      (setq success (= 0 (call-process br-latex-engine nil tex-buf t br-latex-args)))
      (goto-char (point-max))
      
      ;; Check 1st LaTeX results
      (when (not success)
	(setq error-msg (concat "Unable to LaTeX " namestem))
	(switch-to-buffer-other-window tex-buf) 
	(other-window 1)
	(throw 'exit-func nil))
  
      ;; Run bibtex engine
      (when bibfile-updated  
	(message "biblatex ...")
	(br-send-r-mess (concat bibengine " "  namestem))
	(setq success (= 0 (call-process bibengine nil tex-buf t namestem)))
	(goto-char (point-max))
 
	;; Check bibtex results
	(when (not success)
	  (setq error-msg (concat "Unable to " bibengine " " namestem))
	  (switch-to-buffer-other-window tex-buf) 
	  (other-window 1)
	  (throw 'exit-func nil)))
 
      ;; Second LaTeXing
      (message "2nd latex ...")
      (br-send-r-mess latex-cmd)
      (setq success (= 0 (call-process br-latex-engine nil tex-buf t br-latex-args)))
      (goto-char (point-max))
 
      ;; Check 2nd LaTeX results
      (when (not success)
	(setq error-msg (concat "Unable to LaTeX " pathstem))
	(switch-to-buffer-other-window tex-buf) 
	(other-window 1)
	(throw 'exit-func nil))
      
      ;; View	
      (if (not br-pdf-viewer) (throw 'exit-func nil))
      (br-send-r-mess  "...and now the viewer")
      (goto-char (point-max))
      (setq success (file-exists-p br-pdf-viewer))
      (when (not success)
	(setq error-msg (concat "Can\\'t find executable " br-pdf-viewer))
	(throw 'exit-func nil))
 
      ;; If you need viewer console output, use "(start-process "pdf-viewer" tex-buf ...";
      ;; but you block tex-buf buffer till the viewer is open
      (start-process "pdf-viewer" nil br-pdf-viewer (concat namestem ".pdf")))
    
    (if success
	(if bibfile-updated (message (concat "Updated to "  (file-name-nondirectory bibfile))))
      (message error-msg)
      (br-send-r-mess error-msg))))
 
(defun bib-getfile(&optional show-messages)
  "Check if 'addbibresource' command and related file exist. 
If found, return .bib file full path, else:
if SHOW-MESSAGES is nil return nil, if SHOW-MESSAGES is non-nil return related error."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "\\\\addbibresource{\\(.+\\)}" nil t))
  (let ((fmatch (match-string-no-properties 1)) 
	(success nil)
	mess)	 
    (cond 
     ((not fmatch) (setq mess "Missing \\addbibresource command."))
     ((not (file-exists-p (concat (file-name-sans-extension fmatch) ".bib")))
      (setq mess (concat "Missing file: " fmatch ".bib")))
     ;; if no problem, sucess=message=bib-file-path
     (t (setq mess (concat (file-name-directory (buffer-file-name)) fmatch)
	      success mess)))
 
    (if show-messages mess success)))
 
(defun bib-getengine(&optional show-messages)
  "Find biblatex engine.
If found,  engine name, else:
if SHOW-MESSAGES is nil return nil, if SHOW-MESSAGES is non-nil return related error."
  (save-excursion
    (goto-char (point-min))
    (let ((pack (re-search-forward "\\\\usepackage *\\(\\[[^]]*\\)\\] *{ *biblatex *}" nil t))
	  (bend nil)
	  mess)
 
      (when pack (setq pack (match-string-no-properties 1)))
      (when (and pack
		 (string-match "[^[:alpha:]]+backend *= *\\([^, \n]+\\)" pack))
	(setq bend (match-string 1 pack)))
      (cond 
       ((not pack) (setq mess "Missing biblatex package command."))
       ((not bend) (setq mess "Missing biblatex backend."))
       ;; if no problem, sucess=message=bib-file-path
       (t (setq mess bend)))
      (if show-messages mess bend))))
 
 
(defun br-send-r-mess (mess)
  "Just send MESS at the end of R console buffer"
  (process-send-string (ess-get-process)
		     (format "cat('%s\\n')\n" mess)))
 
(defun br-check-rnw ()
  "Give error if `ess-dialect' is not \"R\""
  (if (not (string= "R" ess-dialect))
      (error "Not an Rnw buffer")))

(provide 'br-rnw)
