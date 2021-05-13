(defvar br-bloomr-dir 
  (or (getenv "BLOOMR") (error "BRemacs launcher did not set `BLOOMR' environment variable."))
  "Absolute path to directory where BloomR/BRemacs files have been extracted.
It is set at init time based on \"BLOOMR\" environment variable.
\"BLOOMR\" variable is dynamical set by the BRemacs launcher, i.e. \"bremacs.exe\", based on its invocation directory.")

(defvar br-app-dir nil 
  "Absolute path to BRemacs \"apps\" directory designed to host BloomR applications.")

(defvar br-bremacs-dir nil 
  "Absolute path to BloomR/BRemacs directory designed to host BRemacs distro files.")

(defvar br-pdf-viewer nil "Location of local PDF viewer.
Currently default location of Sumatra viewer")

(defvar br-rterm  nil
  "Location of local R x64 executable \"rterm\"")


(defun br-init-paths ()

  ;; .emacs.d is set relative to Emacs invocation dir
  (setq user-emacs-directory
	(br-expand-as-dir (concat invocation-directory "../.emacs.d/")))

  ;; Manage HOME dir
  ;; ---------------
  ;; We do not want emacs.d in HOME, so we change the initial HOME shell value set to host emacs.d
  (setenv "HOME"
	  (br-expand-as-dir (concat invocation-directory "../../../mybloomr")))

  ;;(locate-user-emacs-file) -> (abbreviate-file-name) uses abbreviated-home-dir
  (setq abbreviated-home-dir (concat			      
			      "\\`" 
			      (getenv "HOME")
			      "\\(/\\|\\'\\)"))
  ;; Without embedded newllines in path one might replace  \` and \' with \^ and \$

  ;; Reset stadandard files to be located .emacs.d
  ;; ---------------------------------------------  
  (setq abbrev-file-name ; instead of $HOME/emacs.d/abbrev_defs
 	(locate-user-emacs-file "abbrev_defs"))

  (setq auto-save-list-file-prefix ; based on user-emacs-directory
 	(locate-user-emacs-file "auto-save-list/.saves-"))

  ;; Set BloomR specific paths
  ;; -------------------------
  (setq br-app-dir  (br-locate-bloomr-path "apps"))
  (setq br-bremacs-dir  (br-locate-app-path "bremacs"))

  (setq br-pdf-viewer (br-locate-app-path "Sumatra/SumatraPDF.exe")) 
  (setq br-rterm (br-locate-app-path "R/bin/x64/Rterm.exe"))
  
  
  ;; Consider file-truename as an alt to expand-file-name
  (cd "~"))


(defun br-expand-as-dir (path)
  "Make PATH absolute and canonical, and interpret it as a directory. 
The latter means generally removing the trailing (back)slash."
     (file-name-as-directory (expand-file-name path)))

(defun br-locate-bloomr-path (path)
  "If PATH is absolute return PATH, else return it as an absolute path relative to `br-bloomr-dir'"
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-bloomr-dir))))

(defun br-locate-app-path (path)
  "If PATH is absolute return PATH, else return it as an absolute path relative to `br-app-dir'"
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-app-dir))))

(defun br-locate-bremacs-path (path)
  "If PATH is absolute return PATH, else return it as an absolute path relative to `br-bremacs-dir'"
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-bremacs-dir))))

(defun br-init-visual()
  "Mostly visual settings plus keyboard feedback."
  (setq inhibit-splash-screen t)  ; for site-start invoke with  --no-splash
  (modify-frame-parameters  nil (quote ((fullscreen . maximized))))
  (set-face-attribute 'default  nil :height 146) 
  (show-paren-mode t)
  (setq cua-enable-cursor-indications t)
  (setq cua-overwrite-cursor-color "yellow")
  (setq cua-read-only-cursor-color "green")
  (setq visible-bell t) 


  ;; Keyboard
  (cua-mode t)
  (setq completion-styles '(partial-completion initials))
  (setq scroll-conservatively 10000) ; high number causes one line at a time
  (setq scroll-preserve-screen-position t) ;page up and down, find the original line
  
  
  ;; Bookmarks
  (setq bm-in-lifo-order t)                           ; Cycle in LIFO order
  (setq bm-cycle-all-buffers t)                       ; Allow to change buffer
  (setq bm-highlight-style 'bm-highlight-only-fringe) ; Left fringe
  ;; (which-function-mode)  ;; May confuse beginners

  ;; Menu and Icons. Requires ESS
  (require 'br-menico)
)



(defun br-init-set-ess()
  "ESS/R key bindings that you want to keep handy. Global variables that might be subject to frequent change.
The rest goes to br-setmodes.el."
  
  (setq tab-always-indent t); Auto-complete in ESS code mode if there is nothing to indent
  (setq ess-first-tab-never-complete 'symbol) ; Complete, yes, unless following char is a symbol
  (setq ess-ask-for-ess-directory nil) ;don't prompt for data dir
  (setq ess-history-directory user-emacs-directory)
  ;; br-R-exit-command defaults to q('no')
  (setq initial-major-mode 'R-mode)
  (setq initial-scratch-message "R scratchpad")
  (setq-default major-mode 'R-mode)
  (setq inferior-ess-r-program br-rterm)

  ;; Inject main\bremacs\share\emacs\site-lisp\bremacs\ess-init.R in R console on start
  ;; This is equivalent to standard \etc\Rprofile.site, but is run only when starting R from BRemacs
  (add-hook 'ess-r-post-run-hook (lambda ()
				   (ess-load-file
				    (concat (file-name-directory
					     (locate-library  "br-init")) "ess-init.R"))))

  ;; Use BRemacs for help, but currently we prefer HTML help
  ;; (add-hook 'ess-post-run-hook
  ;;  	    (lambda()
  ;;  	      (when (string= ess-dialect "R")
  ;;  		(ess-eval-linewise "options(chmhelp=FALSE, help_type=\"text\")"
  ;;  				   nil nil nil 'wait))))
  ;; (setq ess-help-own-frame t ess-help-reuse-window nil) ; new frame for each help instance

  )


(defun br-init-autoloads ()
  "Activate autoloads in site-lisp subdirectories and set `Info-directory-list'. 
The function does NOT recursively descend into site-lisp subdirectories."
  
  (mapcar (lambda (pkg-dir)
	    (let*   ((pkg-name (file-name-nondirectory pkg-dir))
		     (pkg-autoload-basename (format "%s-autoloads.el" pkg-name))
		     (pkg-autoload-path (expand-file-name pkg-autoload-basename pkg-dir)))

	      (load pkg-autoload-path 'noerror) ; no error on no *-autoloads.el
	      
	    ;; Add dir to directories searched by Info, if dir file found
	    (when (file-exists-p (expand-file-name "dir" pkg-dir))
              (require 'info)
              (info-initialize)
              (push pkg-dir Info-directory-list))))
	  
	  (let ((site-lisp (expand-file-name "share/emacs/site-lisp"    br-bremacs-dir)))
	    (seq-filter 'file-directory-p
			(directory-files site-lisp t directory-files-no-dot-files-regexp)))))

(defun br-init-packs()
;;  (package-initialize) not necessary with latest emacs 
  (require 'br-setmodes)
  (require 'br-simple-buffer-menu)
  (declare-function br-init-simple-menu "br-simple-buffer-menu.el" ())
  (br-init-simple-menu)
  (require 'bm))

(defun br-init-history()
  (savehist-mode 1) ; minibuffer hist
  (require 'br-recentf))

(defun br-init-main()  
  (br-init-paths)
  (br-init-autoloads)
  (br-init-packs)
  (br-init-visual)
  (br-init-set-ess)
  (br-init-history)
  (require 'br-keys)

 (let ((inith "~/init.el")
       (inite (locate-user-emacs-file "init.el")))
    (if (file-exists-p inith) (load (file-name-sans-extension inith))
      (if (file-exists-p inite) (load (file-name-sans-extension inite))))) 
 )
  

(br-init-main)

