
;;; Set menu and icons for all modes 
;; As of now only icons
;; For Buffer menu see `br-simple-buffer-menu.el`

(require 'ess-site)

(defun br-init-general-icons()
  "Set icons not mode-specific."
  (tool-bar-add-item "splith" 'br-toggle-split-h
		     'br-toggle-split-h
		     :help   "Toggle horizontal split"))


(defun ess-eval-line-and-step-xx (&optional simple-next even-empty invisibly)
  "Evaluate the current line visibly and step to the \"next\" line.
If SIMPLE-NEXT is non-nil, possibly via prefix arg, first skip
empty and commented lines. If 2nd arg EVEN-EMPTY [prefix as
well], also send empty lines.  When the variable `ess-eval-empty'
is non-nil both SIMPLE-NEXT and EVEN-EMPTY are interpreted as
true."
  ;; From an idea by Rod Ball (rod@marcam.dsir.govt.nz)
  (interactive "P\nP"); prefix sets BOTH !
  (ess-force-buffer-current "Process to load into: ")
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)
      ;; go to end of process buffer so user can see result
      (ess-eval-linewise (buffer-substring (point) end)
                         invisibly 'eob (or even-empty ess-eval-empty))))
  (if (or simple-next ess-eval-empty even-empty)
      (forward-line 1)
    (ess-next-code-line 1)))


(define-key ess-mode-map
       [menu-bar ESS sync]
       '("Set directory to this file" . ess-use-this-dir))

(defun br-init-ess-icons()
  "Set ESS icons. Run after `br-init-general-icons` to inherit its icons."
  
  ;; Remove S-plus icon
  (setq ess-toolbar-items
 	(quote
 	 ((R "startr" "Start R process only")
 	  (ess-eval-line-and-step "rline" "Eval line & step")
 	  (ess-eval-region "rregion" "Eval region")
 	  (ess-eval-function-or-paragraph-and-step "rregion" "Eval function or paragraph and step")
 	  (ess-load-file "rbuffer" "Load file")
 	  (ess-eval-function "rfunction" "Eval function")

 	  (ess-use-this-dir "switchr" "Set directory to this file")

	  )))

  (ess-make-toolbar))


(defun br-toggle-split-h ()
 "Toggle horizontal split"
 (interactive)
 (if (= 1 (count-windows))
     (split-window-below)
   (delete-window))
 (message "Click again to toggle"))

(defun br-init-menico()
  "Call specific and generic icon and menu functions."
  (br-init-general-icons)
  (br-init-ess-icons))


(br-init-menico)
(provide 'br-menico)

