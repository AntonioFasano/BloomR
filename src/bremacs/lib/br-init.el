

(defun br-init-paths ()
  
  (setq user-emacs-directory
	(expand-file-name (concat invocation-directory "../.emacs.d/")))

  ;; We do not want emacs.d in HOME, so we change the initial HOME shell value set to host emacs.d
  (setenv "HOME"
	  (expand-file-name (concat invocation-directory "../../../mybloomr")))

  ;; Without embedded newllines in path one might replace  \` and \' with \^ and \$
  (setq abbreviated-home-dir (concat 
			      "\\`" 
			      (getenv "HOME")
			      "\\(/\\|\\'\\)"))
  
  (setq abbrev-file-name ; instead of $HOME/emacs.d/abbrev_defs
 	(locate-user-emacs-file "abbrev_defs"))

  (setq auto-save-list-file-prefix ; based on user-emacs-directory
 	(locate-user-emacs-file "auto-save-list/.saves-"))

  (setq br-pdf-viewer (expand-file-name
		       (concat user-emacs-directory "../../Sumatra/SumatraPDF.exe")))

  (setq br-rterm 
	(expand-file-name (concat invocation-directory "../../R/bin/x64/Rterm.exe")))

  

  ;; Consider file-truename as an alt to expand-file-name
  (cd "~"))


(defun br-init-visual()
  "Mostly visual settings plus keyboard feedeback."
  (setq inhibit-splash-screen t)  ; for site-start invoke with  --no-splash
  (modify-frame-parameters  nil (quote ((fullscreen . maximized))))
  (set-face-attribute 'default  nil :height 146) 
  (show-paren-mode t)
  (setq cua-enable-cursor-indications t)
  (setq cua-overwrite-cursor-color "yellow")
  (setq cua-read-only-cursor-color "green")

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
  
  (setq ess-tab-complete-in-script t); Auto-complete in ESS code mode if there is nothing to indent
  (setq ess-first-tab-never-complete 'symbol) ; Complete, yes, unless following char is a symbol
  (setq ess-ask-for-ess-directory nil) ;don't prompt for data dir
  (setq ess-history-directory user-emacs-directory)
  ;; br-R-exit-command defaults to q('no')
  (setq initial-major-mode 'R-mode)
  (setq initial-scratch-message "R scratchpad")
  (setq-default major-mode 'R-mode)
  (setq inferior-R-program-name br-rterm)


  ;; Rprofile specific for EMACS/Rterm
  ;; Currently patches install.packages to use tcltk
  (setf (cdr (assoc 'inferior-ess-start-file ess-r-customize-alist))
	(concat (file-name-directory (locate-library  "br-init")) "ess-init.R")))


(defun br-init-packs()
  (package-initialize)
  (require 'ess-site)
  (require 'markdown-mode)
  (require 'br-setmodes)
 ; (require 'br-rnw) 
  (require  'br-simple-buffer-menu)
  (br-init-simple-menu))

(defun br-init-history()
  (savehist-mode 1) ; minibuffer hist
  (require 'br-recentf))

(defun br-init-key()
  "Global key bindings"
  (cua-mode t)
  (setq completion-styles '(partial-completion initials))
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key [(control tab)] 'other-window)
  (setq tab-always-indent 'complete) ; ESS uses a different var!
  (global-set-key  (kbd "ESC ESC") 'keyboard-escape-quit)

  ;; search 
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "<f3>")  'isearch-forward)
  (global-set-key (kbd "S-<f3>")  'isearch-backward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)

  (global-set-key (kbd "C-w")  '(lambda ()
				  "Kill current buffer."
				  (interactive) (kill-buffer (current-buffer))))

  )



(defun br-init-main()  
  (br-init-paths)
  (br-init-packs)
  (br-init-visual)
  (br-init-set-ess)
  (br-init-history)
  (require 'br-keys)

 (let ((inith "~/init.el")
       (inite (concat user-emacs-directory "init.el")))
    (if (file-exists-p inith) (load inith)
      (if (file-exists-p inite) (load inite)))) 
 )
  

(br-init-main)


