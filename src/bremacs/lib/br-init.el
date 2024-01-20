;;; br-init.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; Set paths, path finding functions and general settings.

;;; Code:

(defconst br-bloomr-dir
  (or (getenv "BLOOMR") (error "BRemacs launcher did not set `BLOOMR' environment variable"))
  "Absolute path to directory where BloomR files have been extracted.
It is set at init time based on \"BLOOMR\" environment variable.
\"BLOOMR\" variable is dynamical set by the BRemacs launcher,
i.e. \"bloomr-lab.exe\" or \"bloomr-studio.exe\", based on its invocation directory.")

(defconst br-app-dir nil "Absolute path to BRemacs \"apps\" directory designed to host BloomR applications.")

(defconst br-bremacs-dir nil "Absolute path to BRemacs app directory.")

(defconst br-site-lisp-dir nil
  "Absolute path to \"share/emacs/site-lisp\" in the BRemacs app directory.")
  
(defconst br-infodirs nil "List of infodirs paths found in `br-site-lisp-dir'.
An \"infodirs paths\" is basically a package dir with a \"dir\" file.
The list is used by `br-init-infodir' as part of our simplfied
pacakge activation process.")

(defvar br-pdf-viewer nil "Location of local PDF viewer.
Currently default location of Sumatra viewer")

(defvar br-rterm  nil
  "Location of local R x64 executable \"rterm\".")

(defun br-init-paths ()
  "BRemacs system directories are kept separated by the HOME dir.
This is intended to keep the product user friendly, and have HOME
operate like the Windows \"My Documents\" folder.

BloomR/BRemacs launcher sets:
  1. HOME enviroment variable to \"$BLOOMR/mybloomr\",
     where \"$BLOOMR\" denotes the BloomR setup directory,
  2. `user-emacs-directory' to \"$BLOOMR/apps/bremacs/.emacs.d\",
     via the invocation  \"--init-dir apps\\bremacs\\.emacs.d\".

This function further redirects system directories to `user-emacs-directory'.
Note `package-user-dir' is similarly redirected  in the `early-init.el'"
  
  ;; This value is supposed to match what is set via the launcher
  (let ((launch-idir (concat (br-expand-as-dir (getenv "BLOOMR")) "apps/bremacs/.emacs.d/"))
	(our-idir (br-expand-as-dir user-emacs-directory)))
    (unless (string= launch-idir our-idir)
      (warn (format "BloomR launcher was supposed to the set init dir to \n%s, but the value found is \n%s"
		    our-idir launch-idir))))  

  ;;`abbreviated-home-dir' var allows to use (abbreviate-file-name "$HOME/path") -> "~/path"
  ;; We could also set it to nil, then (abbreviate-file-name ..) would recalculate it.
  (setq abbreviated-home-dir (concat
			      "\\`"
			      (getenv "HOME")
			      "\\(/\\|\\'\\)"))
  ;; Without embedded newllines in path one might replace  \` and \' with \^ and \$

  ;; Redirect some file normally located in $HOME to .emacs.d init dir
  (setq abbrev-file-name ; instead of $HOME/emacs.d/abbrev_defs
 	(locate-user-emacs-file "abbrev_defs"))

  (setq auto-save-list-file-prefix ; based on user-emacs-directory
 	(locate-user-emacs-file "auto-save-list/.saves-"))

  ;; Set BloomR specific paths
  (setq br-app-dir       (br-locate-bloomr-path "apps"))
  (setq br-bremacs-dir   (br-locate-app-path "bremacs"))
  (setq br-site-lisp-dir (br-locate-app-path "bremacs/share/emacs/site-lisp"))
 
  ;; Set executable paths
  (setq br-pdf-viewer (br-locate-app-path "Sumatra/SumatraPDF.exe"))
  (setq br-rterm (br-locate-app-path "R/bin/x64/Rterm.exe"))
  
  ;; Change to home
  (cd "~"))


(defun br-expand-as-dir (path)
  "Make PATH absolute and canonical, and interpret it as a directory.
The latter means generally removing the trailing (back)slash."
     (file-name-as-directory (expand-file-name path)))

(defun br-locate-bloomr-path (path)
  "If PATH is relative, prefix with `br-bloomr-dir' and return as an absolute path.
If PATH is absolute return return it as is."
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-bloomr-dir))))

(defun br-locate-app-path (path)
  "If PATH is relative, prefix with `br-app-dir' and return as an absolute path.
If PATH is absolute return return it as is."
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-app-dir))))

(defun br-locate-bremacs-path (path)
  "If PATH is relative, prefix w/ `br-bremacs-dir' and return as an absolute path.
If PATH is absolute return return it as is."
  (expand-file-name path
		    (file-name-as-directory (expand-file-name br-bremacs-dir))))

(declare-function ess-eval-linewise "ess-inf.el")
(defun br-text-help (); currently not used
  "Use BRemacs for R help, rather than the HTML help."

  (add-hook 'ess-post-run-hook
    	    (lambda()
    	      (when (and (bound-and-true-p ess-dialect) (string= ess-dialect "R"))
    		(ess-eval-linewise "options(chmhelp=FALSE, help_type=\"text\")"
    				   nil nil nil 'wait))))
  ;; new frame for each help instance
  (setopt ess-help-own-frame t ess-help-reuse-window nil))


(defun br-init-packs ()
  "Run our packages."
  ;; (package-initialize); getting away with it saves a lot of startup time 
  (require 'br-setmodes)
  (require 'br-simple-buffer-menu)
  (require 'bm))


(defun br-init-visual ()
  "Mostly visual settings plus keyboard feedback.
Some features are in \"early-init.el\" for speed."

  ;; Material moved to early-init.el
  ;; (set-face-attribute 'default  nil :height 146)
  ;; (modify-frame-parameters  nil (quote ((fullscreen . maximized)))); readapted
  ;; (setopt scroll-conservatively 10000)      ; keyboard scroll: high number causes one line at a time
  ;; (setopt scroll-preserve-screen-position t); page up and down, find the original line

  
  (show-paren-mode t)
  (setq next-error-message-highlight t) ; Highlight error messages
  ;; (which-function-mode); might confuse beginners

  ;; Typing
  (cua-mode t)
  ;; (setopt cua-overwrite-cursor-color "yellow"); Change colors? 
  ;; (setopt cua-read-only-cursor-color "green")
  (setopt cua-auto-tabify-rectangles nil); Don't convert spaces in tabs when pasting rectangles
  (setq visible-bell t)
  (setq completion-styles '(partial-completion initials))
  (setq scroll-conservatively 10000) ; high number causes one line at a time
  (setq scroll-preserve-screen-position t) ;page up and down, find the original line
  (setq tab-always-indent t); First indent, then complete. It works for ESS too
  (setq completions-detailed t) ;; e.g show docstring for function completions in C-h f
  (setq use-short-answers t)            ; Replace 'y-or-n-p' for 'yes-or-no-p'
  ;; Do not allow the cursor over the minibuffer prompt string
  (customize-set-variable 'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt)))
       
  ;; Bookmarks
  (setopt bm-in-lifo-order t)                           ; Cycle in LIFO order
  (setopt bm-cycle-all-buffers t)                       ; Allow to change buffer
  (setopt bm-highlight-style 'bm-highlight-only-fringe) ; Left fringe
 
  ;; Menu and Icons. It requires ess-mode, ess-mode, and ess-toolbar
  (require 'br-menico)
)


(defun br-init-set-ess ()
  "ESS/R key bindings that you want to keep handy.
Global variables that might be subject to frequent change.
The rest goes to br-setmodes.el."
  
  (setopt ess-auto-width 'window)             ; adapts R console width to buffer (nice for vertical consoles)
  (setopt ess-first-tab-never-complete 'symbol); Complete? yes, unless following char is a symbol
                                               ; (cf. tab-always-indent)
  (setopt ess-ask-for-ess-directory nil); don't prompt for data dir
  (setopt ess-history-directory user-emacs-directory)
  (setopt inferior-ess-r-program br-rterm)
  (setopt inferior-R-args "LANGUAGE=en --quiet")
  
  (setq-default major-mode 'text-mode)
  ;; want R as default mode instead? 
  ;; (setq initial-major-mode 'R-mode)
  ;; (setq initial-scratch-message "R scratchpad")
  ;; (setq-default major-mode 'R-mode)

  
  ;; Inject a file at rterm start, but now we use a condiional \etc\Rprofile.site, 
  ;;(add-hook 'ess-r-post-run-hook (lambda ()
  ;; 				   (ess-load-file
  ;; 				    (concat (file-name-directory
  ;; 					     (locate-library  "br-init")) "ess-init.R"))))

  ;; show R help in BRemacs?
  ;; br-text-help() 

  )


(defun br-init-history ()
  "Manage BRemacs commands and paths history."
  (savehist-mode 1) ; minibuffer hist
  (require 'br-recentf))


(defun br-init-simple-package-initialize ()
  "A simplified for speed `package-initialize'.
Read `br-init-autoloads' and `br-init-infodir' docstrings for info."

  (br-init-autoloads)
  (br-init-infodir))

(defun br-init-autoloads ()
  "Unless `package-initialize' was called, activate `br-site-lisp-dir' autoloads.

Activating autoloads via `package-initialize' is extremely
costly.  However, here we work in a predicatable environment, so
we can afford a speedier simplified activation.  If expected
autoloads are not found a warning is emettited.  This function
only activates autoloads found in `br-site-lisp-dir' directories
and WITHOUT recursively descend into subdirectories.  It skips
\"bremacs\" directory, as the library is supposed to be loaded at
startup entirely. A possible future alternative could be a
preshipped cache, based on `package-quickstart', to be generated
during BloomR build.

To avoid a second file system query, we also fill now
`br-infodirs', to be used later by `br-init-infodir'."

  (unless (bound-and-true-p package--initialized)    
    (mapcar (lambda (pkg-dir)
	      (let* ((pkg-name (file-name-nondirectory pkg-dir))
       		     (pkg-name-nonversion (replace-regexp-in-string "-[0-9.]*$" "" pkg-name))
		     (pkg-autoload-basename (format "%s-autoloads.el" pkg-name-nonversion))
		     (pkg-autoload-path (expand-file-name pkg-autoload-basename pkg-dir))
		     (pkg-infodir-path (expand-file-name "dir" pkg-dir)))
		(unless (string= pkg-name-nonversion "bremacs")

		  (if (file-exists-p pkg-autoload-path)
		      (load pkg-autoload-path 'noerror) ; no error on no *-autoloads, but warn
		      (warn "Expected autoload missing:\n%s" pkg-autoload-path))

		  (when (file-exists-p pkg-infodir-path)
		    (setq br-infodirs (append br-infodirs pkg-dir))))))
	    
	    (seq-filter 'file-directory-p
			(directory-files br-site-lisp-dir t directory-files-no-dot-files-regexp)))))

(declare-function info-initialize "info.el")
(defun br-init-infodir ()
  "Unless `package-initialize' called, find info dirs in `br-site-lisp-dir'.
Add them to `Info-directory-list'.

This function is simplified for speed with respect to
`package-initialize'.  See `package-initialize' docstring to learn
why and how.  In particular, this function uses the variable
`br-infodirs' filled there.  The reason to keep it separate, is to
possibly use it as a hook for further speed."

  (unless (bound-and-true-p package--initialized)
    (require 'info)
    (info-initialize)
    (if (bound-and-true-p Info-directory-list)
	(mapc (lambda (pkg-dir)	
		  (push pkg-dir Info-directory-list))
		br-infodirs))
    (warn "`Info-directory-list' is not defined, info dirs not updated")))

(defun br-init-main ()
  "The kernel function."
  (br-init-paths)
  (br-init-packs)
  (br-init-visual)
  (br-init-history); seems sensible to run the recent-dlg after visual init is done
  (br-init-set-ess)
  (require 'br-keys)
  (br-init-simple-package-initialize)

  ;; If there is a file named user.el in HOME (mybloomr), eval it
  (let ((myinit "~/user.el"))
	(if (file-exists-p myinit) (load-file myinit))))
  

(br-init-main)
;;; br-init.el ends here
