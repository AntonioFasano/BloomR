;;; br-menico.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; Set menu and icons for all modes
;; As of now only icons
;; For Buffer menu see `br-simple-buffer-menu.el`

;;; Code:


(require 'ess-mode); contains ess-mode-map we want to patch
(require 'ess-toolbar)
;; If autolads are active, can be omitted, but the user is more willing
;; to wait at product launch, than later when calling a file.
(require 'ess-r-mode)

(defun br-init-general-icons()
  "Set icons not mode-specific."
  (tool-bar-add-item "splith" 'br-toggle-split-h
		     'br-toggle-split-h
		     :help   "Toggle horizontal split"))

;; In ess-mode add the button to set R workdir based on current buffer file
(define-key ess-mode-map
       [menu-bar ESS sync]
       '("Set directory to this file" . ess-use-this-dir))

(defun br-init-ess-icons()
  "Set ESS icons.  To be run after `br-init-general-icons' to inherit its icons."
  
  ;; Remove S-plus icon
  (setq ess-toolbar-items
 	(quote
 	 ((R "startr" "Start R process only")
 	  (ess-eval-line-and-step "rline" "Eval line & step")
 	  (ess-eval-region "rregion" "Eval region")
 	  (ess-eval-function-or-paragraph-and-step "rregion" "Eval function or paragraph and step")
 	  (ess-load-file "rbuffer" "Load file")
 	  (ess-eval-function "rfunction" "Eval function")
 	  (ess-use-this-dir "switchr" "Set directory to this file"))))
  
  (ess-make-toolbar))


(defun br-toggle-split-h ()
 "Toggle horizontal split."
 (interactive)
 (if (= 1 (count-windows))
     (split-window-below)
   (delete-window))
 (message "Click again to toggle"))

(defun br-init-menico()
  "Call specific and generic icon and menu functions."
  (br-init-general-icons)
  (br-init-ess-icons)
  )

;;; Themes

;; This advice should be removed in the future
(defun .br-remove-light-blue-theme (themes)
  "Remove the obsolete \"light-blue\" theme from the list of available themes.
THEMES is the argument returned by `custom-available-themes',
which this function is intended to adivce."
  (remove 'light-blue themes))

;; See comment above
(advice-add 'custom-available-themes :filter-return #'.br-remove-light-blue-theme)

(declare-function custom-theme-summary "cus-theme")
(defun br-change-theme ()
  "Display a selectable list of Custom themes.
This is a customised `customize-themes'."
  (interactive)
  (require 'cus-theme)
  (switch-to-buffer (get-buffer-create "Choose a theme"))
  (let ((inhibit-read-only t))
    (erase-buffer))
  (custom-theme-choose-mode)
  (setq-local custom--listed-themes nil)
  (make-local-variable 'custom-theme-allow-multiple-selections)
  (setq custom-theme-allow-multiple-selections nil)

  (widget-insert
   (substitute-command-keys
    "Type RET or click to enable/disable listed themes."))
  (widget-insert (substitute-command-keys "'.\n\n"))

  (widget-create 'push-button
		 :tag " Save Theme Settings "
		 :help-echo "Save the selected themes for future sessions."
                 :action #'custom-theme-save)
  (widget-insert ?\n)

  (widget-insert "\n\nAvailable Custom Themes:\n")
  (let ((help-echo "mouse-2: Enable this theme for this session")
	widget)
    (dolist (theme (custom-available-themes))
      ;; Don't list obsolete themes.
      (unless (get theme 'byte-obsolete-info)
        (setq widget (widget-create 'checkbox
				    :value (custom-theme-enabled-p theme)
				    :theme-name theme
				    :help-echo help-echo
                                    :action #'custom-theme-checkbox-toggle))
        (push (cons theme widget) custom--listed-themes)
        (widget-create-child-and-convert widget 'push-button
				         :button-face-get 'ignore
				         :mouse-face-get 'ignore
				         :value (format "%s" theme)
                                         :action #'widget-parent-action
				         :help-echo help-echo)
        (widget-insert " -- "
		       (propertize (custom-theme-summary theme)
				   'face 'shadow)
		       ?\n))))
  (goto-char (point-min))
  (widget-setup))
  
;;;end Themes


;;; Quick menu
(define-key-after global-map [menu-bar quick]
  (cons "Quick" (make-sparse-keymap "Quick")))

;; 5 Themes
(define-key global-map [menu-bar quick change-theme]
  '(menu-item "Themes" br-change-theme
    :help "Change current BloomR theme"))

;; 4 Spelling
(define-key global-map [menu-bar quick spelling]
  (cons "Spelling" (make-sparse-keymap "Spelling")))

;; sub-2 Spell Region
(define-key global-map [menu-bar quick spelling spell-region]
  '(menu-item "Spell Region" ispell-region
    :help "Check spelling of selected region"))

;; sub-1 Spell Buffer
(define-key global-map [menu-bar quick spelling spell-buffer]
  '(menu-item "Spell Buffer" ispell-buffer
    :help "Check spelling of entire buffer"))

;; 3 Increase font
(define-key global-map [menu-bar quick inc-font]
  '(menu-item "Increase Font Size" text-scale-increase
    :help "Increase current buffer's font size"))

;; 2 Decrease font
(define-key global-map [menu-bar quick dec-font]
  '(menu-item "Decrease Font Size" text-scale-decrease
    :help "Decrease current buffer's font size"))

;; 1 Recents
(define-key global-map [menu-bar quick recents]
  '(menu-item "Recents" recentf-open-files
    :help "Open recent file buffer"))


;;-end Quick menu

(br-init-menico)
(provide 'br-menico)
;;; br-menico.el ends here
