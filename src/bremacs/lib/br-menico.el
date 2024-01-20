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
  "Set ESS icons. To be run after `br-init-general-icons' to inherit its icons."
  
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
 "Toggle horizontal split"
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


(br-init-menico)
(provide 'br-menico)
;;; br-menico.el ends here

