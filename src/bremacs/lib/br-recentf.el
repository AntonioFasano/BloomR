;;; br-recentf.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; At BRemacs startup `br-init-recentf' shows a buffer with open file
;; links, when the recent list is empty, or the recente list


;;; Code:

(defun br-init-recentf()
  "Show recentf buffer with open/create when empty."
  (setopt recentf-save-file  (locate-user-emacs-file ".recentf"))
  (recentf-mode)  ; activate recent menu
  (br-init-recentf-dlg)
  (switch-to-buffer "*Open Recent*"))

(eval-when-compile (require 'recentf)) ; find and expand recentf-dialog macro
(declare-function recentf-open-files "recentf.el")
(defun  br-init-recentf-dlg ()
  "Show recent, if any, or propose to open/create file."
  (if (bound-and-true-p recentf-list)  (recentf-open-files)
    (recentf-dialog (format "*%s*" recentf-menu-title)
      (widget-insert "There is no recent file to open!\n\n")
      (widget-create
       'push-button
       :notify 'br-init-create-new-file-gui
       "Click here to create a new file or use the menu bar.")
      (widget-insert "\n")
      (widget-create
       'push-button
       :notify 'br-init-open-existing-file-gui
       "Click here to open an existing file or use the menu bar.")))
  (kill-buffer  "*scratch*"))


(defun br-init-create-new-file-gui  (&optional widget &rest _ignore)
  "Show the Windows new-file dialog box.
If WIDGET is non-nil kills the current buffer."
  (let* ((last-nonmenu-event nil)
	 (file (find-file-read-args "Find file" nil)))
    (when (cadr file)
      (if widget (kill-buffer (current-buffer)))
      (find-file (car file)))))
;;(br-init-create-new-file-gui)


(defun br-init-open-existing-file-gui  (&optional widget &rest _ignore)
  "Show the Windows open-file dialog box.
If WIDGET is non-nil kills the current buffer."
  (let* ((last-nonmenu-event nil)
	 (file (find-file-read-args "Find file" t)))
    (when (cadr file)
      (if widget (kill-buffer (current-buffer)))
      (find-file (car file)))))
;;(br-init-open-existing-file-gui)


(br-init-recentf)
(provide 'br-recentf)

;;; br-recentf.el ends here

