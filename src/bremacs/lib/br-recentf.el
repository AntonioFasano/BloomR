
;;; See br-init-recentf

(defun br-init-recentf()
  "Show recentf buffer with open/create when empty."
  (setq recentf-save-file  (locate-user-emacs-file ".recentf"))
  (recentf-mode)  ; activate recent menu
  (br-init-recentf-dlg)
)


(defun  br-init-recentf-dlg ()
  "Show recent, if any, or propose to open/create file"
  (if recentf-list  (recentf-open-files)
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
  (let* ((last-nonmenu-event nil)
	 (file (find-file-read-args "Find file" nil)))
    (when (cadr file)
	(if widget (kill-buffer (current-buffer)))
	(find-file (car file)))))
;;(br-init-create-new-file-gui)


(defun br-init-open-existing-file-gui  (&optional widget &rest _ignore)
  (let* ((last-nonmenu-event nil)
	 (file (find-file-read-args "Find file" t)))
    (when (cadr file)
	(if widget (kill-buffer (current-buffer)))
	(find-file (car file)))))
;;(br-init-open-existing-file-gui)


(br-init-recentf)
(provide 'br-recentf)


