

(modify-frame-parameters  nil (quote ((fullscreen . maximized))))
(set-face-attribute 'default  nil :height 146) 
(show-paren-mode t)

(cua-mode t)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key [(control tab)] 'other-window)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "<f3>")  'isearch-forward)
(global-set-key (kbd "S-<f3>")  'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
(global-set-key (kbd "C-w")  '(lambda ()
			  "Kill current buffer."
			  (interactive) (kill-buffer (current-buffer))))

(defun display-startup-echo-area-message ()
  (message "Emacs started with \"br-init-dbg.el\""))
