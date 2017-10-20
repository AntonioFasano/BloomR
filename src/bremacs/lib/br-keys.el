
;;; General and mode specific key bindings

(defun br-init-keys()
  "General and mode specific key bindings."
  
  
  
  ;; ESS binding (R code)
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key (kbd "C-l")   'ess-eval-line-and-step)
	       (local-set-key (kbd "C-S-l") 'ess-eval-region )
					; (local-set-key (kbd "C-c C-k") 'ess-eval-chunk)
	       (local-set-key (kbd "C-d") 'comment-region)
	       (local-set-key (kbd "C-S-d") 'uncomment-region)
	       ;;    (local-set-key (kbd "<f2>") 'knit-or-latex)
	       ;;    (local-set-key (kbd "<f5>") 'smart-eval-chunk)
	       ;;    (local-set-key (kbd "<f11>") (lambda () (interactive) (debugr t)))
	       ;;    (local-set-key (kbd "S-<f11>") (lambda () (interactive) (debugr nil)))
	       ;;    (local-set-key (kbd "C-b") 'toggle-break)
	       (local-set-key (kbd "<tab>") 'ess-indent-or-complete)))


  ;; ESS-inf binding (R console)
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key (kbd "C-a") 'mark-whole-buffer)
	       (macroexpand '(br-ess-quit))
	       ))
  

  
  ;; Buffers
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key [(control tab)] 'other-window)
  (setq tab-always-indent 'complete) ; ESS uses a different var!
  (global-set-key  (kbd "ESC ESC") 'keyboard-escape-quit)
  (global-set-key (kbd "C-w")  '(lambda ()
				  "Kill current buffer."
				  (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "C-s") 'save-buffer)

  ;; Search keys
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "<f3>")  'isearch-forward)
  (global-set-key (kbd "S-<f3>")  'isearch-backward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)

  )


(br-init-keys)
(provide 'br-keys)
