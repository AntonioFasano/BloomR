;;; General and mode specific key bindings

(defun br-init-keys()
  "General and mode specific key bindings."
  
   
  ;; ESS binding (R code)
  (add-hook 'ess-mode-hook
	    '(lambda()
	       (local-set-key "_" #'ess-insert-assign)
	       (local-set-key (kbd "C-l")   'ess-eval-line-and-step)
	       (local-set-key (kbd "C-S-l") 'polymode-eval-region-or-chunk)
	       (local-set-key (kbd "C-d") 'comment-region)
	       (local-set-key (kbd "C-S-d") 'uncomment-region)
	       (local-set-key (kbd "<tab>") 'ess-indent-or-complete)))


  ;; ESS-inf binding (R console)
  (add-hook 'inferior-ess-mode-hook
	    '(lambda()
	       (local-set-key "_" #'ess-insert-assign)
	       (local-set-key (kbd "C-a") 'mark-whole-buffer)))

  
  ;; Buffers
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key [(control tab)] 'other-window)
  (setq tab-always-indent 'complete) ; ESS uses a different var!
  (global-set-key  (kbd "ESC ESC") 'keyboard-escape-quit)
  (global-set-key (kbd "C-w")  '(lambda ()
				  "Kill current buffer."
				  (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)


  ;; Search keys
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "<f3>")  'isearch-forward)
  (global-set-key (kbd "S-<f3>")  'isearch-backward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)

  ;; Bookmarks
  (global-set-key (kbd "<f8>") 'bm-next) ;Next bookmark
  (global-set-key (kbd "S-<f8>") 'bm-toggle) ; Toggle bookmark

  )


(br-init-keys)
(provide 'br-keys)
