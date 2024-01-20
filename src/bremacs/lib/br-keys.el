;;; br-keys.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;;; General and mode specific key bindings

;;; Code:


(defun br-init-keys()
  "General and mode specific key bindings."
  
   
  ;; ESS binding (R code)
  (add-hook 'ess-mode-hook
	    #'(lambda()
	       (local-set-key "_" 'ess-insert-assign)
	       (local-set-key (kbd "C-l")   'ess-eval-line-and-step)
	       (local-set-key (kbd "C-S-l") 'polymode-eval-region-or-chunk)
	       (local-set-key (kbd "C-d") 'comment-region)
	       (local-set-key (kbd "C-S-d") 'uncomment-region)
	       (local-set-key (kbd "<tab>") 'ess-indent-or-complete)))


  ;; ESS-inf binding (R console)
  (declare-function ess-insert-assign "ess-s-lang.el")
  (add-hook 'inferior-ess-mode-hook
	    #'(lambda()
	       (local-set-key "_" #'ess-insert-assign)
	       (local-set-key (kbd "C-a") 'mark-whole-buffer)))

  
  ;; Buffers
  (global-set-key (kbd "C-<next>") 'next-buffer)
  (global-set-key [(control tab)] 'other-window)
  (setq tab-always-indent 'complete) ; ESS uses a different var!
  (global-set-key  (kbd "ESC ESC") 'keyboard-escape-quit)
  (global-set-key (kbd "C-w")  #'(lambda ()
				  "Kill current buffer."
				  (interactive) (kill-buffer (current-buffer))))
  (global-set-key (kbd "C-s") 'save-buffer)
  (global-set-key (kbd "C-a") 'mark-whole-buffer)


  ;; Search and replace keys
  (global-set-key (kbd "C-f") 'isearch-forward)
  (global-set-key (kbd "<f3>")  'isearch-forward)
  (global-set-key (kbd "S-<f3>")  'isearch-backward)
  (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
  (global-set-key (kbd "C-r") 'query-replace)

  ;; Mouse context menu
  (defun open-context-menu (event)
  "Open context menu at EVENT position. EVENT should be a mouse-click event."
  (interactive "e")
  (let ((pos (cadr (event-start event))))
    (goto-char pos)
    (context-menu-open)))
  (global-set-key [mouse-3] 'open-context-menu)
  
  ;; Bookmarks
  (global-set-key (kbd "<f8>") 'bm-next) ;Next bookmark
  (global-set-key (kbd "S-<f8>") 'bm-toggle) ; Toggle bookmark

  ;; Faster typing
  (defun join-next ()
  "Join current with next line and trim spaces between joined lines."
  (interactive)
  (delete-indentation 1))
  (global-set-key (kbd "S-<backspace>") 'join-next) ; Join with next line and trim spaces

  (defun backward-delete-word ()
  "Delete characters backward until encountering the beginning of a word.
   It does not affect the kill ring."
  (interactive)
  (let ((beg (point)))
    (backward-word)
    (delete-region beg (point))))
  (global-set-key (kbd "C-<backspace>") 'backward-delete-word); Delete words backward


  (defun forward-delete-word ()
  "Delete characters forward until encountering the end of a word.
   It does not affect the kill ring."
  (interactive)
  (let ((beg (point)))
    (forward-word)
    (delete-region beg (point))))
  (global-set-key (kbd "C-<delete>") 'forward-delete-word); Delete words forward
  
  )


(br-init-keys)
(provide 'br-keys)
;;; br-keys.el ends here

