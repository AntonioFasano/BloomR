;;; br-init-dbg.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; An alternative boot for the breamcs package intended for instrument
;; the function in the boostrapping sequance.
;;
;; To instrument functions you normally need to comment out the main
;; .el file function.  For example to instrument a function in
;; "br-init.el", you: 1) comment `br-init-main', 2) visit "br-init.el",
;; 3) instrument the desired function, 4) evaluate "br-init.el"
;; buffer.

;;; Code:
;; Now in early init
;; (modify-frame-parameters  nil (quote ((fullscreen . maximized))))
;; (set-face-attribute 'default  nil :height 146)

;; Just the bare essential
(show-paren-mode t)
(cua-mode t)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-<next>") 'next-buffer)
(global-set-key [(control tab)] 'other-window)
(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "<f3>")  'isearch-forward)
(global-set-key (kbd "S-<f3>")  'isearch-backward)
(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
(global-set-key (kbd "C-w")  #'(lambda ()
			  "Kill current buffer."
			  (interactive) (kill-buffer (current-buffer))))


;; Suggest the user where to go from here
(insert "\nGo on evalling the sexp below, perhaps instrumenting critical functions:\n\n")
(let ((std-init
       (expand-file-name (concat invocation-directory "../share/emacs/site-lisp/bremacs/br-init.el"))))
  (insert (format "(load-file\n \"%s\")\n\n" std-init)))


;;; br-init-dbg.el ends here
