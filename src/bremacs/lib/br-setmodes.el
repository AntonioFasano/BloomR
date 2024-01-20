;;; br-setmodes.el --- BRemacs init library -*- lexical-binding: t -*-

;; Copyright (C) Antonio Fasano
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GPL 2+ license.

;;; Commentary:
;; Setup relevant modes and create bremacs-rmd-mode

;;; Code:



;;;; bremacs-rmd-mode is temporary disabled
;;;; Create bremacs-rmd-mode
;;;; =======================
;(defclass pm-hbtchunkmode-short (pm-hbtchunkmode)
;  ((head-mode
;    :initarg :head-mode
;    :type symbol
;    :initform 'border-mode
;    :custom symbol
;    :documentation
;    "See `head-mode' docs for pm-hbtchunkmode")
;   )
;   "Like pm-hbtchunkmode, but the `:initform' of the `head-mode' slot is shortned 
;from `'poly-head-tail-mode' to `'border-mode")
; 
;(define-derived-mode border-mode prog-mode "Rmd"
;  "Short version of `poly-head-tail-mode' used by rmdcode.")
; 
;(define-derived-mode rmarkdown-mode markdown-mode "Rmd"
;  "Rmd nice display of `markdown-mode'.")
; 
;(define-derived-mode rmdcode-mode R-mode "Rmd"
;  "Rmd nice display of ESS `R-mode'.")
; 
;(defcustom pm-host/rmarkdown
;  (pm-bchunkmode "rmarkdown" :mode 'rmarkdown-mode)
;  "R markdown host chunkmode."
;  :group 'hostmodes
;  :type 'object)
; 
;(defcustom  pm-inner/rmdcode
;  (pm-hbtchunkmode-short "rmdcode"
; 		   :mode 'rmdcode-mode
; 		   :head-reg "^[ \t]*```[{ \t]*\\w.*$"
; 		   :tail-reg "^[ \t]*```[ \t]*$"
; 		   )
;  "Rcode typical chunk."
;  :group 'innermodes
;  :type 'object)
; 
; 
;(defcustom pm-poly/bremacs-rmd
;  (pm-polymode-one "bremacs-rmd"
;                   :hostmode 'pm-host/rmarkdown
;                   :innermode 'pm-inner/rmdcode)
;  "Rmd mode used in Bremacs."
;  :group 'polymodes
;  :type 'object)
; 
;(declare-function define-polymode "polymode") ;; don't know if needed
;(define-polymode bremacs-rmd-mode pm-poly/bremacs-rmd
;  ;; default hook sets R help buffers to poly-ess-help+r-mode, but currently this doesn't work
;  :after-hook (setq ess-help-mode-hook nil)
;  )
; 
;;;; end Create bremacs-rmd-mode========


(defvar br-R-save-on-quit "no"
  "Behaviour of `ess-quit', bound to C-c C-q, <menu-bar> <iESS> <Quit>.
Possible values are those accepted in R quit(save = ...),
that is \"no\", \"yes\", \"ask\" or \"default\".
See R function documentation for more information.

Note that the original R base quit() and q() is also overridden
by the attached BloomR environments in \"bloomr.init.R\" and the
default \"save\" value is set to \"no\". Pass this argument
explicitly if you want to an alternative behaviour.")

(defun br-ess-quit-advice (ess-quit-orig &optional arg)
  (setq arg br-R-save-on-quit)
  (funcall ess-quit-orig arg))

;;; Customise R/md modes
;;; ====================

(defun br-init-change-minor-name (mode new-name)
  "Change the name of the minor mode MODE to NEW-NAME."
  (if (not (eq new-name ""))
    (setq new-name (concat " " new-name)))
  (setcdr (assoc mode minor-mode-alist) (cons new-name nil)))

(defun br-init-modes ()
  ;;  ESS[S] to  R 
  (add-hook 'ess-r-mode-hook (lambda ()
			       (setq mode-name "R") ))
  
 ;; (br-init-change-minor-name  'bremacs-rmd-mode "")
  (br-init-change-minor-name  'eldoc-mode "")
  (br-init-change-minor-name  'visual-line-mode "") 
  
  ;;Use wrap lines in markdown
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))

  ;; `ess-quit' defaults to br-R-save-on-quit
  (advice-add 'ess-quit :around #'br-ess-quit-advice)
  )

;;; end Set initialite modes===


(br-init-modes)
(provide 'br-setmodes)

;;; br-setmodes.el ends here
