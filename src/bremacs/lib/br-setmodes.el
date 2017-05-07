
;;; Setup relevant modes and create bremacs-rmd-mode


(require 'ess-site)
(require 'markdown-mode)
(require 'poly-markdown)
(require 'poly-R)

;;; Create bremacs-rmd-mode
;;; =======================

;;(assoc 'poly-markdown+r-mode  minor-mode-alist)

(defclass pm-hbtchunkmode-short (pm-hbtchunkmode)
  ((head-mode
    :initarg :head-mode
    :type symbol
    :initform 'border-mode
    :custom symbol
    :documentation
    "See `head-mode' docs for pm-hbtchunkmode")
   )
   "Like pm-hbtchunkmode, but the `:initform' of the `head-mode' slot is shortned 
from `'poly-head-tail-mode' to `'border-mode")

(define-derived-mode border-mode prog-mode "Rmd"
  "Short version of `poly-head-tail-mode' used by rmdcode.")

(define-derived-mode rmarkdown-mode markdown-mode "Rmd"
  "Rmd nice display of `markdown-mode'.")

(define-derived-mode rmdcode-mode R-mode "Rmd"
  "Rmd nice display of ESS `R-mode'.")

(defcustom pm-host/rmarkdown
  (pm-bchunkmode "rmarkdown" :mode 'rmarkdown-mode)
  "R markdown host chunkmode."
  :group 'hostmodes
  :type 'object)

(defcustom  pm-inner/rmdcode
  (pm-hbtchunkmode-short "rmdcode"
		   :mode 'rmdcode-mode
		   :head-reg "^[ \t]*```[{ \t]*\\w.*$"
		   :tail-reg "^[ \t]*```[ \t]*$"
		   )
  "Rcode typical chunk."
  :group 'innermodes
  :type 'object)


(defcustom pm-poly/bremacs-rmd
  (pm-polymode-one "bremacs-rmd"
                   :hostmode 'pm-host/rmarkdown
                   :innermode 'pm-inner/rmdcode)
  "Rmd mode used in Bremacs."
  :group 'polymodes
  :type 'object)

(declare-function define-polymode "polymode") ;; don't know if needed
(define-polymode bremacs-rmd-mode pm-poly/bremacs-rmd
  ;; default hook sets R help buffers to poly-ess-help+r-mode, but currently this doesn't work
  :after-hook (setq ess-help-mode-hook nil)
  )

;;; end Create bremacs-rmd-mode========


(defvar br-R-exit-command "q('no')\n"
  "R command used to exit followed by a newline.")

(defmacro br-ess-quit ()
  "Delay load of `ess-quit' method for R during inferior-ess hook.
Read R `ess-quit' doc-string and `br-R-exit-command' variable or more."

  (ess-defmethod R ess-quit (&rest args)
    "This is a rewrite of the official ESS function. 
Quit process associated with ESS buffer, using the command set with `br-R-exit-command'.
Possibly (ask to) close R buffer depending on value of `ess-S-quit-kill-buffers-p'.
ARGS are set for compatibility"  
    (interactive)
    (if ess-current-process-name
	(let ((sprocess (ess-get-process ess-current-process-name)))
	  (ess-cleanup)
	  (goto-char (marker-position (process-mark sprocess)))
	  (insert br-R-exit-command) 
	  (process-send-string sprocess br-R-exit-command))
      (message "No R process associated with this buffer.")))  
  )



;;; Set initialite modes
;;; ====================

(defun br-init-change-minor-name (mode new-name)
  "Change the name of the minor mode MODE to NEW-NAME."
  (if (not (eq new-name ""))
    (setq new-name (concat " " new-name)))
  (setcdr (assoc mode minor-mode-alist) (cons new-name nil)))

(defun br-init-modes ()
  ;;  ESS[S] to  R 
  (add-hook 'R-mode-hook (lambda () (setq mode-name "R")))
  
  (br-init-change-minor-name  'bremacs-rmd-mode "")
  (br-init-change-minor-name  'eldoc-mode "")
  (br-init-change-minor-name  'visual-line-mode "") 
  
  ;;Use wrap lines in markdown
  (add-hook 'markdown-mode-hook (lambda () (visual-line-mode 1)))

  ;; Associates Rmd to BloomR poly-markdown+r-mode
  (add-to-list 'auto-mode-alist '("\\.Rmd" . bremacs-rmd-mode)))

;;; end Set initialite modes===


(br-init-modes)
(provide 'br-setmodes)


