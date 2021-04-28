;; main\ed\bremacs-dbg.ed.cmd sets BREMACSDBG=1
(if  (not (string= "1" (getenv "BREMACSDBG")))
    (load "br-init")
  (message "BREMACSDBG=1")
  (load "br-init-dbg"))
