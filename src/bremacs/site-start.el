
(if  (not (string= "1" (getenv "EMACSDBG")))
    (load "br-init")
  (message "EMACSDBG=1")
  (load "br-init-dbg"))
