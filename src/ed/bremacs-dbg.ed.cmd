
:: HOME is set for emacs.d, but changed immediately in init
   set "HOME=%~dp0"

:: 1 causes to load br-init-dbg.el insetead of br-init-dbg.el
   set "EMACSDBG=1"

:: Breamcs site-start.el calls br-init(-dbg)
   bin\runemacs.exe  -q --no-splash %*

:: Call directly br-init.el, removing site-start.el
:: bin\runemacs.exe  -q --load %~dp0share\emacs\site-lisp\br-init.el %*



