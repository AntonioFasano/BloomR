
:: BRemacs Environment Diagnostic DEBUG
:: =====================================

@Echo off
SETLOCAL

:: 1 causes to load br-init-dbg.el insetead of br-init-dbg.el TODO
Set "BREMACSDBG=1"
Echo BloomR/BRemacs debug flag is set:  
Echo BREMACSDBG=%BREMACSDBG%

.\bremacs.cmd
