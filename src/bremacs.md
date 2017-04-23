
The name of the game
--------------------

BRemacs is a fully portable environment to interact with [R](https://www.r-project.org) under Windows, much more powerful then the graphical interface that comes with R itself. 

BRemacs is very similar in concept to [RStudio](https://www.rstudio.com/products/rstudio).  It targets power users who prefer to type rather than click. Besides, since BRemacs is based on the  [Emacs](https://www.gnu.org/software/emacs/) extensible editor, it might also appeal those who want to take advantage of Emacs thousands of packages or program it themselves. 

BRemacs is part of BloomR and so preconfigured to interact with [Bloomberg Professional Service](https://www.bloomberg.com/professional/) [API](https://www.bloomberg.com/professional/support/api-library/).

Motivation
----------

[Emacs Speaks Statistics](http://ess.r-project.org) (ESS) is  an Emacs add-on allowing Emacs to interact with statistical analysis programs. 


In Linux, the distribution package manager can automatically download and install from a central repository a working Emacs environment to interact with R (or other languages). 

In Windows you have to install and configure all necessary components manually and repeat this process when new versions are available.

BRemacs is a prepackaged solution where all the components are already bundled together and ready to use. 

Similarly, if you use [Bloomberg Professional Service](https://www.bloomberg.com/professional/), you might want to run R models in real time against Bloomberg data (see [Bloomberg API](https://www.bloomberg.com/professional/support/api-library/)), but installing and configuring R libraries to do so might be something you want to avoid. BRemacs is part of BloomR (hence the name), therefore automatically detects a Bloomberg session running and interacts with it.



Similar projects
----------------

A very similar and valid project is 
[Emacs Modified for Windows](https://vigou3.github.io/emacs-modified-windows/)

The main difference is that BRemacs is portable. You can run it from your USB flash drive and you can move its folder from one computer to another.   
There are no setup procedures, the installer simply extract its compressed files in the folder you choose.

A second strictly related feature is that BRemacs is part of BloomR, therefore it includes all the libraries to interact with [Bloomberg Professional Service](https://www.bloomberg.com/professional/). 

Portability is very important in this instance. In fact, particularly for academic contexts, physical Bloomberg terminals might be shared among several students/instructors. Therefore, moving your environment via a USB drive makes you immediately up and running: you do not have to reconfigure everything each time you change seat; also, as there is nothing to install, you don't need to queue with the IT staff authorised to install software. 

To make the environment totally portable, BloomR/BRemacs comes with more software packages preinstalled. Most notably R itself. 
Too keep the size of the BloomR/BRemacs folder not too large, LaTeX bundling is optional. If you need it, you may ask too add it to the environment from inside BRemacs. 



It bears mentioning that BloomR/BRemacs is designed to be green on your system: it never writes configuration files outside its own folder or inside the registry and never executes any operation which requires administrator privileges to run. This is a prerequisite to make it fully portable. 




BRemacs Internals
----------------

__Notation__  If you come from a Linux world, note that path separators in Windows are backslashes `\` and not  slashes. For Windows users, note that I will use the PowerShell notation to address enviroment variables, that is `${ENVVAR}`; curly braces are omitted if no confusion arises.

Remember that in BRemacs (and Emacs in general), you might obtain the value of a system evnironment variable `$MYVAR` with the key sequence (where `M` is the meta key  <kbd>Alt</kbd> )

    M-:(getenv "MYVAR")

If you have opened an R session in BRemacs you can also use:

    Sys.getenv("MYVAR")


When you click on the the green BRemacs icon,  a number of enviroment variable are set.

First `bremacs.exe` sets the enviroment variable `$BLOOMR` to the main BloomR/BRemacs folder. Initially, this is the folder chosen by the user at setup time when extracting files. The actual value of  `$BLOOMR`, if the position of the folder changes. 

The enviroment variable `$HOME` is temporary  set to `${BLOOMR}\bremacs`.  This is later changed.


Since BloomR includes also a portable Java environment, it  then sets `$JAVA_HOME` to the related runtime folder, that is `${BLOOMR}\main\openjdk\jre`

The current directory is set to `${BLOOMR}\mybloomr`  at startup, which means that in Emacs:

    C-h vdefault-directory

and in an R session:

    getwd()

give the same value as  `${BLOOMR}\mybloomr`. 


`bremacs.exe` then executes following line:


    ${BLOOMR}\main\bremacs\bin\runemacs.exe -q --no-splash

`runemacs.exe` is the standard Emacs runtime for Windows.  
Therefore `-q` prevents to immediately load the Emacs init file. This will be loaded later by the way.

Based on temporary value of `$HOME`, `runemacs.exe` creates the directory `$HOME\emacs.d` if it does not exists already.  
`runemacs.exe` executes the elisp script: 

    ${BLOOMR}\main\bremacs\share\emacs\site-lisp\site-start.el


In normal conditions, that is, when the enviroment variable `$EMACSDBG` is not set to 1, the `site-start.el` executes the elisp script:


    ${BLOOMR}\main\bremacs\share\emacs\site-lisp\bremacs\br-init.el

This is the main BRemacs init script setting configuration parmateters and loading required add-on libraries.  Its  main function `br-init-main` calls ancillary functions which:


- set Emacs path variables;
- set Emacs standard or package specifc variables;
- load external libraries, including a number  of BRemacs specific libraries in located in  `${BLOOMR}\main\bremacs\share\emacs\site-lisp\bremacs`.
- load Emacs init file

`br-init-paths` sets the Emacs variable `user-emacs-directory` to `${BLOOMR}\main\bremacs\emacs.d`
 
`user-emacs-directory` is a standard variable identifying the directory for storing Emacs package configuration files and the automatically installed packages.

It also changes the temporary value of the enviroment variable `$HOME` to `${BLOOMR}\mybloomr` (previously set  by `bremacs.exe` as working directory).  
Both in BRemacs and in R, when a path contains a `~`, it is exapanded to the value  `$HOME`. 

Finally `br-init.el` executes the BRemacs init file. (Like the standard Emacs init file) BRemacs init file is named `init.el` and  can be located in the directory set by `user-emacs-directory`, but it can also in the be in that set by the enviroment variable `$HOME`.  If it is in both places the latter prevails. 



<!-- Local Variables: -->
<!-- mode: md -->
<!-- End: -->

<!--  LocalWords:  BloomR BRemacs RStudio ESS Bloomberg LaTeX
 -->
<!--  LocalWords:  preinstalled preconfigured
 -->
