###  BloomR source

## Release log
##  New edition system. MiKTeX standalone installer. Minimal Perl for rmarkdown pdfcrop. UNC support. Removed refs to old Java API. New masking quit(). Byte compilation with autoloads. Updated utility URLs.

## To commit

#git update-index --assume-unchanged README.html README.md
#git commit -am "Commit message" 
#git update-index --no-assume-unchanged README.html README.md


## build: makeStudio (with miktex, mpm, initexmf)
## br-init.el: ~tab-always-indent, ~inferior-ess-r-program, ~ess-r-post-run-hook, -require 'ess-site and 'markdown-mode, +declare br-init-simple-menu, +br-init-autoloads.
## br-setmodes.el: -Disabled bremacs-rmd-mode, ~br-R-save-on-quit ~cl-defmethod ess-quit--override
## br-keys.el: -br-ess-quit, ~polymode-eval-region-or-chunk, +smart_assign
## bloomr.init.R: +q/quit
## bloomr.rmd: +.br.addpaths, +perl, ~br.rmd2both, ~br.rmd2pdf, +buildenv
## xlx.R: plyr:::rbind.fill without loading package to avoid conflicts
## ed: Cmds work from prompt, +BREMACSDBG, no Java, ~site-start.el

##  TODO
##  Make an external file for byte-compile and autoloads and solve the async problem see below. 
##  Test it is: (defun name<space>()  with  \(defun +[[:alpha:]]+\)( → \1 (
##  Custom polimode (bremacs-rmd-mode), temporary disabled, to be restored in br-setmode.el
##  Create a download folder var in global env
##  Byte compile is async. "0" can be removed in tha case stadard emacs elisp files
##  Automatically identify knit/rmarkdown needed LaTeX packages. See scatch latex.autopackages
##  Perhaps use switch+get.edition() rather than if-else + is.core/lab/studio, in downloads/expand/bloomrTree
##  NSIS does not delete an existing dir, but silently overwrites it
##  Repurpose src\bloomr.beta.*
##  Repurpose tests and test data 
##  Change app path from main to programs?
##  RCurl find and remove references everywhere 

##
##  Usage:
##  Source this file and run:
##  makeBloomR("path/to/workDir")
##  You will get the BloomR green installer in workDir
##
##  Requirements:
##  XML and curl packages. If missing, I try to download and install them.
##  R should be able to connect to the Internet.
##  .Platform$OS.type == "windows"
##
##  Credits:
##  R from https://www.r-project.org/
##  MiKTeX from  https://miktex.org
##  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
##  peazip from http://sourceforge.net/projects/peazip
##  ahkscript from http://ahkscript.org
##  Nsis from nsis.sourceforge.net
##  innoextract from http://constexpr.org/innoextract
##  Icon set Simplicio CC 3.0 by Neurovit: http://neurovit.deviantart.com
##   retrieved at https://www.iconfinder.com/icons/48750/direction_right_icon
##


### Globals
G <- new.env()
G$lastshell <- NULL # Output of last shell invocation
G$tempmap   <- NULL # Temporary Windows net drive, used by commands not supporting a detected UNC path

### Contribs

## Eikon
G$eikonurl <- "https://github.com/ahmedmohamedali/eikonapir/archive/master.zip"
G$eikonzip <- 'eikon'

## Ahkscript
## G$ahkurl <- "http://ahkscript.org/download/ahk2exe.zip" # removed
G$ahkurl <- "https://autohotkey.com/download/ahk.zip"
G$ahkzip <- "ahk"

## Github
G$github <- "https://raw.githubusercontent.com/AntonioFasano/BloomR/master"
G$github.local <- "" # Auto-set by makeBloomR() if gitsim=T, relative to the build workdir 

## Packages to download. Case sensitive
## To learn about deps use:
## x <- available.packages(); x["ggplot2","Imports"]; x["ggplot2","Depends"]

pks <- "knitr Rblpapi xts XML httr rmarkdown RCurl" # RCurl to be removed
pks <- paste(pks, "plyr pbapply") # for read read.xlx

## All packs deps
G$packlist <- pks
rm(pks)

## Innoextract
G$innourl <- "http://constexpr.org/innoextract/files"
G$innozip <- 'innoextract'

## R
G$rurl <- "https://cloud.r-project.org/bin/windows/base/"
G$rzip <- 'rmain'

## Emacs
G$emacsurl <- "http://ftp.gnu.org/gnu/emacs/windows/"
G$emacszip <- "emacs"
G$emacs.type <- "emacs.*?64.zip$" # e.g. emacs-27.2-x86_64.zip

## SF items
G$pzip <- "peazip"
G$rport <- "rportable" # not used
G$nsisurl <- 'portableapps'
G$nsiszip <- 'nsis'
# G$emacs <- 'emacsbinw64'
# G$emacs.type <- 'Og' # e.g.: emacs-w64-25.1-Og.7z

## ESS, Polymode, Poly-markdown, Poly-noweb  BM
#G$essurl <- 'http://ess.r-project.org/downloads/ess/ess-17.11.zip'
                                        # consider now melpa
G$essurl <- "https://melpa.org/packages/ess-20210414.2354.tar"
#G$essurl <- 'https://github.com/emacs-ess/ESS/archive/refs/tags/ESSRv1.7.zip'
G$esszip <- 'ess'
#G$polyurl <- 'https://github.com/vspinu/polymode/archive/master.zip'
G$polyurl <- 'https://github.com/polymode/polymode/archive/refs/tags/v0.2.2.zip'
G$polyzip <- 'polymode'
G$markurl <- 'https://github.com/jrblevin/markdown-mode/archive/master.zip'
G$markzip <- 'markdown'
G$polymarkurl <- 'https://github.com/polymode/poly-markdown/archive/refs/tags/v0.2.2.zip'
G$polymarkzip <- 'poly-markdown'
G$noweburl <- 'https://github.com/polymode/poly-noweb/archive/refs/tags/v0.2.2.zip'
G$nowebzip <- 'poly-noweb'
G$polyrurl <- 'https://github.com/polymode/poly-R/archive/refs/tags/v0.2.2.zip'
G$polyrzip <- 'poly-R'
G$bmurl <- 'https://github.com/joodland/bm/archive/master.zip'
G$bmzip <- 'bmmode'

## LaTeX & Pandoc for Studio
#G$mikurl <- "http://mirrors.ctan.org/systems/windows/miktex/setup/windows-x86/basic-miktex.exe"
G$mikurl <- "http://ctan.mirror.garr.it/mirrors/ctan/systems/win32/miktex/setup/windows-x64/"
G$mikzip <- "mikzip"
#G$mikinst <- "mikport.exe"
G$panurl <- "https://github.com/jgm/pandoc/releases"
#G$paninst <- "pandoc.msi" #to remove this
G$panzip <- "pandoc"
G$mikpaks <- c('fancyvrb', 'microtype', 'mptopdf', 'upquote', 'url',
               'parskip', 'framed', 'titling', 'booktabs', 'etoolbox')
G$perlzip <- "perl" # used by rmarkdown pdfcrop 
G$perlurl <- "https://strawberryperl.com/download/5.32.1.1/strawberry-perl-5.32.1.1-64bit-portable.zip"

## Local paths
G$work <- "" # This is the build workdir, not to be confused wtih R getwd()
G$appname <- "main" # BloomR application folder name. Used by app.pt() 
G$branch <- NULL # Branch dir: whether "brCore" or a common "brEmacs" dir for non-Core editions

## Arguments
G$bremacs <- NULL
G$studio <- NULL
G$ndown <- 0

G$what <- NULL # What edition? If what !'core', then is.bremacs() TRUE
G$builds <- NULL # Char vector of remaining builds to go 


## Dev style 
## Functions returning/accepting paths normally use paths relative to G$work.
## The latter ones will use a workhorse function with work.pt() if they need to access file system.

makeBloomR <- function( # Build BloomR
                    work,         # work dir path, absolute or relative to cur path
                    tight=FALSE,  # reuse downloaded material in the build workdir
                    ndown=2,      # num of download attempts
                    what='all',   # what edition: all/core/lab/studio
                                  # 'all' requires deb==1:6. Else only core is built
                    ## For debug/test:
                    bundle='exe', # exe/zip/all/none make the related installer for distribution
                    ask=TRUE,     # asks if to overwrite the existent build workdir and installer
                    deb=1:6,      # defaults to 1:6 to execute all steps build steps, modify to debug.
                    gitsim=FALSE, # local path (abs. or relative)to simulate github downloads.
                    reset=TRUE    # FALSE to allow multi-builds calls and keep globals
){

    ## Set work dir
    if(!nzchar(work)) stop("Please, specify a work directory as first arg!")
    if(grepl(":$", work))
        stop("In ",  work, "\n ending with a colon (:) is ambiguous and not allowed as a working directory.",
             "\nFor a USB drive root append a slash to it.")
    G$work <- work 

   
    ## Set git dir
    G$github.local <- ""
    if(gitsim!=FALSE && nzchar(gitsim))
        if (file.info(gitsim)$isdir)
            G$github.local <- gitsim else {
                stop(gitsim, "is not an existing dir")}

    ## Windows?
    if(.Platform$OS.type != "windows")
        stop("Sorry, Bloomberg Terminal only exists for Windows and so BloomR.")
    
    ## Check for required package
    if(!loadLib("curl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Parse Arguments
    editions <- c('all', 'core', 'lab', 'studio')
    if(! what %in% editions) messagev("'what' argument is not in", sQuote(editions))
    G$what <- what

    ## Don't touch the build history, if reset =FALSE 
    if(reset) {
        G$builds <- what
        if(what == 'all') G$builds <- editions[-1]
    }

    ## We use a single branch directory for Lab and Studio. The former needs be bundled before adding Studio files
    G$branch <- ifelse(is.bremacs(), "brEmacs", "brCore") 
    
    #G$bremacs <- what=="bremacs" || what=="studio"
    #G$branch <- ifelse(G$bremacs, "brEmacs", "brCore") # distro directory
    #G$studio <- (sndpass && what=="studio")

    ## Parse residual arguments
    G$ndown <- ndown

    ## Step 1
    if(1 %in% deb) existMake('', overwrite=!tight, ask, paste("working dir:\n", G$work))
    
    ## Step 2
    if(2 %in% deb) downloads(tight)

    ## Step 3
    if(3 %in% deb) expand()

    ## Step 4
    if(4 %in% deb) bloomrTree()
        
    ## Step 5
    if(5 %in% deb) initScripts()

    ## Step 6
    if(6 %in% deb) makeBundle(bundle, ask)

    ## Make additional builds. If  what == 'all', during the Studio build several steps are skipped.  See was.lab()
    G$builds <- G$builds[-1] # needs makeBloomR(reset=F)
    if(length(G$builds)) makeBloomR(
                             work=work, tight=TRUE,  
                             ndown=ndown, bundle=bundle, ask=ask,     
                             what=what,
                             reset =FALSE, # this will keep G$builds status 
                             deb=deb, gitsim=gitsim)
        
    #if(what=='all' && deb==1:6) {
    #    makeBloomR(work=work, tight=TRUE,  
    #               ndown=ndown, bundle=bundle, ask=ask,     
    #               what="bremacs",
    #               deb=deb, gitsim=gitsim)
    #}
    # 
    ### To save space Studio recycles BRemacs folder
    #if((what=='all' || what=='studio') && !sndpass) {
    #    makeBloomR(work=work, tight=TRUE,  
    #               ndown=ndown, bundle=bundle, ask=ask,     
    #               what="studio", sndpass=TRUE,
    #               deb=deb, gitsim=gitsim)
    #}
    
}

### Editions' predicates

is.multi    <- function() G$what == 'all' 
get.edition <- function() G$builds[1] 
is.core     <- function() get.edition() == 'core' 
is.lab      <- function() get.edition() == 'lab' 
is.studio   <- function() get.edition() == 'studio' 
is.bremacs  <- function() is.lab() || is.studio() 
was.core    <- function() is.multi() && is.bremacs() 
was.lab     <- function() is.multi() && is.studio() 

###== Main steps ==

### Load CRAN packages 
loadLib <- function(lib){
    if (!lib %in% rownames(installed.packages())){
        repo <- getOption("repos")[["CRAN"]]
        if(repo=="@CRAN@") repo <- "http://cran.r-project.org"
        install.packages(lib, repos=repo)
    }
    if(!eval(parse(text=paste('require(', lib,')')))){
        message("\nUnable to load", lib, "library")
        return (FALSE)
    }
    return (TRUE)
}

### Get components
downloads <- function(tight){

    overwrite <- !tight

    if(!was.core())                downloads.core(overwrite)
    if(is.bremacs() && !was.lab()) downloads.lab(overwrite)
    if(is.studio())                downloads.studio(overwrite)
}


downloads.core <- function(overwrite){
    
    ## PeaZip
    cback <- function(){
        url <- sfFirstbyProject(G$pzip, '[[:digit:]]') #get release dir 
        url <- sfFirstbyUrl(url, "portable[^\"]*?windows")
        sfDirLink(url)
    }
    download.nice(cback, G$pzip, overwrite,
                  "Peazip files")

    ## innoextract
    cback <- function(){
        dir <- web.greplink("innoextract-", pos=-1, G$innourl, abs=TRUE)
        web.greplink("\\.zip$", pos=0, dir, abs=TRUE)
    }
    download.nice(cback, G$innozip, overwrite,
                  "Innoextract")

    ## R
    from <- web.greplink("R.+win\\.exe", pos=0, G$rurl, abs=TRUE)
    download.nice(from, G$rzip, overwrite,
                  "main R files")

    ## NSIS
    cback <- function(){
        url <- sfFirstbyProject(G$nsisurl, G$nsiszip)
        url <- sfFirstbyUrl(url, 'additional')
        url <- sfFirstbyUrl(url, '[[:digit:]]')
        sfDirLink(url)
    }
    download.nice(cback, G$nsiszip, overwrite,
                  "NSIS")
    

    ## CRAN packages
    existMake("@packs", overwrite=overwrite, ask=FALSE, "packages dir:")# @ to distinguish from unzipped dir
    packs <- getDeps.format(G$packlist)    
    for(pack in unique(packs)) # Loop over packs and download them 
        download.nice(cran.geturl(pack), makePath("@packs", pack), overwrite,
                  pack)
        
    ## Eikon
    download.nice(G$eikonurl, G$eikonzip, overwrite,
                  "Eikon files")
    
    ## ahkscript
    download.nice(G$ahkurl, G$ahkzip, overwrite,
                   "ahkscript")
}


downloads.lab <- function(overwrite){   
### BloomR Lab downloads
    
    cback <- function(){
        dir <- web.greplink("emacs-../", pos=-1, G$emacsurl, abs=TRUE)
        zip <- web.greplink("emacs.*?64.zip$", pos=-1, dir)
        p0(G$emacsurl, rurl, zip)            
    }
    download.nice(cback, G$emacszip, overwrite,
                  "Emacs files")
    
    ## ESS
    download.nice(G$essurl, G$esszip, overwrite,
                  "ESS files")
    
    ## Markdown mode
    download.nice(G$markurl, G$markzip, overwrite,
                  "Markdown files")
    
    ## Polymode
    download.nice(G$polyurl, G$polyzip, overwrite,
                  "Polymode files")

    ## Poly-markdown mode
    download.nice(G$polymarkurl, G$polymarkzip, overwrite,
                  "Polymode for markdown")

    ## Poly-noweb mode
    download.nice(G$noweburl, G$nowebzip, overwrite,
                  "Polymode for noweb")

    ## Poly-r mode
    download.nice(G$polyrurl, G$polyrzip, overwrite,
                  "Polymode for R")
    
    ## Bookmark (bm) mode
    download.nice(G$bmurl, G$bmzip, overwrite,
                  "BM mode files")
    
}

downloads.studio <- function(overwrite){
### BloomR Studio downloads
        
    ## MiKTeX
    from <- web.greplink("*.zip$", pos=-1, G$mikurl, abs=TRUE)
    download.nice(from, G$mikzip, overwrite, "MiKTeX installer")

    miktex.exe <- p0(G$mikzip,'.d/miktexsetup_standalone.exe')
    if(!file.exists(work.pt(miktex.exe))) {
        uzip(G$mikzip, p0(G$mikzip,'.d'), 
             "Preparing MiKTeX installer")
        
        message("Downloading MiKTeX packages")
        cmd <- p0(awin.pt(miktex.exe),
                  " --verbose --local-package-repository=", awin.pt(p0(G$mikzip,'.d')), 
                  " --package-set=basic download")
        shell.ps(cmd, awin.pt("ps.latexdown.txt"))
    }
        
    ## Pandoc        
    from <- p0("https://github.com",
               web.greplink("pandoc-[.0-9]+-windows-x86_64.zip$", pos=1, G$panurl)) 
    download.nice(cback, G$panzip, overwrite, "Pandoc")

    ## Strawberry Perl
    download.nice(G$perlurl, G$perlzip, overwrite, "Perl")
    
}


### Expand components
expand <- function(){
    
    if(!was.core())                expand.core()
    if(is.bremacs() && !was.lab()) expand.lab()
    if(is.studio())                expand.studio()

}


expand.core <- function(){
    
    ## Peazip
    uzip(G$pzip, p0(G$pzip,'.d'), 
          "Peazip binaries")
    
    ## innoextract
    uzip(G$innozip, p0(G$innozip,'.d'), 
          "innoextract binaries")
    
    ## R files
    innoextract(G$rzip, p0(G$rzip,'.d'),
                "R files")

    ## NSIS files
    uzip.7z(G$nsiszip, p0(G$nsiszip, '.d'), 
            "NSIS files")
    
    ## Eikon
    uzip(G$eikonzip, p0(G$eikonzip,'.d'), 
          "Eikon binaries")
    
    ## CRAN packages
    message('\nExpanding packages', '...')
    from <- "@packs"
    del.path("lib.d")
    ## Loop and extract packs
    for(pack in  dir(work.pt(from)))
        uzip(makePath('@packs', pack), 'lib.d',
              paste('R package', pack), delTarget=FALSE)    
    
    ## ahkscript
    uzip(G$ahkzip, p0(G$ahkzip,'.d'),
          "ahkscript")

}

expand.lab <- function(){
### Expand BloomR Lab

    uzip(G$emacszip, p0(G$emacszip,'.d'),
         "BRemacs files")

    utar(G$esszip, p0(G$esszip,'.d'),
         "ESS")

    uzip(G$markzip, p0(G$markzip,'.d'),
         "Markdown mode")
    
    uzip(G$polyzip, p0(G$polyzip,'.d'),
         "Polymode")

    uzip(G$polymarkzip, p0(G$polymarkzip,'.d'),
         "Polymode for markdown")

    uzip(G$nowebzip, p0(G$nowebzip,'.d'),
         "Polymode for noweb")

    uzip(G$polyrzip, p0(G$polyrzip,'.d'),
         "Polymode for R")
    
    uzip(G$bmzip, p0(G$bmzip,'.d'),
         "BM mode")
}
 
expand.studio <- function(){

    ## Expand Straberry Perl
    perlsource <- p0(G$perlzip,'.d')
    uzip(G$perlzip, perlsource, 
         "Perl binaries")

    ## Expand Pandoc
    uzip(G$panzip, p0(G$panzip,'.d'), "Pandoc binaries")

}


bloomrTree <- function(){
### Make BloomR directory tree

### Core components are needed by Lab and Studio too, and Lab's by Studio too.
### However, to save space/time Lab and Studio share a common `brEmacs' branch dir,
### i.e., in a multi-build, Studio recycles previous Lab `brEmacs' dir rather than build a new dir from scratch

    if(is.core()) bloomrTree.Core()

    if(is.lab()) {

        bloomrTree.Core()
        bloomrTree.brEmacs()

    }
    
    if(is.studio()) {

        ## For a Studio build, after a Lab build, we recycle the dir and skip else we go    
        if (was.lab()){
            messagev("This is a Studio Edition build, but a previous Lab Edition has been bundled,",
                     "\nso we are recycling its branch folder.")
            bloomrTree.Studio()

        } else {
        
            bloomrTree.Core()
            bloomrTree.brEmacs()
            bloomrTree.Studio()
        
        }

    }
}

bloomrTree.Core <- function(){
### Make BloomR Core tree
### The only difference from the Core and Bremacs edition is the name that we give to the folder tree 
    
    message("\nCreating BloomR tree")
    desc <- if(is.core()) "BloomR Core" else "Common BloomR Lab/Studio"
    existMake(G$branch, TRUE, FALSE, p0(desc, " root dir:"))
    makeDir(app.pt(), "BloomR app dir:")

    ## Id files
    download.git("curver.txt",  app.pt("bloomr.txt")) 

    ## Copy R and make site directory
    from <- p0(G$rzip , '.d/app')
    to <- app.pt("R")
    copy.dir(from, to, "main R files")
    makeDir(app.pt('R/site-library'), "BloomR library:")
    
    ## Install Eikon package
    message("Install Eikon API")
    ## In Windows R CMD invokes cmd.exe, which is incompatible with a UNC build workdir.
    ## So we'll use a local workdir when invoking it and absolute paths  
    exe <- awin.pt(app.pt('R/bin/R.exe'))
    from <-  awin.pt(p0(G$eikonzip,'.d/eikonapir-master'))
    to <-  awin.pt(app.pt('R/library'))
    to <- p0("--library=", to)
    cmd <- c(exe, "--no-site-file --no-environ --no-save --no-restore --quiet CMD INSTALL", from, to)
    shell.cd(cmd, wd="c:") # In Windows cmd.exe is invoked, so we need to set wd to a non-UNC path 
        
    ## Copy libs
    message("\nAdding R libraries")
    lib.from <- 'lib.d'
    lib.to <- app.pt("R/library")
    for(lib in dir(work.pt(lib.from))){
        from <- makePath(lib.from, lib)  
        to <- makePath(lib.to, lib)
        copy.dir(from, to)
    }

    
    ## Download docs
    message("\nDownloading BloomR help resources")
    download.git("README.html",               root.pt("README.html"))
    download.git("LICENSE",                   root.pt("LICENSE.txt"))
    makeDir(root.pt("help"), "BloomR help directory:")
    download.git("src/bloomr.html",           root.pt("help/bloomr.html")) 
    download.git("src/bloomr.pdf",            root.pt("help/bloomr.pdf"))
    download.git("src/xlx/xlx.help.html",     root.pt("help/xlx.help.html"))     
    download.git("src/xlx/xlx.help.pdf",      root.pt("help/xlx.help.pdf"))
    download.git("reports/reporting.pdf",     root.pt("help/reporting.pdf"))     


    ## Environment diagnostic
    message("\nAdding ED tools")
    makeDir(app.pt('ed'), "ED tools:")
    download.git("src/ed/bloomr.ed.cmd",  app.pt("ed/bloomr.ed.cmd")) 
    
}

bloomrTree.brEmacs <- function(){
### Make BRemacs directory tree

    bremacs <- app.pt("bremacs")
    existMake(bremacs, TRUE, FALSE, "BRemacs tree")

    ## Copy Emacs
    message("Copying main BRemacs files")
    from <- p0(G$emacszip, '.d/')
    dirs <- c('bin', 'libexec', 'share/emacs', 'share/icons', 'share/info', 'share/man')
    copy.glob(from, bremacs, dirs)
    
    
    ## Copy ESS
    message("Adding ESS files")
    from <- p0(G$esszip, '.d')
    # git version now Melpa
    #from <- globpaths(from, '/ESS-ESSR*')
    #to <- slisp.pt(G$esszip)
    #makeDir(to)
    #copy.glob(from, to, "etc")
    #files <- c('obsolete', '*.el')
    #copy.glob(makePath(from, 'lisp'), to, files)

    from <- globpaths(from, '/ess-*')
    to <- slisp.pt(G$esszip)
    copy.dir(from, to, "ESS mode")

    ## Copy Markdown mode
    message("Adding Markdown mode files")
    from <- p0(G$markzip, '.d')
    from <- makePath(from, 'markdown-mode-master')
    to <- slisp.pt(G$markzip)
    makeDir(to)
    copy.glob(from, to, "*.el")

    
    ## Copy Polymode
    message("Adding Polymode files")
    from <- p0(G$polyzip, '.d')
    from <- globpaths(from, '/polymode*')    
    to <- slisp.pt(G$polyzip)
    makeDir(to)
    copy.glob(from, to, "*.el")

    ## Copy Poly-markdown mode
    message("Adding Poly-markdown files")
    from <- p0(G$polymarkzip, '.d')
    from <- globpaths(from, '/poly-markdown*')
    to <- slisp.pt(G$polymarkzip)
    makeDir(to)
    copy.glob(from, to, "*.el")

    ## Copy Poly-noweb mode
    message("Adding Poly-noweb files")
    from <- p0(G$nowebzip, '.d')
    from <- globpaths(from, '/poly-noweb*')
    to <- slisp.pt(G$nowebzip)
    makeDir(to)
    copy.glob(from, to, "*.el")
   
    ## Copy Poly-R mode
    message("Adding Poly-R files")
    from <- p0(G$polyrzip, '.d')
    from <- globpaths(from, '/poly-R*')
    to <- slisp.pt(G$polyrzip)
    makeDir(to)
    copy.glob(from, to, "*.el")
    
    ## Copy BM mode
    from <- p0(G$bmzip, '.d/bm-master')              
    to <- slisp.pt(G$bmzip)
    copy.dir(from, to, "BM mode")
    
    ## Copy BRemacs lib files
    makeDir(slisp.pt("bremacs"), "BRemacs library:")

    ## Get BRemacs lib files with ls or dir and parse into a string
    bfiles <- "
br-init-dbg.el  br-keys.elc    br-recentf.el   br-rnw.elc       br-simple-buffer-menu.el     
br-init.el      br-menico.el   br-recentf.elc  br-setmodes.el   br-simple-buffer-menu.elc  splith.svg      
br-keys.el      br-menico.elc  br-rnw.el       br-setmodes.elc  ess-init.R  splith.xpm
"
    
    bfiles <- gsub(" ", "\n", bfiles)
    bfiles <- strsplit(bfiles, "\n")[[1]]
    bfiles <- bfiles[nzchar(bfiles)]

    ## Download BRemacs lib files
    d <- slisp.pt("bremacs")
    x <- sapply(bfiles, function(f)
        download.git(makePath("src/bremacs/lib", f),  makePath(d, f)))
    download.git("src/bremacs/site-start.el",   slisp.pt("site-start.el")) 

    ## Environemnt diagnostic
    message("\nAdding ED tools")
    download.git("src/ed/bremacs.ed.cmd",      app.pt("ed/bremacs.ed.cmd")) 
    download.git("src/ed/bremacs-dbg.ed.cmd",  app.pt("ed/bremacs-dbg.ed.cmd")) 

    ## === Byte compile === ##

    message("Starting byte-compilation...")
    ## -batch switch (without GUI) works but it seems to exit immediately and go async

    ## Lisp path quote: "path\\to" -> \\\"path\\\\to\\\" 
    l <- function(path) { # <- a quoted windows path  
        x = gsub("\\", "\\\\", normalizePath(path, mustWork = FALSE), fixed=TRUE)
        sprintf("\\\"%s\\\"", x)
    }
    
    cmd <-   makePath(bremacs, 'bin/runemacs.exe')

    ## Standard Emacs sources are already in lisp-path 
    eldir <- globpaths(bremacs, 'share/emacs/[0-9]*')
    elisp.t <- " 
(progn
  (byte-recompile-directory %s 0)
  (kill-emacs 0))
"
    elisp <- sprintf(elisp.t,  l(work.pt(eldir)))
    args <- sprintf("-Q -eval %s", dQuote(elisp))
    shell.cd(c(work.pt(cmd), args))   
    ## messagev(c(work.pt(cmd), args)) # To test in PS replace eval "..." with '...'

    ## Site-lisp sources need adding their directories to lisp-path, we also create autoloads
    eldir <- globpaths(bremacs, 'share/emacs/site-lisp')
    elisp.t <-"
(let* ((site-lisp %s)
       (default-directory site-lisp)
       (pkg-name) (pkg-dir) (pkg-autoload-buf)
       (pkg-autoload-path) (pkg-autoload-basename))

  ;; genarate elc in all site-lisp subdirs
  (normal-top-level-add-subdirs-to-load-path)
  (byte-recompile-directory site-lisp 0)

  ;; genarate autoloads in all site-lisp subdirs
  (require 'package)
  (require 'autoload)
  (mapcar (lambda (pkg-dir)
	    (setq pkg-name (file-name-nondirectory pkg-dir)
		  pkg-autoload-basename (concat pkg-name (symbol-name '-autoloads.el))
		  pkg-autoload-path (expand-file-name pkg-autoload-basename pkg-dir))
		   
	    (unless (file-exists-p pkg-autoload-path)
	      ;; comments added to autoload files and 
	      (let ((autoload-timestamps nil)
		    (backup-inhibited t)
		    (version-control 'never)
		    (generated-autoload-file pkg-autoload-path)); wanted by upd-dir-autoloads
		(write-region (autoload-rubric pkg-autoload-path (symbol-name 'package) nil)
			      nil pkg-autoload-path nil 'silent)
		(update-directory-autoloads pkg-dir))
	      ;; update-directory-autoloads does not close generated-autoload-file 
	      (when (setq pkg-autoload-buf (find-buffer-visiting pkg-autoload-path))
		(kill-buffer pkg-autoload-buf))
		(message pkg-dir)))
	  
  	  (seq-filter 'file-directory-p (directory-files site-lisp t directory-files-no-dot-files-regexp)))
  (kill-emacs 0))
"       

# old        
#        "    
#(let* ((site-lisp %s)
#       (default-directory site-lisp))
#  (normal-top-level-add-subdirs-to-load-path)
#  (byte-recompile-directory site-lisp 0)
# 
#  (require 'package)
#  (mapcar (lambda (dir) (package-generate-autoloads (file-name-nondirectory dir) dir))
#  	  (seq-filter 'file-directory-p (directory-files site-lisp t directory-files-no-dot-files-regexp)))
# 
#  (kill-emacs 0))
#"
    elisp <- sprintf(elisp.t,  l(work.pt(eldir)))
    args <- sprintf("-Q -eval %s", dQuote(elisp))
    shell.cd(c(work.pt(cmd), args))        
    ## messagev(c(work.pt(cmd), args)) # To test in PS replace eval "..." with '...'
    
}


bloomrTree.Studio <- function(){
### We add the to the tree created by bloomrTree.brEmacs() the LaTeX related component
    
    makeStudio.addLatex()
    makeStudio.addPerl()
    makeStudio.addPandoc()
}


makeStudio.addLatex <- function(){
### Install LaTeX. Currently MiKTeX
           
    ## Latex variables  
    latdir <- app.pt("latex")
    latbin.pt <- makePath(latdir, "/texmfs/install/miktex/bin/x64/latex.exe")


    ## Expand MiKTeX 
    message("Adding MiKTeX files")
    miktex.exe <- p0(G$mikzip,'.d/miktexsetup_standalone.exe')       
    cmd <- p0(awin.pt(miktex.exe),
                  " --verbose --local-package-repository=", awin.pt(p0(G$mikzip,'.d')),
                  " --portable=", awin.pt(app.pt("latex")), 
                  " --package-set=basic install")
    shell.ps(cmd, awin.pt("ps.latexexpn.txt"))
    
    if(!file.exists(work.pt(latbin.pt)))
        stop(paste("An error occurred while extracting\n", G$mikinst,
                   "\nPlease, exit BloomR and try again."))
    

    ## Add extra packages required by knitr and rmardown::render
    message('\nUpdating packages')
    
    ## MiKTeX mpm.exe requires mapping a UNC build workdir do a drive
    orig.work <- G$work
    if(unc.is(G$work)) {
        message(G$work, " is a UNC path, which is not supported by MikTeX mpm, a mapped drive will be used.")
        G$work <- unc.subs(G$work)
    }

    ## Package to download and install
    toinstall <- "upquote microtype parskip kvoptions ltxcmds kvsetkeys xurl bookmark infwarerr kvdefinekeys
                  pdfescape hycolor letltxmacro auxhook intcalc etexcmds bitset bigintcalc rerunfilecheck
                  uniquecounter geometry fancyvrb framed booktabs footnotehyper refcount gettitlestring"
    toinstall <- paste(toinstall, "pdfcrop") # required by intermediate commands  
    toinstall <- gsub("[[:space:]]+", ",", toinstall)
    
    mpm <- app.pt("latex/texmfs/install/miktex/bin/x64/mpm.exe")
    if(!file.exists(work.pt(mpm))) stop(paste('Unable to find:\n', mpm))
    cmd <- p0(awin.pt(mpm), " --verbose --require=", toinstall)
    shell.ps(cmd, awin.pt("ps.addpacks.txt"))

    ## Update distro, necessary to avoid: 'major issue: So far, you have not checked for MiKTeX updates'
    cmd <- p0(awin.pt(mpm), "  --update --verbose")
    shell.ps(cmd, awin.pt("ps.latexupd.txt"))

    initexmf <- app.pt("latex/texmfs/install/miktex/bin/x64/initexmf.exe")
    if(!file.exists(work.pt(initexmf))) stop(paste('Unable to find:\n', initexmf))
    cmd <- p0(awin.pt(initexmf), " --report")
    shell.ps(cmd, awin.pt("ps.latexrpt.txt"))

    ## Restore previous drive mapping
    if(!is.null(G$tempmap)) unc.mapdel(G$tempmap)
    G$work <- orig.work
    
}

latex.autopackages  <- function(){
### This is a draft to be implemented, based on MiKTeX Just enough TeX, https://miktex.org/kb/just-enough-tex
    
    ## To identify the package needed by knit and rmarkdown:::redner, we use the MiKTeX auto-install-package feature.
    ## Step 1 Enable automatic packages with:
    ##       initexmf --set-config-value=[MPM]AutoInstall=yes
    ## Step 2 Generate a test.tex file from an Rmd.
    ## Step 3 Compile it and backup the resulting
    ##        c:/Users/kvm/Desktop/brEmacs/main/latex/texmfs/data/miktex/log/pdflatex.log
    ## Step 4 Parse for autoinstalled packages pdflatex.bak
    
    ## pdfcrop
    ## rmarkdown:::redner() makes some preliminary tex operations before generating the tex file.
    ## currently they seem figure cropping based on package pdfcrop, which in turn reuires perl
    ## If render() does not find pdfcrop package stops, so the auto-install will not operate for pdfcrop

    ## Activate AutoInstall packages
    initexmf <- app.pt("latex/texmfs/install/miktex/bin/initexmf.exe")
    cmd <- paste0(awin.pt(initexmf), " --set-config-value=[MPM]AutoInstall=yes")
    shell.ps(cmd, awin.pt("ps.latexautop.txt"))

    ## Find auto-installed packages
    latlog.path <- paste0("c:/Users/kvm/Desktop/", "brEmacs/main/latex/texmfs/data/miktex/log/pdflatex.log")
    latlog <- readLines(latlog.path)
    glines <- grep("pdflatex.packagemanager - installing package", latlog, value=TRUE)
    packs <- strsplit(glines, " ")
    if(length(unique(sapply(packs, length))) >1) stop("Unequal items in 'pdflatex.log' install lines\n",
                                                      paste((sapply(packs, paste, collapse=" ")), collapse="\n"))
    (packs <- sapply(packs, `[`, 9))
    paste(packs, collapse=" ")
    

}

latex.packList <- function(inst=FALSE){ # NOT USED
### Get list of all packages from MiKTeX mpm --list
## If inst=TRUE, give vector of names of installed ones

    mpm <- app.pt("latex/texmfs/install/miktex/bin/x64/mpm.exe")
    if(!file.exists(work.pt(mpm))) stop(paste('Unable to find:\n', mpm))
    out <- system(paste(win.pt(mpm), '--list'), intern = TRUE, invisible = FALSE)
    if(inst) {
        x <- grep('^i', out, value=TRUE)
        sapply(strsplit(x, ' +'), `[`, 4)
    } else out
}

latex.packCheck <- function(pname) # Check if a package is installed via MiKTeX mpm --list
    pname %in% latex.packList(inst=TRUE)    

makeStudio.addPandoc <- function(){ # NOT USED

    ## Pandoc variables (msi wants abs path)
    pandir <- app.pt("pandoc")
    
    ## Create Pandoc dir
    existMake(pandir, TRUE, FALSE, "Pandoc dir:")

    from <- Sys.glob(work.pt(p0(G$panzip,'.d/pandoc*')))
    file.rename(from, work.pt(makePath(pandir, "/bin")))
    
}

makeStudio.addPerl <- function(){
    
    message("Making Perl tiny")
    
    perlsource <- p0(G$perlzip,'.d')
    perldir <- app.pt("perl")
    perlbins <- makePath(perldir, "bin")
    perllibs <- makePath(perldir, "lib")

    existMake(perldir, TRUE, FALSE, "Adding Perl files")
    makeDir(perlbins)
    makeDir(perllibs)

    binpaths <- "
libgcc_s_seh-1.dll
libstdc++-6.dll
libwinpthread-1.dll
perl.exe
perl532.dll
"
    libpaths <- "
auto
Config.pm
Config_git.pl
Config_heavy.pl
constant.pm
Cwd.pm
DynaLoader.pm
Exporter
Exporter.pm
File
Getopt
overload.pm
overloading.pm
strict.pm
vars.pm
warnings
warnings.pm
Win32.pm
XSLoader.pm
"

    f <- function(string) sapply(strsplit(trimws(string), "\n")[[1]], trimws, USE.NAMES = FALSE)
    binpaths <-f(binpaths)
    libpaths <- f(libpaths)

    sbins <- sapply(binpaths, function(pth) makePath(perlsource, p0("perl\\bin\\", pth)))
    slibs <- sapply(libpaths, function(pth) makePath(perlsource, p0("perl\\lib\\", pth)))

    sapply(sbins, function(from) file.copy(work.pt(from), work.pt(perlbins)))
    sapply(slibs, function(from) file.copy(work.pt(from), work.pt(perllibs), recursive=TRUE))
}

###== Boot functions ==

initScripts <- function(){
### Make R and BloomR etc files (configs such as Rprofile.site from PROF()) and the launchers
### Etc. files are currently the same for all the editions while launchers differ 
    
    initScripts.etc()

    ## Make R bootstrapper launcher
    if(is.core())    makeLauncher.Core()

    ## brEmacs editons include also the Core edition, thus the launcher    
    if(is.bremacs()) {
        makeLauncher.Core()
        makeLauncher.brEmacs()
    }    
}

initScripts.etc <- function(){
       

    message("\nMaking etc/Rprofile.site and shared directory")

    ## Make new Rprofile.site and keep old
    p <- capture.output(PROF)  # Get PROF function definition 
    p <- p[-c(1, length(p))]   # Remove "function {", "}"
    prof.new <- app.pt('R/etc/Rprofile.site')
    prof.nat <- app.pt('R/etc/Rprofile.native')

    ## Append with Unix line endings
    if(!is.file(prof.nat)){
        prof.new <- work.pt(prof.new)
        prof.nat <- work.pt(prof.nat)
        file.copy(prof.new, prof.nat)                  
        con <-  file(prof.new, open="ab")
        writeLines(text=p, con=con)
        close(con)
    }
    
    ## Get bloomr lib files including xlx.R from Github
    to <- app.pt("R/share/bloomr")    
    makeDir(to,"BloomR share directory:")
    download.git("src/bloomr.init.R", app.pt("R/share/bloomr/bloomr.init.R"))
    download.git("src/bloomr.beta.R", app.pt("R/share/bloomr/bloomr.beta.R"))
    download.git("src/bloomr.api.R",  app.pt("R/share/bloomr/bloomr.api.R"))
    download.git("src/bloomr.R",      app.pt("R/share/bloomr/bloomr.R"))
    download.git("src/bloomr.sys.R",  app.pt("R/share/bloomr/bloomr.sys.R"))
    download.git("src/bloomr.test.R", app.pt("R/site-library/bloomr.test.R"))
    download.git("src/xlx/xlx.R",     app.pt("R/share/bloomr/xlx.R"))

    ## Testdata
    to <- app.pt("R/share/bloomr/testdata")
    makeDir(to,"BloomR share directory:")  
    download.git("src/testdata/testdata.high.R", app.pt("R/share/bloomr/testdata/testdata.high.R"))
    download.git("src/testdata/testdata.low.R", app.pt("R/share/bloomr/testdata/testdata.low.R"))
    
    ## Make personal dir with some sample files
    makeDir(root.pt('mybloomr'), "personal directory:")
    makeDir(root.pt('mybloomr/examples'), "personal directory:")
    download.git("res/semic.csv",             root.pt("mybloomr/examples/semic.csv"))
    download.git("res/tickers.csv",           root.pt("mybloomr/examples/tickers.csv"))
    download.git("res/tickers.eqt.csv",       root.pt("mybloomr/examples/tickers.eqt.csv"))
    download.git("res/my-first-report.Rmd",   root.pt("mybloomr/examples/my-first-report.Rmd"))
    download.git("res/tryme.R",               root.pt("mybloomr/examples/tryme.R"))
                  

}

makeLauncher.Core <- function(){
### Make launcher of Core editions

    ## Boot string    
    bloomr.run <- "
EnvSet,  BLOOMR,     %A_ScriptDir%
EnvSet,  HOME,       %A_ScriptDir%\\mybloomr
Run, %AppDir%\\R\\bin\\x64\\Rgui.exe LANGUAGE=en
"
    bloomr.run <- gsub("%AppDir%", G$appname, bloomr.run)
    makeLauncher_(bloomr.run, "core")
}

makeLauncher.brEmacs <- function(){
### Make launcher of Lab and Studio editions

    bremacs.run <-  "
EnvSet,  BLOOMR,     %A_ScriptDir%
EnvSet,  HOME,       %A_ScriptDir%\\main\\bremacs
Run, %AppDir%\\bremacs\\bin\\runemacs.exe -q --no-splash
"        
    bremacs.run <- gsub("%AppDir%", G$appname, bremacs.run)
    makeLauncher_(bremacs.run, "bremacs")
}


makeLauncher_ <- function(script.cont, edition){

   
    ## Make boot file
    ahkdir <- work.pt(p0(G$ahkzip, '.d/Compiler'))
    cat(script.cont, file=makePath(ahkdir, p0(edition, ".run")))
   
    ## Get icon from GitHub
    icon <- if(edition=="core") "bloomr" else "bremacs" 
    to <- makePath(p0(G$ahkzip, '.d/Compiler'), p0(edition, ".ico"))
    download.git(makePath("res", p0(icon, ".ico")), to)

    ## Core icon is currently as setup icon too. It is named bloomr.ico, should it change style in the future
    from <- makePath(ahkdir, "core.ico")
    to <- makePath(ahkdir, "bloomr.ico")
    if(edition =="core")  file.copy(from, to, overwrite=TRUE) 

    exename <-
        if(edition=="core") "bloomr-core.exe" 
        else if(edition=="bremacs") 
            if(is.lab()) "bloomr-lab.exe"
            else if (is.studio()) {
                ## if it a multi build, then we have the Lab launcher too, to delete
                (function(x) if(file.exists(x)) file.remove(x))(work.pt(makePath(G$branch,"bloomr-lab.exe")))
                "bloomr-studio.exe"
            }
    
    ## Make exe
    message("\nMaking executable: ", exename)
    wd <- normalizePath(ahkdir)
    run <- c("Ahk2Exe.exe",
             "/in", p0(edition, ".run"),
             "/icon", p0(edition, ".ico"),
             "/bin \"Unicode 32-bit.bin\"",
             "/out", exename
             ) 
    shell.cd(run, wd=wd)    

    ## Move exe
    from <- makePath(ahkdir, exename)
    to <- work.pt(makePath(G$branch, exename))
    file.rename(from, to)        
}

PROF <- function(){ #Keep this on separate line
### "etc/Rprofile.site" source (braces on separate lines) 
    
    ## BloomR bootstrap
    ## ================
    
    source(paste0(R.home("share"), "/bloomr/bloomr.init.R"))
    
    
    ## end BloomR----------
}



###== Utilities ==

### Exe and Zip distro
makeBundle <- function(bundle, # 'exe'/'zip'/'all'/'none'
                    ask
                    ){
### Create create exe, zip, both, or nothing   
### Every bundle value not 'exe'/'zip'/'all' means 'none'
    
    if(bundle=='all' || bundle=='exe') makeInst(ask)
    if(bundle=='all' || bundle=='zip') makeZip(ask)    
}


makeInst <- function(ask){

    message('\nCreating BloomR green installer')
    ## Set name (nsi name is "BRsetup.exe")
    to <- "BloomR-Core_setup_.exe"
    if(is.lab()) to <- "BloomR-Lab_setup_.exe"
    if(is.studio()) to <- "BloomR-Studio_setup_.exe"
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)
    download.git("bloomr.nsi", "bloomr.nsi")
    message('Creating green installer ', to, '\nThis may take a bit...')
    nsi <- 'bloomr.nsi'
    nexe <- p0(G$nsiszip,'.d/App/NSIS/makensis.exe')
    nsrc <- p0("/dSRCDIR=", G$branch)
    cmd <- c(win.pt(nexe), "/v2", nsrc, win.pt(nsi))
    shell.cd(cmd)    
    file.rename(work.pt("BRsetup.exe"), work.pt(to))
}


makeZip <- function(ask){

    message('\nCreating BloomR.zip')
    to <- "BloomR-Core_setup_.zip"
    if(is.lab()) to <- "BloomR-Lab_setup_.zip"
    if(is.studio()) to <- "BloomR-Studio_setup_.zip"
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)    
    from <- p0('./', G$branch, '/*')  # In 7z ./ removes dir prefix from archive files
    zexe <- get7zbin()
    cmd <- paste(win.pt(zexe), "a", win.pt(to), win.pt(from))
    ret <- system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}


shell.cd <- function(
### Similar to system2(), but can set the work dir and stops on errors
                      cmdvec,   # c(cmd, arg1,...) or c(cmd, agvec)
                                # cmdvec[1] is tested for exisitance and shQuoted if necessary
                     wd=NULL,   # optional work dir
                     raw=FALSE  # if T, do not pretty format the output with messagev
                      ){

### 1. system(), takes a single cmd string, in Linux prefixes with "sh -c"
### 2. system2() wants cmd + args. In Linux wraps system() adding "2>&1" if stderr = T.
### 3. shell(), Windows-only, wraps system() and prefixes the command string with "cmd.exe /c".  
### Don't invoke cmd.exe (so shell()) as it breaks on UNC paths. Prefer PowerShell, direct calls to exes or R builtins
### The best seems x <- suppressWarnings(system2(cmd, ags, stdout=T, stderr=T)); attr(x, "status")
### In Linux also suppressWarnings(tryCatch(system(paste(cmdstring, "2>&1"), intern=TRUE)))


    ## Set temporary wd
    oldwd <- getwd()
    if(!is.null(wd)) setwd(wd)

    ## Test command exists
    cmdnq <- gsub("^[\"']|[\"']$", "", cmdvec[1]) # unquote (if shQuote was used)
    notfound <- ! nzchar(Sys.which(cmdnq))

    ## Test Linux builtin
    linux  <- .Platform$OS.type == "unix"
    if(notfound && linux){
                    cmdstring <- paste("builtin", cmdvec[1], "2>&1")
                    out <- suppressWarnings(tryCatch(system(cmdstring, intern=TRUE)))
                    notfound <- attr(out, "status") > 0
                }
    
    if(notfound) {
        G$lastshell <- list(cmdvec=cmdvec, output=out <- paste("There is no command:\n", cmdnq))
        setwd(oldwd)
        stop(out,
             "\n(If the command requires args, make sure they are provided as (a) separate string(s) in the command vector.)",
             "\n\nSee G$lastshell for more.")
    }

    ## Execute
    out <- suppressWarnings(system2(cmdnq, cmdvec[-1], stdout=TRUE, stderr=TRUE))    
    setwd(oldwd)
    G$lastshell <- list(cmdvec=cmdvec, output=out)

    ## Stop on error ...
    if(!is.null(attr(out,"status")))
        stop(paste(cmdvec[-1], collapse=" "), "\n returned:\n", tail(out, 2), "\n\nSee G$lastshell for more.")

    ## or return the required ouptut
    if(raw) out else messagev(out, s='\n')
}

shell.ps <- function(
### Similar to shell(), but based on Powershell. tees stdout to file and console and stops on errors
                      cmdstr,   # command string
                     outfile    # optional work dir
                      ){

    pscmd <- sprintf("& {%s | Tee-Object %s; exit $LASTEXITCODE}", cmdstr, outfile)
    psline <- paste("powershell -NoProfile -command", pscmd)
    ret <- system(psline)
    if(ret) stop("The following powershell command exited with ", ret, ":\n  ", cmdstr,
                 "\nRead ", outfile, "for the command log.")    
}

###== Website helpers ==


web.greplink <- function(regexp, pos=0, url, abs=FALSE){
### Grep links from URL and return 0=all, 1=first, -1=last
### abs: Return absolute or relative URL paths
    
    req <- curl_fetch_memory(url, new_handle())
    req <- rawToChar(req$content)        
    lnks <- xpathSApply(htmlTreeParse(req, useInternalNodes = TRUE), 
                        "//a", xmlGetAttr, "href")
    glinks <- grep(regexp, lnks, ignore.case=TRUE, value=TRUE)
    if(abs) glinks <- p0(p0(sub("/$", "", url), "/"), glinks)
    if(pos==1) return(glinks[1])
    else if (pos==-1) return(tail(glinks,1))
    else
        glinks  
}


sfFirstbyProject <- function (project, filtx, quiet=FALSE){
### First SF project item matching regex filter (url made from prj name)
    
    if(!quiet) messagev('Searching last', project, 'version on SF.net')
    url <- p0("https://sourceforge.net/projects/", project, "/files/")
    ref <- "https://sourceforge.net"    
    page <- download.html(url)    
    url <- xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
                    "//a[span[@class='name']]",  xmlGetAttr, "href")
    url <- grep(filtx, url, value=TRUE, ignore.case=TRUE)[1] 
    if(substr(url,1,1)=='/') url <- p0(ref, url)#relative to absolute
    return (url)
}

sfFirstbyUrl <- function (url, versionx, quiet=FALSE){
### First SF url item matching regex filter (url given from prj name)

    if(!quiet) messagev('Searching for version', versionx, 'on\n', url)
    ref <- "https://sourceforge.net"
    if(substr(url,1,1)=='/') url <- p0(ref, url)#relative to absolute
    page <- download.html(url)    
    url <- xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[span[@class='name']]",  xmlGetAttr, "href")
    return (grep(versionx, url, value=TRUE, ignore.case=TRUE)[1])
}

sfDirLink <- function (url, quiet=FALSE){
### Follow the direct-download link 

    if(!quiet) messagev('Find best mirror for\n', url)
    ref <- "http://sourceforge.net"
    page <- download.html(url, refr=ref)    
    url <- xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[@data-release-url]",  xmlGetAttr, "data-release-url")
    return (url)
}


cran.geturl <- function(pack){
## Get  CRAN Windows binaries release for a package 

    ## CRAN links
    cranpage <- "http://cran.r-project.org/web/packages/"
    cranbin <- "http://cran.r-project.org/bin/"

    ## Get package page 
    url <- p0(cranpage, pack, "/index.html")
    page <- download.html(url)    

    ## Get bin url: the first occurence on page is the dev version, second is release
    url <- regmatches(page, gregexpr("windows/contrib.*?\\.zip", page))[[1]][2]
    p0(cranbin, url)   
}



###== Donwload helpers ==


download.nice <- function(from, to, overwrite, desc=""){
### Download relative to the build workdir with nice user info and overwrite management

    if(desc=="") desc <- sub('.+/', '', to)
    to <- work.pt(to)
    message("\nDownloading ", desc) 

    if(is.path_(to)) {
        warn.path(to, "already exists.")
        if(!overwrite){
            message("Skipping action to preserve existing download.")
            return()
        }
    }

    ## Execute from callback 
    if(is.function(from)) from <- from()
    message("from:\n", from)

    ## Download ndown times and exit deleting file on errors
    for(i in 1:G$ndown)
        if(s <- !is.null(tryCatch(
                     curl_download(from, to, quiet = FALSE),
                     error = function(x) cat("\nCurl Error:", x$message, '\n')))) break
    if(!s){
        unlink(to, force = TRUE)
        stop('\nDownload error')
    }
}


download.html <- function(url, refr=NULL){
### Download html page with simple progress and stop on error

    h <- new_handle()
    handle_setopt(h, referer = refr)
    for(i in 1:G$ndown){
        req <- curl_fetch_memory(url, handle=h)
        if(is200 <- req$status_code == 200) break
    }
    if(!is200) stop('\nRequest for page\n', url, '\ngave error ', req$status_code)
    rawToChar(req$content)        
}


download.git <- function(file, to, overwrite=TRUE, desc=""){
### Download (realtive to work.pt) from github or use local git dir
### Git dir can be set with makeBloomR(gitsim=...) and is shared via global var G$github.local 
    
    if(!nzchar(desc)) desc <- file
    
    ## remote git
    if(!nzchar(G$github.local)) {
        from <- makePath(G$github, file)
        download.nice(from, to, overwrite=TRUE, desc)
        
    ## local git
    } else {
        messagev('\nDownloading', desc)
        from <- makePath(G$github.local, file)  # not relative to the build workdir 
        if(!is.path_(from)) stop(from, "\nnot found in local Git repo:\n", G$github.local)
        to <- work.pt(to)
        file.copy(from, to, overwrite=TRUE) 
    }

}



###== File System ==
makePath <- function(parent, child){    
### Chain parent-child paths managing '/' middle slashes 
## You don't have to rememeber if you need to add or not that slash

    ## No trail for parent 
    parent <- sub('/$', '',  parent)
    parent <- sub('\\\\$', '',  parent)

    ## No lead for child
    child <- sub('^/', '',  child)
    child <- sub('^\\\\', '',  child)

    ## Now file.path inputs is secure
    file.path(parent, child)

}

work.pt <- function(dir=""){    
### Prefix dir path with work dir path
### Work dir path is set with global G$work

    if(is.null(G$work)) stop("`G$work' build workdir is not set!") 
    if(is.abspath(dir))
        stop("The path you provided:\n", dir, "\nis absolute, but it should be relative to:\n", G$work)
    makePath(G$work, dir)
}

root.pt <- function(dir=""){
### Prefix dir path with BloomR distro root path relative to the build workdir.
### Depends on distro under build: e.g. path/to/workdir/brCore

    makePath(G$branch, dir)
}


app.pt <- function(dir=""){
### Prefix dir path with BloomR apps' path relative to the build workdir.
### App dir name depends on G$appname, its parent is BloomR root dir
### E.g. path/to/workdir/current-distro/G$appname

    x <- root.pt(G$appname)
    makePath(x, dir)
}

slisp.pt <- function(dir=""){
### Prefix dir path with BRemacs site-lisp path relative to the build workdir.
    
    x <- app.pt("bremacs/share/emacs/site-lisp")
    makePath(x, dir)
}



win.pt <- function(path){
### Given a Unix path relative to G$work build workdir, quote, windows-ize, and prefix with wordir
### If G$work=="./workdir", "foo/bar" -> "\".\\wordir\\foo\\bar\""
### This function is used to send paths Windows shell (e.g. with shell)
    
    path <- work.pt(path)            
    path <- shQuote(path)
    chartr("/", "\\", path)
}

awin.pt <- function(path){
### Convert a Unix path, relative to G$work build workdir, to a shell-quoted absolute Windows path. 

    path <- work.pt(path)            
    path <- normalizePath(path, mustWork=FALSE)
    shQuote(path)
}


    
is.path <- function(path){ # Path exists relative to the build workdir
    path <- work.pt(path)
    is.path_(path)
}

is.path_ <- function(path){ ## Test for a generic path existence.
### Not to be used by main code, but only by filesytem related functions
    
    path <- sub("/$", "", path) # R file.exists does'nt want a trailing "/" for a Windows directory
    file.exists(path)
}


is.file <- function(file){ # Path exists as a file relative to the build workdir    
    is.path(file) && !dirtype_(file)
}

is.dir <- function(dir){ # Path exists as a dir relative to the build workdir
    is.path(dir) && dirtype_(dir)
}

is.empty <- function(dir){
### Dir (relative to the build workdir) is empty
### Also: T, if does not exist; F, if is a file

    if(is.file(dir)) return (FALSE)
    is.empty_(dir)
}

is.empty_ <- function(dir){ # is.empty workhorse
    length(dir(work.pt(dir), all.files=TRUE, no..=TRUE)) ==0
}


chk.file <- function(file, desc=""){ # Break if path relative to the build workdir not a file
    if(!is.path(file)) stop(file, "\nis not a valid path")
    if(is.dir(file))   stop(file, "\nis a directory")
}

chk.dir <- function(dir){ # Break if path relative to the build workdir not a dir
    if(!is.path(dir)) stop(dir, "\nis not a valid path")
    if(!is.dir(dir))  stop(dir, "is a file")
}

dirtype_ <- function(file){ # File relative to the build workdir is of type dir
    file.info(work.pt(file))$isdir
}

is.abspath <- function(path){
### True if path is absolute
    
    ## Linix /path
    if (grepl("^/", path))
        return(TRUE)
    
    ## Windows X:/ or X:\\
    if (grepl("^[[:alpha:]]:(/|\\\\)", path))
        return(TRUE)

    ## Tilde ~
    if (grepl("^~", path))
        return(TRUE)

    ## UNC \\\\path
    if (grepl("^\\\\\\\\", path))
        return(TRUE)
    
    FALSE    
}

makeDir <- function(dir, desc=""){
### Create a dir relative to the build workdir with optional desc message and stop on errors   
### The function invisibly returns dir
    
    if(nzchar(desc)) message("Creating ", desc )
    del.path(dir)
    makeDir_(dir, desc)
}

makeDir_ <- function(dir, desc) { # makeDir FS helper        
    fulldir <- work.pt(dir)
    dir.create(fulldir, showWarnings = FALSE)
    ## Unable to create? 
    if(!is.path_(fulldir))
        stop("\nUnable to create ", desc, "\n", dir)
    invisible(dir)
}

del.path <- function(path){
### Delete path relative to the build workdir and break on fail 
    if(!is.path(path)) return()
    del.path_(path)
}

del.path_ <- function(path){ # del.path workhorse

    path <- work.pt(path)
    unlink(path,recursive=TRUE, force=TRUE) #  non-existent file is not an unlink failure
    if(is.path_(path)) {
        Sys.sleep(1.5)
        if(is.path_(path)) stop("\nUnable to access\n", path)
    }
}

del.ask <- function(path, ask, desc){
### Warn on relative to the build workdir path existence.
### if ASK flag is set, ask for delete confirmation, else just notify
### Actual delete is not peformed here    

    del.ask_(path, ask, desc)
}

del.ask_ <- function(path, ask, desc){ # del.ask workhorse
    path <- work.pt(path)
    del.ask.abs_(path, ask, desc)
}

del.ask.abs_ <- function(path, ask, desc){
### Warn on path existence, relative to currpath.
### if ASK flag is et, ask for delete confirmation, else just notify

    warn.path(path, desc) ## Path warnings are better relative to currpath then the build workdir
    
    ## Confirmation not required
    if(!ask) {
        message("It will be deleted ...")
        return()
    }
    ## Require confirmation
    repeat{
        ans <- toupper(readline("Delete it? (Y/n) : "))
        if(ans=="Y" || ans=="N" || ans=="") break
    }    
    if(ans=="N") stop("Stopped by user.")
}

copy.glob <- function(srcdir, destdir, subpaths){
### Copy recursively the SUBPATHS vector of SOURCEDIR to DESTDIR. Paths should be relative to the build workdir 
### SUBPATHS and SOURCEDIR can use globs, but SOURCEDIR must identify a single directory.
### If necessary, recursively create the ancestor directories of SUBPATHS

    ## Expand globs
    srcpaths.xpn <- globpaths(srcdir, subpaths, both=TRUE)  # parent/children 
    subdirs.xpn  <- globpaths(srcdir, subpaths, both=FALSE) # children only

    ## Generate destinations to indentify ancestors to be created
    destpaths <- sapply(subdirs.xpn, function(sub) makePath(destdir, sub))

    ## Create ancestors
    sapply(unique(dirname(destpaths)), function(dir) if(!is.dir(dir)){
                                                         message("Creating directory\n", dir)
                                                         dir.create(work.pt(dir), recursive=TRUE)})
    ## Perform copy 
    rets <- sapply(seq_along(srcpaths.xpn), function(i)
        file.copy(work.pt(srcpaths.xpn[i]), work.pt(dirname(destpaths[i])), recursive = TRUE))    
    if(length(which(!rets))) stop(pv("Unable to write to the following destination/s:", destpaths[which(!rets)], s='\n'))
}
    
globpaths <- function( # Return parent/children, resolving globs (not regex). Both dirs should refer to the build workdir.
### Parent is scalar and its expansion should be unique. Children is a vector and each expansion can generate multiple matches
                     parent,   # with globbing, but its expansion gives a single path 
                     children,    # with globbing
                     both=TRUE # return only resolved child, without parent 
                     ){

    unlist(
        sapply(children, function(child) globpath_(parent, child, both=both), 
               USE.NAMES = FALSE))
}

globpath_ <- function( # globpaths() work horse, accepting a scalar child
### Return parent/child, resolving globs (not regex). Both dirs should refer to the build workdir, parent should expand to a unique path.
                      
                     parent,   # with globbing, but its expansion gives a single path 
                     child,    # with globbing
                     both=TRUE # return only resolved child, without parent 
                     ){

    ## Can't normalize winslash = "/", as it won't work with UNC paths 
    abswdir <- normalizePath(work.pt())
    absprnx <- normalizePath(Sys.glob(work.pt(parent)), mustWork = FALSE)
    if(length(absprnx) > 1)
        stop("When solving parent/child globs, parent expansion should be unique, but there are ", length(absprnx),
             " parent expansions:\n", pv(absprnx, s='\n'))
    
    relglob <- work.pt(makePath(parent, child))
    relxpns <- Sys.glob(relglob)
    if(!length(relxpns))
        stop ("The following expression does not expand to an existing path:\n", relglob)
    
    absxpns <- sapply(relxpns, normalizePath, mustWork = FALSE, USE.NAMES = FALSE)
    rempath <- ifelse(both, abswdir, absprnx)
    rempath <- gsub("\\", "\\\\", rempath, fixed=TRUE) # escape any backslashes for regexps
    relxpns <- sub(p0("^", rempath), "", absxpns)
    ## relxpns <- sub("^$", ".", relxpns)    # empty paths are replaced with dot
    relxpns <- sub("^/", "", relxpns)     # no leading slash 
               sub("^\\\\", "", relxpns)  # no leading backslash 
}

make.relative <- function(path, relto, append = FALSE){ # NOT USED
### Return PATH relative to RELTO, if possible, or PATH. Both PATH and RELTO should be in turn relative to the the build workdir
### If APPEND, try to prefix PATH with RELTO. 

    if(length(relto)>1)
        stop('You can only make a path relative to a single dir and yu specified the following ones:\n', pv(relto, s='\n'))
    
    if(is.file(relto))
        stop("You are trying to make a path relative to:\n", relto, "\n, but this path exist as a file")

    abspath <- normalizePath(path, winslash = "/", mustWork = FALSE)
    absrelto <- normalizePath(relto, winslash = "/", mustWork = FALSE)

    if(grepl(p0('^', absrelto), abspath)) {
        rel <- sub(p0('^', absrelto), "", abspath)
        rel <- sub("^$", ".", rel)    # if empty replaced with dot
        rel <- sub("^/", "", rel)     # no leading slash 
        if(append) makePath(relto, rel) else rel
    } else path 
}

is.relative  <- function(path, relto, append){ # NOT USED
### TRUE if PATH is relative to RELTO, if possible, or PATH. Both PATH and RELTO should be in turn relative to the build workdir


    abspath <- normalizePath(path, winslash = "/", mustWork = FALSE)
    absrelto <- normalizePath(relto, winslash = "/", mustWork = FALSE)

    outpath <- if(grepl(p0('^', absrelto), abspath)) {
        rel <- sub(p0('^', absrelto), "", abspath)
        rel <- sub("^$", ".", rel)    # if empty replaced with dot
        rel <- sub("^/", "", rel)     # no leading slash 
        if(append) makePath(relto, rel) else rel
    } else path
    
}


copy.dir <- function(from, to, desc=""){
### Copy FROM directory in TO parent and rename TO as FROM
### If TO dir exists delete it. FROM/TO are relative to the build workdir 
### E.g: if from='a/b/x' and to='c/d/y', copies 'x' under 'c/d/' and renames the copied 'x' as 'y'. 
### Warning: If the temp dir 'c/d/x' exists, an error is raised
    
    if(nzchar(desc)) message("\nAdding ", desc)
    del.path(to)
    copy.dir_(from, to)
}

copy.dir_ <- function(from, to){ # copy.dir work horse

    from <- work.pt(from)
    to <- work.pt(to)
    tempdir <- makePath(dirname(to), basename(from))
    if(dir.exists(tempdir))
        stop("To creare\n", to, "\nI need to create\n", tempdir, "\nbut the latter already exists and cannot be overwritten")
    file.copy(from, dirname(to), recursive=TRUE)  # this creates tempdir
    file.rename(tempdir, to)
}

existMake <- function(dir, overwrite, ask, desc=""){
### If dir relative to the build workdir does not exist make it, otherwise might ask and skip creation 
### An empty-dir is considered non-existent. Note: if dir="", it is the build workdir 

    ## Inform user with desc if any
    if(nzchar(desc)) message("\nCreating ", desc, '\n',  dir)

    ## Not a dirty dir
    if(!is.path(dir) || is.empty(dir)){
        makeDir(dir)
        return(invisible())
    }
    ## Dirty, and not overwritable 
    else if(!overwrite) message("Skipping action, to preserve existing downloads!")
    ## Dirty, but overwritable 
    else {
        del.ask(dir, ask, "exists non-empty")
        del.path(dir)
        makeDir(dir)
    }             
}

file.read <- function( # Read file  
                   fpath # File path relative to the build workdir
                   )    
    readLines(work.pt(fpath))


uzip <- function(from, to, desc, delTarget=TRUE){
### Unzip making path relative to the build workdir
### Stops on errors and inform used with a desc argument

    message('\nExpanding ', desc, '...') 
    chk.file(from)
    if(delTarget) del.path(to)
    uzip_(from, to)
    
}

uzip_ <- function(from, to){ # uzip workhorse    

    from <- work.pt(from)
    to <- work.pt(to)
    if(length(unzip(from, exdir= to))==0)
        stop('\nUnable to perform extraction')  
}

utar <- function(from, to, desc, delTarget=TRUE){
### Untar making path relative to the build workdir
### Stops on errors and inform used with a desc argument

    message('\nExpanding ', desc, '...') 
    chk.file(from)
    if(delTarget) del.path(to)
    utar_(from, to)
    
}

utar_ <- function(from, to){ # uzip workhorse    


    ## Untar requires mapping a UNC build workdir do a drive
    orig.work <- G$work
    if(unc.is(G$work)) {
        message(G$work, " is a UNC path, which is not supported by untar, a mapped drive will be used.")
        G$work <- unc.subs(G$work)
    }

    from <- work.pt(from)
    to <- work.pt(to)
    tryCatch(untar(from, exdir= to), finally={    
        ## Restore previous drive mapping
        if(!is.null(G$tempmap)) unc.mapdel(G$tempmap)
        G$work <- orig.work
    })
     
}


uzip.7z <- function(from, to, desc, delTarget=TRUE){
    zexe <- get7zbin()
    if(is.path(to)) {
        message('\nDeleting exisiting ', desc)
        del.path(to)
    }    
    message('\nExpanding (w/ 7zip) ', desc)
    message('This may take a bit ...')
    cmd <- paste(win.pt(to), win.pt(from))
    cmd <- p0(win.pt(zexe), ' x -aoa -r -o', cmd)
    ret <- system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}

get7zbin <- function(){ # Get 7z.exe relative to the build workdir
    zipdir <- p0(G$pzip,'.d')
    versiondir <- get7zbin.ver_(zipdir)
    subpath <- '/res/7z/7z.exe'
    makePath(zipdir, p0(versiondir, subpath))
}

get7zbin.ver_ <- function(zipdir){ # get7zbin workhorse
    zipdir <- work.pt(zipdir)
    dir(zipdir)
}


innoextract <- function(from, to, desc, delTarget=TRUE){
    exe <- getInnobin()
    if(is.path(to)) {
        message('\nDeleting exisiting ', desc)
        del.path(to)
    }    
    message('\nExpanding (w/ innoextract) ', desc)
    message('This may take a bit ...')
    cmd <- paste(win.pt(exe), win.pt(from),  "--output-dir", win.pt(to))
    ret <- system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}


getInnobin <- function(){ # Get 7z.exe relative to the build workdir
    innodir <- p0(G$innozip,'.d')
    makePath(innodir, "innoextract.exe")
}


getDeps <- function(pnames){
### Get recursively a package depenecies/imports

    myCRAN <- "http://cran.r-project.org"
    
    ## Set package repository, unless already set 
    repOpt <- getOption("repos")
    if(repOpt["CRAN"] == "@CRAN@")  repOpt["CRAN"] <- myCRAN
    options(repos=repOpt)       

    sort(unique(unlist(utils:::.make_dependency_list(pnames, available.packages(), recursive=TRUE))))

}


getDeps.format <- function(packstring){
### Format space separated package string and get packs+deps vector
    
    pnames <- strsplit(gsub('(^ +)|( +$)', '', packstring), split=' +')[[1]]
    unique(c(pnames, getDeps(pnames)))
}

getImports <- function(pnames, available=NULL){
### Get recursively package imports
### NOT USED. Replaced by getDeps

    myCRAN <- "http://cran.r-project.org"
    
    ## Get available CRAN packages
    if(is.null(available)) {
        ## Set default repository (to install packaes)
        repOpt <- getOption("repos")
        if(repOpt["CRAN"] == "@CRAN@")  repOpt["CRAN"] <- myCRAN
        options(repos=repOpt)       
        available <- available.packages()
    }
    udeps <- unique(unlist(lapply(pnames, function(pname){       
        deps <- NA
        if(pname %in% rownames(available)){
            deps <- available[pname,"Imports"]
            deps <- gsub("\\(.+?\\)", "", deps)
            deps <- gsub("\\n", "", deps)
            deps <- gsub(" +", "", deps)
            deps <- strsplit(deps, ",")[[1]]
        }

        if(!any(is.na(deps))) deps <- c(deps, getImports(deps, available))
     })))

    ## Remove initial
    setdiff(udeps, pnames)

}


###== UNC Paths ==

unc.is <- function(path){
### Check if a path is a UNC path
    
    apath <- normalizePath(path, mustWork=FALSE)
    grepl("^\\\\\\\\", apath)
}    

unc.top <- function(uncpath){
### Extract the top level dir from a UNC path (i.e. immediately below the server name) with no trailing slash
### To test if the path is UNC, use unc.is()
    apath <- normalizePath(uncpath, mustWork=FALSE)
    p0("\\", strsplit(apath, "\\\\+")[[1]][1:3], collapse="")
}   

unc.hasmap <- function(uncpath){
### Given unc.is(uncpath), test if the whole path or its top level dir is mapped to a network drive
### E.g. in \\server\share\long\path, \\server\share or \\server\share\long\path are mapped to Z:    

    mps <- unc._maps()
    
    ## Test full 
    uncpath <- normalizePath(uncpath)
    if(uncpath %in% mps$share) return(TRUE)

    ## Test top level
    top <- unc.top(uncpath)
    top %in% mps$share
}

unc.getmap <- function(uncpath){
### Given uncpath unc.hasmap(), extract the first available mapped drive or NULL
### If a map to both full path and top level dir exists, prefer the full path
### Before extracting, test with unc.hasmap()

    mps <- unc._maps()

    ## Test full 
    uncpath <- normalizePath(uncpath)
    if(length(pos <- grep(uncpath, mps$share, fixed=TRUE)))
        return(mps$drive[pos])
        
    ## Test top level
    top <- unc.top(uncpath)
    if(length(pos <- grep(top, mps$share, fixed=TRUE)))
        mps$drive[pos]
} 

unc.setmap <- function(uncpath){
### Map a net share to a drive. Return a letter-colon or error and set G$tempmap
### Before using, test that !unc.hasmap(uncpath)

    share <- unc.top(uncpath)
    out <- system(paste("net use *", shQuote(share), "/persistent:no"), intern=TRUE)

    if(out[3] !=  "The command completed successfully.")
        stop("Unable to map ", share, " to a network")

    message(drv <- out[1])
    dl <- regmatches(drv, regexpr(".:", drv))
    (G$tempmap <- dl)
}

unc.mapdel <- function(drive){
### Delete a mapped drive previously created as stored in G$tempmap

    mps <- unc._maps()
    pos <- grep(toupper(drive), mps$drive)
    share <- mps$share[pos]

    shell.cd(c("net", "use", drive, "/delete"))
    mps <- unc._maps()
    if(length(pos <- grep(toupper(drive), mps$drive))){
        stop("I was unable to disconnect mapped drive ", toupper(drive), " from\n", share)
    } else {
        messagev("Mapped drive", toupper(drive), "was disconnected from", share)
    }

    G$tempmap <- NULL
    
}


unc.subs <- function(uncpath){
### Replace all or part of the UNC path with a mapped drive
### If a mapped drive is found with unc.hasmap() is used else a drive is mapped to uncpath top level dir

    ndrive <- if(unc.hasmap(uncpath)) unc.getmap(uncpath) else unc.setmap(uncpath)
    sub(unc.top(uncpath), ndrive,
        apath <- normalizePath(uncpath),
        fixed=TRUE)
}

unc._maps <- function(){
### A data.frame version of NET USE, where ()$drive, ()$share are resp. the mapped drive and the remote share

    mps <- read.csv(text=shell.cd(c("wmic", "netuse get LocalName,RemoteName /format:csv"), raw=TRUE),
                    stringsAsFactors=FALSE) # <- to be removed

    if(ncol(mps)==1) mps <- as.data.frame(t(rep("", 3)), stringsAsFactors=FALSE)
    names(mps)=c('pc', 'drive', 'share')
    mps       
}


### == Messages ==

warn.path <- function(path, mess){
    message("Warning:\n", path, "\n", mess)
}

messagev <- function(..., s=" ") { # message() converting argument to a single vector and separating elements with `s'
    ## see also pv()
    message(pv(..., s=s))
}

pv <- function(..., s=" ") { # paste() converting argument to a single vector and separating elements with `s'
   paste(c(...), collapse=s)
}

### no more paste0 ever
p0 <- paste0


### probably not used
   
### Check if we can overwrite non-empty dir and possibly stop
chk.write <- function(path, over, desc="", stop=TRUE){
    ## Empty dir are overwritten without prompt
    ## File is considered a non-empty dir
     
    if(nzchar(desc)) desc <- paste(desc, '\n')

    ## Ret if non-exisitng path
    if(!is.path(path)) return(TRUE)
    
    ## Stop/warn if dir is non-empty and no right to overwrite
    if(is.path(path) && !is.empty(path)){   
        if(over){
            warn.p(path, "already exists")}
        else {
            if(stop)  exit.p(path, desc, "already exists")
            warn.p(path, "already exists\nSkipping action!")
            return(FALSE)
        }
    }
    return(TRUE)
}
       

### Exsisting paths warn
warn.p <- function(path, mess){
    messagev("Warning:", mess.p(path, mess))
}

### Exisintg paths
mess.p <- function(path,  mess){
    p0("\n", path, "\n", mess)
}

### Exisintg paths stop
exit.p <- function(path,  mess){ 
    stop(mess.p(path,  mess))
}

### Generic download info for custom download
mess.down <- function(desc){
    messagev("\nDownloading", desc)
}

