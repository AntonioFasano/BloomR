###  BloomR source

##  TODO
##  In bloomr.init.R, eikonapir loading removes requestInfo from .GlobalEnv with a hack. Test it and then delete eikon.fallback() 
##  Byte compile is async. "0" can be removed in case of standard emacs elisp files
##  Finalise dir temp-help docs source dir
##  Make an external file for byte-compile and autoloads and solve the async problem see below. 
##  Custom polymode (bremacs-rmd-mode), temporary disabled, to be restored in br-setmode.el
##  melpa.getpak check if other packages can benefit from it otehr than ESS
##  Repurpose tests and test data 
##  RCurl find and remove references everywhere 
##  bloomr.time.Rmd introduces a system to auto extract args desc from comment (see parseargs) consider use it.
##  In `knit.twice` in "bloomr.time.Rmd" add the path hacks as `knit2` in "bloomr.Rmd" so as to build it in BloomR.
##  Separate code (e.g. in bloomr.Rmd) requiring a Bloomberg or Refinitive to test from knitting, time, system code. 
##  In bloomr.Rmd bdh always.display.ticker, dates.as.row.names and perhaps more relate to old java package.
##  Implemented gitsim=TRUE the git simulated dir to set to bloomr.build this script dir.
##  Time startup of br-init.elc and possibly use it.

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
##  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
##  TinyTeX https://yihui.org/tinytex/TinyTeX-1.zip
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
G$tempmap   <- NULL # Temporary Windows net drive, used by commands not supporting a detected UNC path (NOT USED now)

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

pks <- "knitr Rblpapi xts XML httr rmarkdown sodium httpuv RCurl" # RCurl to be removed
pks <- paste(pks, "plyr pbapply") # for read read.xlx

## All packs deps
G$packlist <- pks
rm(pks)

## BloomR infrastructure packages
G$bloomrpacks <- c("secretR", "pcloudr", "elearnr") |>
    paste0(".tar.gz")
 
## Innoextract
G$innourl <- "http://constexpr.org/innoextract/files"
G$innozip <- 'innoextract'

## R
G$rurl <- "https://cloud.r-project.org/bin/windows/base/"
G$rzip <- 'rmain'

## Emacs
G$emacsurl <- "http://ftp.gnu.org/gnu/emacs/windows/"
G$emacszip <- "emacs"
G$emacs.type <- "emacs.*?.zip$" # e.g. emacs-28.2.zip

## SF items
G$pzip <- "peazip"
G$rport <- "rportable" # not used
G$nsisurl <- 'portableapps'
G$nsiszip <- 'nsis'

## ESS, Polymode, Poly-markdown, Poly-noweb, BM
G$melpalibs <- NULL # melpa.org packages' names/version
G$essurl <- 'ess.tar'
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

## Pandoc for Studio
G$panurl <- "https://api.github.com/repos/jgm/pandoc/releases"
#G$paninst <- "pandoc.msi" #to remove this
G$panzip <- "pandoc"

## TeX distro
G$texurl <- "https://yihui.org/tinytex/TinyTeX-1.zip"
G$texzip <- "tinytex"
#old
G$mikpaks <- c('fancyvrb', 'microtype', 'mptopdf', 'upquote', 'url',
               'parskip', 'framed', 'titling', 'booktabs', 'etoolbox')

## Local paths
G$work <-    "" # This is the build workdir, not to be confused wtih R getwd()
G$downdir <- "" # This is the downloads dir. 
G$appname <- "apps" # BloomR application folder name. Used by app.pt() 
G$branch <- NULL # Branch dir. The value can be "brCore" or "brEmacs" (for non-Core editions)

## Arguments
G$bremacs <- NULL
G$studio <- NULL
G$ndown <- 0
G$what <- NULL # What edition? If what !'core', then is.bremacs() TRUE
G$builds <- NULL # Char vector of remaining builds to go 
G$alttex <- FALSE # Try alt LaTeX repos on failures


## Path convention
## Paths are normally related to G$work or G$downdir.
## Absolute paths are retrieved by means of work.pt() and down.pt().
## Other *.pt functions return paths relative to special BloomR folders e.g. root.pt().
## Primitive functions not using the *.pt functions use a foo_ template

makeBloomR <- function( # Build BloomR
                    work,         # work dir path, absolute or relative to cur path
                    downdir=NULL, # optional resoruce download directory   
                    tight=FALSE,  # Reuse the downloads directory and build workdir 
                    ndown=2,      # num of download attempts
                    what='all',   # what edition: all/core/lab/studio

                    ## For debug/test:
                    bundle='exe', # exe/zip/all/none make the related installer for distribution
                    ask=TRUE,     # asks if to overwrite the existent build workdir and installer
                    alttex=FALSE, # Try alternative LaTeX repos on failures, beyond the suggested one 
                    deb=1:6,      # defaults to 1:6 to execute all steps build steps, modify to debug, avoid with what=all
                    gitsim=FALSE, # local path (abs. or relative) to simulate github downloads.
                    reset=TRUE    # Only set FALSE internally to allow multi-builds calls and keep globals
) {
    
    ## Set work dir
    if(!nzchar(work)) stop("Please, specify a work directory as first arg!")
    chk.colon(work)
    G$work <- work 

    ## Set downloads dir
    if(!is.null(downdir)){
        chk.colon(downdir)
        if(is.subdir_gen(downdir, work))
            stop(downdir, "\n cannot be a subidrectory of\n", work, ".")
    }    
    G$downdir <- if(is.null(downdir)) work.pt("!downloads") else downdir
       
    ## Set git dir
    G$github.local <- ""
    if(gitsim!=FALSE && nzchar(gitsim))
        if (file.info(gitsim)$isdir)
            G$github.local <- gitsim else {
                stop(gitsim, "is not an existing dir")}

    ## Windows?
    if(.Platform$OS.type != "windows")
        stop("Sorry, Bloomberg Terminal only exists for Windows and so BloomR.")
    
    ## Check for required packages
    if(!loadLib("curl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Parse Arguments
    editions <- c('all', 'core', 'lab', 'studio')
    if(! what %in% editions) messagev("'what' argument is not in", sQuote(editions))
    G$what <- what
    G$alttex  <- alttex

    ## Don't touch the build history, if reset =FALSE 
    if(reset) {
        G$builds <- what
        if(what == 'all') G$builds <- editions[-1]
    }

    ## We use a single branch directory for Lab and Studio. The former needs be bundled before adding Studio files
    G$branch <- ifelse(is.bremacs(), "brEmacs", "brCore") 
    
    ## Parse residual arguments
    G$ndown <- ndown

    ## Step 1
    if(1 %in% deb) {
        existMake(   '', overwrite=!tight, ask, paste("working dir:\n",   G$work))
        existMake.dd('', overwrite=!tight, ask, paste("downloads dir:\n", G$downdir))
    }
    
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
                             work=work, downdir=downdir, tight=TRUE,  
                             ndown=ndown, bundle=bundle, ask=ask, alttex=alttex,
                             what=what,
                             reset =FALSE, # this will keep G$builds status 
                             deb=deb, gitsim=gitsim)
        
    
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
    existMake.dd("rlibs", overwrite=overwrite, ask=FALSE, "packages dir:")
    packs <- getDeps.format(G$packlist)    
    for(pack in unique(packs)) # Loop over packs and download them 
        download.nice(cran.geturl(pack), makePath("rlibs", pack), overwrite,
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

    ## Download Melpa name/version 
    melpa.getvers()    

    cback <- function(){
        dir <- web.greplink("emacs-../", pos=-1, G$emacsurl, abs=TRUE)
        zip <- web.greplink(G$emacs.type, pos=-1, dir)
        p0(dir, zip)
    }
    download.nice(cback, G$emacszip, overwrite,
                  "Emacs files")
    
    ## ESS
    url <- melpa.getpak("ess.tar")
    download.nice(url, G$esszip, overwrite,
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

    ## Download TinyTeX distro
    download.nice(G$texurl, G$texzip , overwrite, "TinyTeX")

    ## Pandoc        
    cback <- function() {
      json <- rawToChar(curl::curl_fetch_memory(G$panurl) $content)
      lnks <- regmatches(json, gregexpr("\"browser_download_url\":\"[^\"]+", json))[[1]]
      regmatches(lnks, regexpr("https://.+windows.+zip$", lnks))      
    }
    download.nice(cback, G$panzip, overwrite, "Pandoc")
    
}


### Expand components
expand <- function(){
    
    if(!was.core())                expand.core()
    if(is.bremacs() && !was.lab()) expand.lab()
    if(is.studio())                expand.studio()

}


expand.core <- function(){
    
    ## Peazip
    uzip(G$pzip, G$pzip, "Peazip binaries")
    
    ## innoextract
    uzip(G$innozip, G$innozip, "innoextract binaries")
    
    ## R files
    innoextract(G$rzip, G$rzip, "R files")

    ## NSIS files
    uzip.7z(G$nsiszip, G$nsiszip, "NSIS files")
    
    ## Eikon
    uzip(G$eikonzip, G$eikonzip, "Eikon binaries")
    
    ## CRAN packages
    message('\nExpanding packages', '...')
    from <- "rlibs"
    del.path("rlibs")
    ## Loop and extract packs
    for(pack in  dir(down.pt(from)))
        uzip(makePath('rlibs', pack), 'rlibs',
              paste('R package', pack), delTarget=FALSE)    
    
    ## ahkscript
    uzip(G$ahkzip, G$ahkzip, "ahkscript")

}

expand.lab <- function(){
### Expand BloomR Lab

    uzip(G$emacszip, G$emacszip, "BRemacs files")

    utar(G$esszip, G$esszip, "ESS")

    uzip(G$markzip, G$markzip, "Markdown mode")
    
    uzip(G$polyzip, G$polyzip, "Polymode")

    uzip(G$polymarkzip, G$polymarkzip, "Polymode for markdown")

    uzip(G$nowebzip, G$nowebzip, "Polymode for noweb")

    uzip(G$polyrzip, G$polyrzip, "Polymode for R")
    
    uzip(G$bmzip, G$bmzip, "BM mode")
}
 
expand.studio <- function(){

    ## Expand TinyTeX
    uzip(G$texzip, G$texzip, "TinyTeX distro")
    
    ## Expand Pandoc
    uzip(G$panzip, G$panzip, "Pandoc binaries")

}


bloomrTree <- function(){
### Make BloomR directory tree

### Core components are needed by Lab and Studio too, and Lab's by Studio too.
### However, to save space/time Lab and Studio share a common `brEmacs' branch dir,
### i.e., in a multi-build, Studio recycles previous Lab `brEmacs' dir rather than build a new dir from scratch

    switch(get.edition(),

           core= bloomrTree.Core(),

           lab= {
               bloomrTree.Core()
               bloomrTree.brEmacs()               
           },
    
           studio={

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
           })
}

bloomrTree.AddVersion <- function() { # Add and possible relpace version file (ver, build, edition) to tree
### Studio will replace the Lab version file, Lab will replace the Core file.

    download.git("curver.txt",  "curver.txt") 
    ver <- file.read("curver.txt")[1]
    edt <- paste(get.edition(), "edition")
    build <-  makeBuildnum()
    file.write(p0(ver, "\n", build, "\n", edt), app.pt("bloomr.txt"))    
}

bloomrTree.Core <- function() {
### Make BloomR Core tree
### All editions contain Core edition files, so  this function output differs only in the name given to the build folder, which is
### G$branch, that is "brCore" or "brEmacs" (for non-Core editions)
    
    message("\nCreating BloomR tree")
    desc <- if(is.core()) "BloomR Core" else "Common BloomR Lab/Studio"
    existMake(G$branch, TRUE, FALSE, p0(desc, " root dir:"))
    makeDir(app.pt(), "BloomR app dir:")

    ## Add version file
    bloomrTree.AddVersion()
    # download.git("curver.txt",  app.pt("bloomr.txt")) 
    # download.git("curver.txt",  "curver.txt") 
    # ver <- file.read("curver.txt")[1]
    # file.write(p0(ver, "\n", makeBuildnum()), app.pt("bloomr.txt"))
    
    ## Copy R and make site directory
    from <- p0(G$rzip , '/app')
    to <- app.pt("R")
    copy.dir(from, to, "main R files")
    makeDir(app.pt('R/site-library'), "BloomR library:")
    
    ## Copy libs
    message("\nAdding R libraries")
    lib.from <- 'rlibs'
    lib.to <- app.pt("R/library")
    for(lib in dir(work.pt(lib.from))){
        message("Adding ", lib)
        from <- makePath(lib.from, lib)  
        to <- makePath(lib.to, lib)
        copy.dir(from, to)
    }

    ## Install Eikon package
    message("Install Eikon API")
    ## In Windows R CMD invokes cmd.exe, which is incompatible with a UNC build workdir.
    ## So we'll use a local workdir when invoking it and absolute paths  
    exe <- winwork.pt(app.pt('R/bin/R.exe'))
    from <-  winwork.pt(p0(G$eikonzip,'/eikonapir-master'))
    to <-  winwork.pt(app.pt('R/library'))
    to <- p0("--library=", to)
    cmd <- c(exe, "--no-site-file --no-environ --no-save --no-restore --quiet CMD INSTALL", from, to)
    shell.cd(cmd, wd="c:") # In Windows cmd.exe is invoked, so we need to set wd to a non-UNC path 

    ## Install BloomR infrastructure packages
    message("Install BloomR infrastructure packages")
    existMake("rlibs-bloomr", TRUE, FALSE, "BloomR infrastructure packages dir:")
    exe <- winwork.pt(app.pt('R/bin/R.exe'))
    to <-  winwork.pt(app.pt('R/library'))
    to <- p0("--library=", to)    
    for(pack in G$bloomrpacks) { # Loop over packs and download them
        pack.relpath <- p0("res/", pack)
        download.git(pack.relpath, "rlibs-bloomr")
        from <- winwork.pt(makePath(work.pt("rlibs-bloomr"), pack))       
        cmd <- c(exe, "--no-site-file --no-environ --no-save --no-restore --quiet CMD INSTALL", from, to)
        shell.cd(cmd, wd="c:") # In Windows cmd.exe is invoked, so we need to set wd to a non-UNC path
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
    download.git("src/bloomr.time.html",      root.pt("help/bloomr.time.html"))
    download.git("src/bloomr.time.pdf",       root.pt("help/bloomr.time.pdf"))
    
    ## These docs are not uploaded on git, so they are added only with gitsim != FALSE
    if(nzchar(G$github.local)){
        download.git("temp-help/BloomR.pdf",               root.pt("help/BloomR.pdf"))     
        download.git("temp-help/frontier.pdf",             root.pt("help/frontier.pdf"))     
        download.git("temp-help/Bloomberg API Intro.pdf",  root.pt("help/Bloomberg API Intro.pdf"))
        unlink(root.pt("help/bloomr.html"))
        download.git("temp-help/elearnr.pdf",               root.pt("help/elearnr.pdf"))     
        download.git("temp-help/pcloudr.pdf",               root.pt("help/pcloudr.pdf"))     
        download.git("temp-help/secretR.pdf",               root.pt("help/secretR.pdf")) 
    }
    
    ## Environment diagnostic
    message("\nAdding ED tools")
    makeDir(app.pt('ed'), "ED tools:")
    download.git("src/ed/bloomr.ed.cmd",  app.pt("ed/bloomr.ed.cmd"))

}

bloomrTree.brEmacs <- function() {
### Make BRemacs directory tree

    bremacs <- app.pt("bremacs")
    existMake(bremacs, TRUE, FALSE, "BRemacs tree")

    ## Replace version file
    bloomrTree.AddVersion()
    
    ## Copy Emacs
    message("Copying main BRemacs files")
    from <- makePath(G$emacszip, "emacs-*")    
    dirs <- c('bin', 'lib', 'libexec', 'share/emacs', 'share/icons', 'share/info', 'share/man')
    copy.glob(from, bremacs, dirs)
    
    
    ## Copy ESS
    message("Adding ESS files")
    from <- G$esszip
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
    from <- G$markzip
    from <- makePath(from, 'markdown-mode-master')
    to <- slisp.pt(G$markzip)
    makeDir(to)
    copy.glob(from, to, "*.el")
    
    ## Copy Polymode
    message("Adding Polymode files")
    from <- G$polyzip
    from <- globpaths(from, '/polymode*')    
    to <- slisp.pt(G$polyzip)
    makeDir(to)
    copy.glob(from, to, "*.el")

    ## Copy Poly-markdown mode
    message("Adding Poly-markdown files")
    from <- G$polymarkzip
    from <- globpaths(from, '/poly-markdown*')
    to <- slisp.pt(G$polymarkzip)
    makeDir(to)
    copy.glob(from, to, "*.el")

    ## Copy Poly-noweb mode
    message("Adding Poly-noweb files")
    from <- G$nowebzip
    from <- globpaths(from, '/poly-noweb*')
    to <- slisp.pt(G$nowebzip)
    makeDir(to)
    copy.glob(from, to, "*.el")
   
    ## Copy Poly-R mode
    message("Adding Poly-R files")
    from <- G$polyrzip
    from <- globpaths(from, '/poly-R*')
    to <- slisp.pt(G$polyrzip)
    makeDir(to)
    copy.glob(from, to, "*.el")
    
    ## Copy BM mode
    from <- p0(G$bmzip, '/bm-master')              
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

    ## Lisp path quote: "path\\to" -> \\\"path\\\\to\\\" 
    l <- function(path) { # <- a quoted windows path  
        x = gsub("\\", "\\\\", normalizePath(path, mustWork = FALSE), fixed=TRUE)
        sprintf("\\\"%s\\\"", x)
    }

    ## runemacs does not wait 
    cmd <-   makePath(bremacs, 'bin/emacs.exe')

    ## Standard Emacs sources are already in lisp-path 
    eldir <- globpaths(bremacs, 'share/emacs/[0-9]*')
    elisp.t <- " 
(progn
  (byte-recompile-directory %s 0)
  (kill-emacs 0))
"
    elisp <- sprintf(elisp.t,  l(work.pt(eldir)))
    args <- sprintf("-batch -Q -eval %s", dquoteu(elisp))
    shell.cd(c(work.pt(cmd), args), evars = c(emacs_dir = NA))   
    ## messagev(c(work.pt(cmd), args)) # To test in PS replace eval "..." with '...'

    ## Site-lisp sources need adding their directories to lisp-path, we also create autoloads
    eldir <- globpaths(bremacs, 'share/emacs/site-lisp')
    elisp.t <-"
(let* ((site-lisp %s)
       (default-directory site-lisp)
       (pkg-name) (pkg-dir) (pkg-autoload-buf)
       (pkg-autoload-path) (pkg-autoload-basename))

  ;; generate elc in all site-lisp subdirs
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

    elisp <- sprintf(elisp.t,  l(work.pt(eldir)))
    args <- sprintf("-Q -eval %s", dquoteu(elisp))
    shell.cd(c(work.pt(cmd), args), evars = c(emacs_dir = NA))
    ## messagev(c(work.pt(cmd), args)) # To test in PS replace eval "..." with '...'


    ## Set the edition if this is the actual Lab
    #if(is.lab()){ # No, if Studio is building common BRemacs
    #    ## ver <- file.read(app.pt("bloomr.txt"))
    #    edt <- paste(get.edition(), "edition")
    #    ## file.write(p0(ver, "\n", edt), app.pt("bloomr.txt"))
    #    file.write(edt, app.pt("bloomr.txt"), append=TRUE)
    #}
    
}


bloomrTree.Studio <- function(){
### We add the to the tree created by bloomrTree.brEmacs() the LaTeX related component

    ## Replace version file
    bloomrTree.AddVersion()
    
    makeStudio.addLatex()
    ## makeStudio.addPerl()
    makeStudio.addPandoc()

    ## Set the edition
    # ver <- file.read(app.pt("bloomr.txt"))
    # edt <- paste(get.edition(), "edition")
    # file.write(p0(ver, "\n", edt), app.pt("bloomr.txt"))
    # file.write(edt, app.pt("bloomr.txt"), append=TRUE)
}


makeStudio.addLatex <- function() {
### Install LaTeX. Currently TinyTex

    
    ## Copy TinyTeX distro
    message("Adding LaTeX files")
    copy.glob(G$texzip, app.pt(), "tinytex")
    
    ## TeXLive installer path
    tlmgr <- makePath(app.pt(G$texzip), 'bin/windows/tlmgr.bat')
    if(!file.exists(work.pt(tlmgr))) stop("Unable to find executable:\n ", work.pt(tlmgr))
    tlmgr <- pswork.pt(tlmgr)
    
    ## Set xetex fonts dir to local
    ## Check before and after with: texmf-var/fonts/conf/fonts.conf 
    cmd <- paste(tlmgr, "postaction install script xetex")
    shell.ps(cmd, winwork.pt("ps.tinytex.txt"))

    ## Do not wrap short (80 columns) log file lines
    cmd <- paste(tlmgr, "conf texmf max_print_line 10000")
    shell.ps(cmd, winwork.pt("ps.tinytex.txt"))

    ## Set repo for updates to nearby
    cmd <- paste(tlmgr, "option repository 'ctan'")
    shell.ps(cmd, winwork.pt("ps.tinytex.txt"))
        
    return()

    ## Needed to build the PDF of R packages 
    cmd <- paste(tlmgr, "install makeindex")
    shell.ps(cmd, winwork.pt("ps.tinytex.txt"))




    ### ==============
    ## More packs for minimal Rnw: palatino breakurl fpl mathpazo
    morepaks <- c("palatino", "breakurl", "fpl", "mathpazo")
    
    rscript <- makePath(app.pt("R"), 'bin/Rscript.exe')
    rscript <- winwork.pt(rscript)
    
    args <- "library('tinytex')" 
    tinytex.dir <- work.pt(app.pt("tinytex"))
    ## Powershell call operator doesnt work with double quotes and backslash in shQuote(). This is a temp fix:
    usquote <- function(path) squoteu(normalizePath(path, winslash = "/", mustWork = FALSE))    
    tinytex.dir.sh <- usquote(tinytex.dir)
    
    morepaks <- paste(sapply(morepaks, squoteu), collapse = ', ')
    args <- c(args,
              sprintf(
                  "install_tinytex(dir = %s, version = 'daily', add_path = FALSE, extra_packages = c(%s))",
                  tinytex.dir.sh, morepaks))
                  
    ## Find tlmgr for r_texmf
    tlmgr <- makePath(tinytex.dir, "bin/windows/tlmgr.bat")
    tlmgr <- usquote(tlmgr)
    args <- c(args,
               sprintf("options(tinytex.tlmgr.path = %s)", tlmgr))
            
    ## Remove hard path. The alternative could be to copy files to texmf-local
    args <- c(args, "r_texmf('remove')")    

    message("Executing:")
    message(rscript)
    messagev(args, s = "\n")

    qargs <- shQuote(pv(args, s="; "))
    cmd <- paste(rscript, "-e", qargs)
    shell.ps(cmd, winwork.pt("ps.tinytex.txt"))
    

    ## Old MikTeX paks
    toinstall <- "upquote microtype parskip kvoptions ltxcmds kvsetkeys xurl bookmark infwarerr kvdefinekeys
                  pdfescape hycolor letltxmacro auxhook intcalc etexcmds bitset bigintcalc rerunfilecheck
                  uniquecounter geometry fancyvrb framed booktabs footnotehyper refcount gettitlestring"
    toinstall <- paste(toinstall, "pdfcrop") # required by intermediate commands  

}

makeStudio.addPandoc <- function(){ # NOT USED

    ## Pandoc variables (msi wants abs path)
    pandir <- app.pt("pandoc")
    
    ## Create Pandoc dir
    existMake(pandir, TRUE, FALSE, "Pandoc dir:")

    from <- Sys.glob(work.pt(p0(G$panzip,'/pandoc*')))
    file.rename(from, work.pt(makePath(pandir, "/bin")))
    
}

makeStudio.addPerl <- function(){
    
    message("Making Perl tiny")
    
    perlsource <- G$perlzip
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

    ## Test deb/what mismatch 
    debug.mismatch()
    
    initScripts.etc()

    ## Make R bootstrapper launcher
    if(is.core())    makeLauncher.Core()

    ## brEmacs editons include also the Core edition, thus the launcher    
    if(is.bremacs()) {
        makeLauncher.Core()
        makeLauncher.brEmacs()
    }    
}

initScripts.etc <- function() {
       

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
    ## download.git("src/bloomr.beta.R", app.pt("R/share/bloomr/bloomr.beta.R"))
    ## download.git("src/bloomr.api.R",  app.pt("R/share/bloomr/bloomr.api.R"))
    download.git("src/bloomr.R",      app.pt("R/share/bloomr/bloomr.R"))
    download.git("src/bloomr.sys.R",  app.pt("R/share/bloomr/bloomr.sys.R"))
    download.git("src/bloomr.test.R", app.pt("R/site-library/bloomr.test.R"))
    download.git("src/bloomr.time.R", app.pt("R/share/bloomr/bloomr.time.R"))
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
EnvSet,  vanilla,    true
Run, %AppDir%\\R\\bin\\x64\\Rgui.exe LANGUAGE=en
"
    bloomr.run <- gsub("%AppDir%", G$appname, bloomr.run)
    makeLauncher_(bloomr.run, "core")
}

makeLauncher.brEmacs <- function(){
### Make launcher of Lab and Studio editions



    bremacs.run <-  "
EnvSet,  BLOOMR,     %A_ScriptDir%
EnvSet,  HOME,       %A_ScriptDir%\\apps\\bremacs
Run, %AppDir%\\bremacs\\bin\\runemacs.exe -q --no-splash
"        
    bremacs.run <- gsub("%AppDir%", G$appname, bremacs.run)
    makeLauncher_(bremacs.run, "bremacs")
}


makeLauncher_ <- function(script.cont, edition){
   
    ## Make boot file
    ahkdir <- work.pt(p0(G$ahkzip, '/Compiler'))
    cat(script.cont, file=makePath(ahkdir, p0(edition, ".run")))
   
    ## Get icon from GitHub
    icon <- if(edition=="core") "bloomr" else "bremacs" 
    to <- makePath(p0(G$ahkzip, '/Compiler'), p0(edition, ".ico"))
    download.git(makePath("res", p0(icon, ".ico")), to)

    ## Core icon is currently as setup icon too. It is named bloomr.ico, should it change style in the future
    from <- makePath(ahkdir, "core.ico")
    to <- makePath(ahkdir, "bloomr.ico")
    if(edition =="core")  file.copy(from, to, overwrite=TRUE) 

    exename <- switch(
        edition,
        
        core="bloomr-core.exe",
        
        bremacs = switch(
            get.edition(),
                         
            lab =  "bloomr-lab.exe",
            
            studio = {
                ## if it a multi build, then we have the Lab launcher too, to delete
                (function(x) if(file.exists(x)) file.remove(x))(work.pt(root.pt("bloomr-lab.exe")))
                "bloomr-studio.exe"
            }
        )
    )
    
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
    to <- work.pt(root.pt(exename))
    file.rename(from, to)        
}

PROF <- function(){ #Keep this on separate line
### "etc/Rprofile.site" source (braces on separate lines) 
    
    ## BloomR bootstrap
    ## ================
    
    source(paste0(R.home("share"), "/bloomr/bloomr.init.R"))
    
    
    ## end BloomR----------
}



### Exe and Zip distro
makeBundle <- function(bundle, # 'exe'/'zip'/'all'/'none'
                    ask
                    ){
### Create create exe, zip, both, or nothing   
### Every bundle value not 'exe'/'zip'/'all' means 'none'

    ## Test deb/what mismatch 
    debug.mismatch()
    
    if(bundle=='all' || bundle=='exe') makeInst(ask)
    if(bundle=='all' || bundle=='zip') makeZip(ask)    
}


makeInst <- function(ask){ # not used since long

    message('\nCreating BloomR green installer')        
    ## Set name (nsi name is "bloomr-setup.exe")    
    to <- switch(get.edition(), 
                 core = "BloomR-Core_setup_.exe",
                 lab = "BloomR-Lab_setup_.exe", 
                 studio = "BloomR-Studio_setup_.exe")
    
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)
    download.git("bloomr.nsi", "bloomr.nsi")
    message('Creating green installer ', to, '\nThis may take a bit...')
    nsi <- 'bloomr.nsi'
    nexe <- p0(G$nsiszip,'/App/NSIS/makensis.exe')
    nsrc <- p0("/dSRCDIR=", G$branch)
    cmd <- c(winwork.pt(nexe), "/v2", nsrc, winwork.pt(nsi))
    shell.cd(cmd)
    file.rename(work.pt("bloomr-setup.exe"), work.pt(to))
}


makeZip <- function(ask){

    message('\nCreating BloomR.zip')
    to <- switch(get.edition(), 
                 core = "BloomR-Core_setup_.zip",
                 lab = "BloomR-Lab_setup_.zip",
                 studio = "BloomR-Studio_setup_.zip")
    
    if(is.path(to)) del.ask(to, ask, "already exists")
    
    del.path(to)    
    from <- p0('./', G$branch, '/*')  # In 7z ./ removes dir prefix from archive files
    zexe <- get7zbin()
    cmd <- paste(winwork.pt(zexe), "a", winwork.pt(to), winwork.pt(from))
    ret <- system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}

makeBuildnum <- function(){

    ## today
    td <- format(Sys.time(), "Build %Y%m%d%H%M")

    # days from 2015 
    diff <- as.numeric( as.POSIXct(Sys.time()) - as.POSIXct("2015/01/01") )
    rnd <- round(diff, 2) * 100  # About 13 min rounding  
    paste(td, rnd)
    
}

debug.mismatch <- function(){ # Test and stop on deb & what args inconsistence
### When you use step by step build via 'deb'", this function checks that 'what' matches previous steps 

    wmess <- 
"\nEdition Mismatch!

You are building %s, but %s refers to %s.
Perhaps you are using a step by step build via 'deb' and 'what' does not match,
or you are using 'deb' with what='all'."

    ver <- file.read(app.pt("bloomr.txt"))
    edt.file <- unlist(regmatches(ver, regexec("(core|lab|studio) +edition", ver, ignore.case = TRUE)))[2]
    edt.file <- paste(sub("(.)", "\\U\\1", edt.file,      perl=TRUE), "Edition")
    edt.code <- paste(sub("(.)", "\\U\\1", get.edition(), perl=TRUE), "Edition")
    if(edt.code == edt.file) return()
    
    wmess <- sprintf(wmess, edt.code, app.pt("bloomr.txt"), edt.file)

    stop(wmess)

}


### ===============
### == Utilities ==
### ===============

### === Shell helpers ===

shell.cd <- function(
### Similar to system2(), but can set the work dir and stops on errors
                     cmdvec,     # c(cmd, arg1,...) or c(cmd, agvec)
                                 # cmdvec[1] is tested for exisitance and shQuoted if necessary
                     wd=NULL,    # optional work dir
                     raw=FALSE,  # if T, do not pretty format the output with pv
                     echo=TRUE,  # Echo to console via messagev
                     evars=NULL  # Named vector or list of environment variables to set (`NA` value to unset)
                     ) {

    
## 1. system(), takes a single cmd string, in Linux prefixes with "sh -c"
## 2. system2() wants cmd + args. In Linux wraps system() adding "2>&1" if stderr = T.
## 3. shell(), Windows-only, wraps system() and prefixes the command string with "cmd.exe /c".  
## Don't invoke cmd.exe (so shell()) as it breaks on UNC paths. Prefer PowerShell, direct calls to exes or R builtins
## The best seems x <- suppressWarnings(system2(cmd, ags, stdout=T, stderr=T)); attr(x, "status")
## In Linux also suppressWarnings(tryCatch(system(paste(cmdstring, "2>&1"), intern=TRUE)))


    ## Set temporary wd
    oldwd <- getwd()
    if(!is.null(wd)) setwd(wd)

    ## Test command exists
    cmdnq <- gsub("^[\"']|[\"']$", "", cmdvec[1]) # unquote (if shQuote was used)
    cmdnq <- normalizePath(cmdnq) # Sys.which and system2 do not work properly over Windows shares 
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

    ## Environment variables    
    if(!is.null(evars)) {
        if(any(!nzchar(names(evars)))) stop("In 'shell.cd(evars = ...)' not all elements are named")
        evars <- as.list(evars)
        is.na <- sapply(evars, is.na)
        evars[! is.na] -> setv
        evars[is.na]   -> unsetv 

        old.vars <- as.list(Sys.getenv(names(evars), unset = NA, names = TRUE))
        is.na <- sapply(old.vars, is.na)
        old.vars[! is.na] -> old.setv
        old.vars[is.na]   -> old.unsetv

        ## Set
        if(length(setv)) do.call("Sys.setenv", setv)
        Sys.unsetenv(names(unsetv))

        ## Unset
        on.exit({
            if(length(old.setv)) do.call("Sys.setenv", old.setv)
            Sys.unsetenv(names(old.unsetv))
        })
    }

    ## Execute
    out <- suppressWarnings(system2(cmdnq, cmdvec[-1], stdout=TRUE, stderr=TRUE))    
    setwd(oldwd)
    G$lastshell <- list(cmdvec=cmdvec, output=out)

    ## Stop on error ...
    if(!is.null(attr(out,"status")))
        stop(paste(cmdvec[-1], collapse=" "), "\n returned:\n", tail(out, 2), "\n\nSee G$lastshell for more.")

    ## or return the required ouptut
    if(!raw) out <- pv(out, s='\n')
    if(echo) messagev(out)
    out
}

shell.ps <- function(
### Similar to shell(), but based on Powershell. tees stdout to file and console and stops on errors
                     cmdstr,    # command string
                     outfile,   # optional work dir
                     stop=TRUE  # stop on errors
                      ) {

    ## $LASTEXITCODE is for executables and "-not $?" is for cmdlets
    cmdstr.tee <- sprintf("& %s | Tee-Object %s; exit ($LASTEXITCODE -le 1 -and -not $?)", cmdstr, outfile)
    ## Quotes in cmdstr.tee, before passing to PS, are removed so we quote again
    psline <- paste("powershell -NoProfile -command", shQuote(cmdstr.tee))

    ## system doesn't like the UNC paths, which we do
    old.path <- setwd(Sys.getenv("TMP"))
    on.exit(setwd(old.path))    
    ret <- system(psline)
    if(ret) {
        msg <- sprintf(
            "The following PowerShell command had a nonzero exit:\n  %s \nFor the error log see:\n  %s",
            cmdstr, outfile)
        if(stop) stop(msg) else message(msg)
    }
    ret
}

shell.ps.alt <- function( # Not used 
### Similar to shell.ps(), but exit code works for cmdlets -not $?
                     cmdstr,    # command string
                     outfile    # output log file
                      ){

    cmdstr.tee <- sprintf("%s | Tee-Object %s; exit -not $?", cmdstr, outfile)
    psline <- paste("powershell -NoProfile -command", shQuote(cmdstr.tee))
    ret <- system(psline)
    if(ret){
        msg <- sprintf(
            "The following PowerShell command had a nonzero exit:\n  %s \nFor the error log see:\n  %s",
            cmdstr, outfile)
        if(stop) stop(msg) else warning(msg)
    }
    ret
}

### === Website helpers ===


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

cran.geturl <- function(pack){ # Get CRAN Windows binaries relative to current release for a package
### Make sure the R release is current too  

    ## CRAN links
    cranpage <- "https://cran.r-project.org/web/packages/"
    cranwin <- "https://cran.r-project.org/bin/windows/"

    ## Get package page 
    url <- p0(cranpage, pack, "/index.html")
    page <- download.html(url)

    ## Get bin url: the first occurence on page is the dev version, second is release
    url <- regmatches(page, gregexpr("../../../bin/windows/contrib/.*?\\.zip", page))[[1]][2]
    url <- regmatches(url, regexec("contrib/.*?\\.zip", url))   
    p0(cranwin, url)
}

melpa.getvers <- function(){ # Store MELPA packages names/version as a data frame in G$melpa.vers

    ## Get JSON package list and vectorise it
    melpa.json <- rawToChar(curl_fetch_memory("https://melpa.org/archive.json")$content)
    melpa.json <- substr(melpa.json, 2, nchar(melpa.json))
    melpa.vec <- strsplit(melpa.json, "}},", fixed = TRUE)[[1]]

    ## Extract only package name and version as a data frame 
    melpa.vers <- strsplit(melpa.vec, ":")
    melpa.vers <- t(sapply(melpa.vers, `[`, c(1,3)))
    x <- melpa.vers[,2]
    x <- sub("\\[", "", regmatches(x, regexpr("\\[[^]]*", x)))
    x <- sub(",", ".", x)
    melpa.vers[, 2] <- x
    G$melpalibs <- setNames(as.data.frame(melpa.vers), c("name", "version"))
}

melpa.getpak <- function(pakname.ext){ # Build a package from G$melpa.vers given its name and extension

    pakname <- tools:::file_path_sans_ext(pakname.ext)
    qpakname <- dquoteu(pakname)
    extension <- tools:::file_ext(pakname.ext)
    
    pos <- grep(qpakname, G$melpalibs$name, fixed=TRUE)
    version  <- G$melpalibs$version[pos]
    sprintf("https://melpa.org/packages/%s-%s.%s", pakname, version, extension)
  
}



### === Donwload helpers ===


download.nice <- function(from, to, overwrite, desc=""){
### Download relative to the build workdir with nice user info and overwrite management

    if(desc=="") desc <- sub('.+/', '', to)
    to <- down.pt(to)
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
        #to.temp <- basename(to)
        to.temp <- basename(tempfile(pattern = basename(to), tmpdir = down.pt()))
        download.nice(from, to.temp, overwrite=TRUE, desc)
        file.rename(down.pt(to.temp), work.pt(to))
        
    ## local git
    } else {
        messagev('\nObtaining', desc, "\nfrom ", G$github.local)
        from <- makePath(G$github.local, file)  # not relative to the build workdir 
        if(!is.path_(from)) stop(from, "\nnot found in local Git repo:\n", G$github.local)
        to <- work.pt(to)
        file.copy(from, to, overwrite=TRUE) 
    }

}



### === File System ===
makePath <- function(parent, child){    
### Chain parent-child paths managing middle slashes ('/')
## You don't have to remember if you need to add or not that slash

    ## No trail for parent 
    parent <- sub('/$', '',  parent)
    parent <- sub('\\\\$', '',  parent)

    ## No lead for child
    child <- sub('^/', '',  child)
    child <- sub('^\\\\', '',  child)

    ## Now file.path inputs is secure
    file.path(parent, child)

}

work.pt <- function(path=""){    
### Prefix path with the build workdir
### The build workdir is by global G$work

    if(is.null(G$work)) stop("`G$work' build workdir is not set!") 
    if(is.abspath(path))
        stop("The path you provided:\n", path, "\nis absolute, but it should be relative to:\n", G$work)
    makePath(G$work, path)
}

down.pt <- function(path=""){    
### Prefix path with downloads directory 
### The downloads directory is set by global G$downdir
    
    if(is.null(G$downdir)) stop("`G$downdir' downloads directory is not set!")
    if(is.abspath(path))
        stop("The path you provided:\n", path, "\nis absolute, but it should be relative to:\n", G$downdir)
    makePath(G$downdir, path)
}

root.pt <- function(path=""){
### Prefix path with current branch name, e.g 'brStudio'
    makePath(G$branch, path)
}


app.pt <- function(path=""){
### Prefix path with BloomR app path relative to the build workdir.
### The app directory is below the branch directory and its name is set by G$appname, e.g.: brStutio/programs

    x <- root.pt(G$appname)
    makePath(x, path)
}

slisp.pt <- function(dir=""){
### Prefix dir path with BRemacs site-lisp path relative to the build workdir.
    
    x <- app.pt("bremacs/share/emacs/site-lisp")
    makePath(x, dir)
}



winwork.pt <- function(path){
### Convert a Unix path, relative to the build workdir, to a shell-quoted absolute Windows path. 

    path <- work.pt(path)            
    path <- normalizePath(path, mustWork=FALSE)
    shQuote(path)
}

windown.pt <- function(path){
### Convert a Unix path, relative to the downloads dir, to a shell-quoted absolute Windows path. 

    path <- down.pt(path)            
    path <- normalizePath(path, mustWork=FALSE)
    shQuote(path)
}

pswork.pt <- function(path){
### Like winwork.pt, but use single quotes, convenient for PowerShell

    path <- work.pt(path)            
    path <- normalizePath(path, mustWork=FALSE)
    squoteu(path)
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


chk.colon <- function(path){ # Test a path does not end with a colon
    if(grepl(":$", path))
        stop("In ", path, "\n ending with a colon (:) is ambiguous and not allowed as a working directory.",
             "\nFor a USB drive root append a slash to it.")
}


chk.write <- function(path, over, desc="", stop=TRUE){ # Not Used!!
### Check if we can overwrite non-empty dir and possibly stop
### Empty dir are overwritten without prompt
### File is considered a non-empty dir
     
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
        message("It will be deleted ...\n")
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
                     children, # with globbing
                     both=TRUE # return only resolved child, without parent, or both 
                     ){

    unlist(
        sapply(children, function(child) globpath_(parent, child, both=both), 
               USE.NAMES = FALSE))
}

globpath_ <- function( # globpaths() work horse, accepting a scalar child
### Return parent/child, resolving globs (not regex). Both dirs should refer to the build workdir, parent should expand to a unique path.
                      
                     parent,   # with globbing, but its expansion gives a single path 
                     child,    # with globbing
                     both=TRUE # return only resolved child, without parent, or both 
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

existMake.old <- function(dir, overwrite, ask, desc=""){
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
    else if(!overwrite) message("Skipping action, due to 'tight = TRUE' argument!")
    ## Dirty, but overwritable 
    else {
        del.ask(dir, ask, "exists non-empty")
        del.path(dir)
        makeDir(dir)
    }             
}


existMake <- function(dir, overwrite, ask, desc=""){
### If dir (relative to the build workdir) does not exist make it, otherwise might ask and maybe skip creation.
### An empty-dir is considered non-existent. Note: if dir="", it is the build workdir.

    existMake_(dir, overwrite=overwrite, ask=ask, desc=desc, isdown=FALSE)    
}

existMake.dd <- function(dir, overwrite, ask, desc=""){
### If dir (relative to the downloads dir) does not exist, make it, otherwise might ask and skip creation.
### An empty-dir is considered non-existent. Note: if dir="", it is the downloads dir.

    existMake_(dir, overwrite=overwrite, ask=ask, desc=desc, isdown=TRUE)
    
  # wouldbe symlinks linka are fs specific,  better to avoid 
  #  ## https://superuser.com/questions/1307360/how-do-you-create-a-new-symlink-in-windows-10-using-powershell-not-mklink-exe
  #  if(!is.subdir_gen(G$downdir, G$work)){  
  #      mklink  <- sprintf("new-item -itemtype symboliclink -path %s -name 'downloads.lnk' -Target %s",
  #                         shQuote(G$work), shQuote(G$downdir))
  #      psline <- paste("powershell -NoProfile -command", shQuote(mklink))
  #      ret <- system(psline) 
  #      if(ret) stop("Creating a symlink to the downloads dir inside ", G$work, " was not possible.\n",
  #                   "Note that in old Windows versions (before circa 2017), you need admin privileges to create symlinks")
  #  }

}

existMake_ <- function(dir, overwrite, ask, desc="", isdown=FALSE){
### This is the workhorse for 'existMake', if isdown=FALSE, or 'existMake.dd', isdown=TRUE.
    
    ## Inform user with desc if any
    if(nzchar(desc)) message("\nCreating ", desc, '\n',  dir)

    ## Who are we serving?
    dir <- if(isdown) down.pt(dir) else work.pt(dir)
    dir <- normalizePath(dir, mustWork = FALSE)

    emsg <- paste("\nUnable to access/create\n", dir)
    exist    <- function() length(Sys.glob(dir)) # even if it is a file
    nondirty <- function() !length(Sys.glob(makePath(dir, "/*"))) # inexistent or empty
    delete   <- function() unlink(dir, recursive=TRUE, force=TRUE) # missing's not a failure
    make     <- function() dir.create(dir)
    isborn   <- function() if(!exist())  stop(emsg)
    isdead   <- function() if(exist()) {
                               Sys.sleep(2)
                               if(exist()) stop(emsg)
                           }
    ## delete-check-make-check
    dcmc <- function(){
        delete(); isdead(); make(); isborn()
    }
   
    ## Not a dirty dir
    if(nondirty()) dcmc()
           
    ## Dirty and not overwritable 
    else if(!overwrite) message("Skipping action, due to 'tight = TRUE' argument!")

    ## Dirty but overwritable 
    else{
        del.ask.abs_(dir, ask, "exists non-empty") # ask if required         
        dcmc()
    }    
}

is.subdir_gen <- function(sub, par){
### Is sub a subdir of par? Paths are absolute or getwd()-relative, rather than G$work

    par <- normalizePath(par, mustWork = FALSE)
    sub <- normalizePath(sub, mustWork = FALSE)
    
    prefix <- substr(par, 1, nchar(sub))
    prefix == sub
    
}



file.read <- function( # Read file  
                   fpath # File path relative to the build workdir
                   )    
    readLines(work.pt(fpath))

file.write <- function( # Write file
                       text,
                       fpath, # File path relative to the build workdir
                       append =FALSE
                   )    
    write(text, work.pt(fpath), append = append)


### === UNC Paths ===  NOT USED any more 

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

### === Extaction utils ===
uzip <- function(from, to, desc, delTarget=TRUE){
### Unzip FROM and TO relative resp. to the downloads dir and the build workdir
### Stops on errors and inform used with a desc argument

    message('\nExpanding ', desc, '...')
    if(!file.exists(down.pt(from))) stop("Cannot find the file\n", down.pt(from))
    if(delTarget) del.path(to)
    uzip_(from, to)
    
}

uzip_ <- function(from, to){ # uzip workhorse    

    from <- down.pt(from)
    to <- work.pt(to)
    if(length(unzip(from, exdir= to))==0)
        stop('\nUnable to perform extraction')  
}

utar <- function(from, to, desc, delTarget=TRUE) {
### Untar making path relative to the build workdir
### Stops on errors and inform used with a desc argument

    message('\nExpanding ', desc, '...') 
    if(!file.exists(down.pt(from))) stop("Cannot find the file\n", down.pt(from))
    if(delTarget) del.path(to)
    # utar_(from, to)  # consider remove this 
    
    from <- down.pt(from)
    to <- work.pt(to)
    if(untar(from, exdir= to) != 0)
        stop('\nUnable to perform extraction')    
}

uzip.7z <- function(from, to, desc, delTarget=TRUE){
    zexe <- get7zbin()
    if(is.path(to)) {
        message('\nDeleting exisiting ', desc)
        del.path(to)
    }    
    message('\nExpanding (w/ 7zip) ', desc)
    message('This may take a bit ...')
    cmd <- paste(winwork.pt(to), windown.pt(from))
    cmd <- p0(winwork.pt(zexe), ' x -aoa -r -o', cmd)
    ret <- system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}

get7zbin <- function(){ # Get 7z.exe relative to the build workdir
    zipdir <- G$pzip
    versiondir <- get7zbin.ver_(zipdir)
    subpath <- '/res/bin/7z/7z.exe'
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
    cmd <- c(winwork.pt(exe), windown.pt(from),  "--output-dir", winwork.pt(to))
    ret <- shell.cd(cmd, echo = FALSE)
    # cmd <- paste(winwork.pt(exe), windown.pt(from),  "--output-dir", winwork.pt(to))
    # ret <- system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    # if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}


getInnobin <- function(){ # Get 7z.exe relative to the build workdir
#    innodir <- p0(G$innozip,'.d')
    makePath(G$innozip, "innoextract.exe")
}



### === R dependencies ===

getDeps <- function(pnames){
### Get recursively package depenecies/imports

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


### === Messages ===

warn.path <- function(path, mess){
    message("\n", path, "\n  ", mess)
}

messagev <- function(..., s=" ") { # message() converting argument to a single vector and separating elements with `s'
    ## see also pv()
    message(pv(..., s=s))
}

pv <- function(..., s=" ") { # paste() converting argument to a single vector and separating elements with `s'
   paste(c(...), collapse=s)
}

p0 <- paste0 # no more paste0

warn.p <- function(path, mess){
### Exsisting paths warn
    messagev(" ", mess.p(path, mess))
}

mess.p <- function(path,  mess){
### Exisintg paths
    p0("\n", path, "\n  ", mess)
}

exit.p <- function(path,  mess){ 
### Exisintg paths stop
    stop(mess.p(path,  mess))
}

mess.down <- function(desc){
### Generic download info for custom download
    messagev("\nDownloading", desc)
}

### No fancy quotes errors for shell commands, but usually for paths you want shQuote to escape backslashes
squoteu <- function( # Undirectional sQuote
                    x) sQuote(x, q = FALSE)

dquoteu <- function( # Undirectional dQuote
                    x) dQuote(x, q = FALSE)



