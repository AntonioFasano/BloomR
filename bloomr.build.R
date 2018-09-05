###  BloomR source

##  TODO
##  Compile BRemacs packages (BM) on first run
##  NSIS does not delete an existing dir, but silently overwrites it
##  Repurpose src\bloomr.beta.*
##  Repurpose tests and test data 
##  Change app path from main to programs?
##  Add MikTeX to credit
##  RCurl find and remove references everywhere 
##  Set new polymode version
##
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
##  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
##  peazip from http://sourceforge.net/projects/peazip
##  ahkscript from http://ahkscript.org
##  Nsis from nsis.sourceforge.net
##  innoextract from http://constexpr.org/innoextract
##  Icon set Simplicio CC 3.0 by Neurovit: http://neurovit.deviantart.com
##   retrieved at https://www.iconfinder.com/icons/48750/direction_right_icon
##


### Globals
G=new.env()

### Contribs
## Bloomberg API for icedtea (not used)
G$apiurl="https://bloomberg.bintray.com/BLPAPI-Stable-Generic/blpapi_java_3.8.8.2.zip"
G$apizip="blpapi"

## Eikon
G$eikonurl="https://github.com/ahmedmohamedali/eikonapir/archive/master.zip"
G$eikonzip='eikon'

## Ahkscript
## G$ahkurl="http://ahkscript.org/download/ahk2exe.zip" # removed
G$ahkurl="https://autohotkey.com/download/ahk.zip"
G$ahkzip="ahk"

## Github
G$github="https://raw.githubusercontent.com/AntonioFasano/BloomR/master"
G$github.local="" # Auto-set by makeBloomR() if gitsim=T, relative to workdir 

## Packages to download. Case sensitive
## To learn about deps use:
## x=available.packages(); x["ggplot2","Imports"]; x["ggplot2","Depends"]

pks="knitr Rblpapi xts XML httr rmarkdown RCurl" # RCurl to be removed
pks=paste(pks, "plyr pbapply") # for read read.xlx

### now auto with getDeps.format
### Rblpapi deps
#pks=paste(pks, "Rcpp") 
# 
### RCurl deps
#pks=paste(pks, "bitops")
# 
### Knitr deps
#pks=paste(pks, "evaluate digest markdown yaml highr formatR stringr")
# 
### Stringr deps
#pks=paste(pks, "stringi magrittr")
# 
### Markdown deps
#pks=paste(pks, " mime")
# 
### read.xlx deps
#pks=paste(pks, "plyr pbapply Rcpp")
# 
### Eikon deps
#pks=paste(pks, "httr jsonlite curl openssl R6")

## All packs deps
G$packlist=pks
rm(pks)
    
## Innoextract
G$innourl="http://constexpr.org/innoextract/files"
G$innozip='innoextract'

## R
G$rurl="https://cloud.r-project.org/bin/windows/base/"
G$rzip='rmain'

## SF items
G$pzip="peazip"
G$rport="rportable" # not used
G$nsisurl='portableapps'
G$nsiszip='nsis'
G$emacs='emacsbinw64'
G$emacs.type='Og' # e.g.: emacs-w64-25.1-Og.7z

## ESS, Polymode, Markdown, BM
G$essurl='https://github.com/emacs-ess/ESS/archive/master.zip' # seems to require a Julia lib dep
G$essurl='http://ess.r-project.org/downloads/ess/ess-17.11.zip'
G$esszip='ess'
G$polyurl='https://github.com/vspinu/polymode/archive/master.zip'
G$polyzip='polymode'
G$markurl='https://github.com/jrblevin/markdown-mode/archive/master.zip'
G$markzip='markmode'
G$bmurl='https://github.com/joodland/bm/archive/master.zip'
G$bmzip='bmmode'

## LaTeX & Pandocfor Studio
G$mikurl="http://mirrors.ctan.org/systems/windows/miktex/setup/windows-x86/miktex-portable.exe"
G$mikinst="mikport.exe"
G$panurl = "https://github.com/jgm/pandoc/releases"
G$paninst = "pandoc.msi"

## Local paths
G$work=""
G$appname="main" # BloomR application folder name. Used by app.pt() 
G$branch=NULL #"brCore"

## Arguments
G$bremacs=NULL
G$studio=NULL
G$ndown=0


## Dev style 
## Functions returning/accepting paths normally use paths relative to G$work.
## The latter ones will use a workhorse function with work.pt() if they need to access file system.

makeBloomR=function( # Build BloomR
                    work,         # work dir path, absolute or relative to cur path
                    tight=FALSE,  # reuse downloaded material in workdir
                    ndown=2,      # num of download attempts
                    what='all',   # core/bremacs/studio/all; BloomR core, BRemacs, Studio, all 
                                  # 'all' requires deb==1:6. Else only core is built
                    ## For debug/test:
                    bundle='exe', # exe/zip/all/none make the related installer for distribution
                    ask=TRUE,     # asks if to overwrite existent workdir and installer
                    deb=1:6,      # defaults to 1:6 to execute all steps build steps, modify to debug.
                    gitsim=FALSE, # local path (abs. or relative)to simulate github downloads.
                    sndpass=FALSE   # Used for Studio to denote second pass 
){

    ## Set work dir
    if(!nzchar(work)) stop("Please, specify a work directory as first arg!")
    G$work=work
    
    ## Set git dir
    G$github.local=""
    if(gitsim!=FALSE && nzchar(gitsim))
        if (file.info(gitsim)$isdir)
            G$github.local=gitsim else {
                stop(gitsim, "is not an existing dir")}

    ## Windows?
    if(.Platform$OS.type != "windows")
        stop("Sorry, Bloomberg Terminal only exists for Windows and so BloomR.")
    
    ## Check for required package
    if(!loadLib("curl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Parse Arguments
    G$bremacs <- what=="bremacs" || what=="studio"
    G$branch=ifelse(G$bremacs, "brEmacs", "brCore") # distro directory
    G$studio <- (sndpass && what=="studio")

    ## Parse residual arguments
    G$ndown=ndown

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
    if(6 %in% deb) makeBundle(bundle)

    ## Make additional builds
    if(what=='all' && deb==1:6) {
        makeBloomR(work=work, tight=TRUE,  
                   ndown=ndown, bundle=bundle, ask=ask,     
                   what="bremacs",
                   deb=deb, gitsim=gitsim)
    }

    ## To save space Studio recycles BRemacs folder
    if((what=='all' || what=='studio') && !sndpass) {
        makeBloomR(work=work, tight=TRUE,  
                   ndown=ndown, bundle=bundle, ask=ask,     
                   what="studio", sndpass=TRUE,
                   deb=deb, gitsim=gitsim)
    }     
}

###== Main steps ==

### Load CRAN packages 
loadLib= function(lib){
    if (!lib %in% rownames(installed.packages())){
        repo=getOption("repos")[["CRAN"]]
        if(repo=="@CRAN@") repo="http://cran.r-project.org"
        install.packages(lib, repos=repo)
    }
    if(!eval(parse(text=paste('require(', lib,')')))){
        message("\nUnable to load", lib, "library")
        return (FALSE)
    }
    return (TRUE)
}

### Get components
downloads=function(tight){

    overwrite=!tight
        
    ## PeaZip
    cback=function(){
        url=sfFirstbyProject(G$pzip, '[[:digit:]]') #get release dir 
        url=sfFirstbyUrl(url, "portable[^\"]*?windows")
        sfDirLink(url)
    }
    download.nice(cback, G$pzip, overwrite,
                  "Peazip files")

    ## innoextract
    download.nice(innourl.ver, G$innozip, overwrite,
                  "Innoextract")

    ## R 
    download.nice(rurl.ver, G$rzip, overwrite,
                  "main R files")

    ## NSIS
    cback=function(){
        url=sfFirstbyProject(G$nsisurl, G$nsiszip)
        url=sfFirstbyUrl(url, 'additional')
        url=sfFirstbyUrl(url, '[[:digit:]]')
        sfDirLink(url)
    }
    download.nice(cback, G$nsiszip, overwrite,
                  "NSIS")
    

    ## CRAN packages
    existMake("@packs", overwrite=!tight, ask=FALSE, "packages dir:")# @ to distinguish from unzipped dir
    packs=getDeps.format(G$packlist)    
    for(pack in unique(packs)) # Loop over packs and download them 
        download.nice(cran.geturl(pack), makePath("@packs", pack), overwrite,
                  pack)
    
    ## Bloomberg API (not used)
    download.nice(G$apiurl, G$apizip, overwrite,
                  "Bloomberg API")
        
    
    ## Eikon
    download.nice(G$eikonurl, G$eikonzip, overwrite,
                  "Eikon files")
    
    ## ahkscript
    download.nice(G$ahkurl, G$ahkzip, overwrite,
                   "ahkscript")
    
    ## BRemacs
    if(G$bremacs){
        cback=function(){
            url=sfFirstbyProject(G$emacs, '[[:digit:]]') #get release dir 
            url=sfFirstbyUrl(url, paste0("-", G$emacs.type, ".7z"))
            sfDirLink(url)
        }
        download.nice(cback, G$emacs, overwrite,
                      "Emacs files")

        ## ESS
        download.nice(G$essurl, G$esszip, overwrite,
                      "ESS files")
    
        ## Polymode
        #download.nice(G$polyurl, G$polyzip, overwrite,
        #              "Polymode files")
        download.git("res/polymode-2017.zip", G$polyzip,
                     "Polymode files")

        ## Markdown mode
        download.nice(G$markurl, G$markzip, overwrite,
                      "Markdown mode files")
        
        ## Bookmark (bm) mode
        download.nice(G$bmurl, G$bmzip, overwrite,
                      "BM mode files")

    }

    ## BloomR Studio
    if(G$studio) {
        ## MikTeX
        download.nice(G$mikurl, G$mikinst, overwrite, "MikTeX")
        ## Pandoc
        cback=function(){
            req <- curl_fetch_memory(G$panurl, new_handle())
            req <- rawToChar(req$content)        
            lnks = xpathSApply(htmlTreeParse(req, useInternalNodes = TRUE), 
                               "//a", xmlGetAttr, "href")
            lrel =grep("pandoc-[.0-9]+-windows.msi$", lnks, ignore.case=TRUE, value=TRUE)[1] 
            if (!nzchar(lrel)) 
                stop("I can't find a parsable Pandoc download.") 
            paste0("https://github.com", lrel)
        }
        download.nice(cback, G$paninst, overwrite, "Pandoc")
    }
}


### Expand components
expand=function(){

    if(G$studio) return()
    
    ## peazip
    uzip(G$pzip, paste0(G$pzip,'.d'), 
          "Peazip binaries")
    
    ## innoextract
    uzip(G$innozip, paste0(G$innozip,'.d'), 
          "innoextract binaries")
    
    ## R files
    innoextract(G$rzip, paste0(G$rzip,'.d'),
                "R files")

    ## NSIS files
    uzip.7z(G$nsiszip, paste0(G$nsiszip, '.d'), 
            "NSIS files")
    
    ## Bloomberg API (not used)
    uzip(G$apizip, paste0(G$apizip,'.d'), 
          "API binaries")

    ## Eikon
    uzip(G$eikonzip, paste0(G$eikonzip,'.d'), 
          "Eikon binaries")
    
    ## CRAN packages
    message('\nExpanding packages', '...')
    from="@packs"
    #chk.write(work.pt("lib.d"), overwrite)
    del.path("lib.d")
    ## Loop and extract packs
    for(pack in  dir(work.pt(from)))
        uzip(makePath('@packs', pack), 'lib.d',
              paste('R package', pack), delTarget=FALSE)    
    
    ## ahkscript
    uzip(G$ahkzip, paste0(G$ahkzip,'.d'),
          "ahkscript")

    ## BRemacs
    if(G$bremacs){

        uzip.7z(G$emacs, paste0(G$emacs,'.d'),
             "BRemacs files")

        uzip(G$esszip, paste0(G$esszip,'.d'),
             "ESS")

        uzip(G$polyzip, paste0(G$polyzip,'.d'),
             "Polymode")

        uzip(G$markzip, paste0(G$markzip,'.d'),
             "Markdown-mode")

        uzip(G$bmzip, paste0(G$bmzip,'.d'),
             "BM mode")

    }
}

bloomrTree=function(){
### Make BloomR directory tree

    if(G$studio) {
        message("\nAdding Studio files to BRemacs BloomR tree")   
        makeStudio()
        return()
    }
    
    message("\nCreating BloomR tree")   
    existMake(G$branch, TRUE, FALSE, "BloomR root dir:")
    makeDir(app.pt(), "BloomR app dir:")

    ## Id files
    download.git("curver.txt",  app.pt("bloomr.txt")) 

    ## Copy R and make site direcory
    from=paste0(G$rzip , '.d/app')
    to=app.pt("R")
    copy.dir(from, to, "main R files")
    makeDir(app.pt('R/site-library'), "BloomR library:")
    
    ## Copy Bloomberg API
    from=paste0(G$apizip,'.d'); x=work.pt(from)
    from=makePath(from, dir(x))
    to=app.pt(G$apizip)
    copy.dir(from, to, "Bloomberg API")

    ## Install Eikon package
    message("Install Eikon API")
    exe=win.pt(app.pt('R/bin/R.exe'))
    from= win.pt(paste0(G$eikonzip,'.d/eikonapir-master'))
    to= win.pt(app.pt('R/library'))
    to=paste0("--library=", to)
    cmd=paste(exe, "--no-site-file --no-environ --no-save --no-restore --quiet CMD INSTALL",
              from, to)
    cmd=paste0('"', cmd, '"') # do not shQuote() as it will escape internal quotes
    shell(cmd, shell=Sys.getenv("COMSPEC"))
        
    ## Copy libs
    message("\nAdding R libraries")
    lib.from='lib.d'
    lib.to=app.pt("R/library")
    for(lib in dir(work.pt(lib.from))){
        from=makePath(lib.from, lib)  
        to=makePath(lib.to, lib)
        copy.dir(from, to)
    }

    
    ## Download manuals
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
    
    if(G$bremacs) bremacsTree()
}


bremacsTree=function(){
### Make BRemacs directory tree

    message("\nCreating BRemacs tree")
    makeDir(app.pt("bremacs"), "BRemacs app dir:")

    ## Copy Emacs 
    from=paste0(G$emacs, '.d/emacs')
    to=app.pt("bremacs")
    copy.dir(from, to, "main BRemacs files")

    ## Copy ESS
    from=paste0(G$esszip, '.d')
    from=globpath(from, '/ess*')
    to=slisp.pt(G$esszip)
    copy.dir(from, to, "ESS")

    ## Copy Polymode
    from=paste0(G$polyzip, '.d')
    from=globpath(from, '/polymode*')
    to=slisp.pt(G$polyzip)
    copy.dir(from, to, "Polymode")

    ## Copy Markdown mode
    from=paste0(G$markzip, '.d/markdown-mode-master')              
    to=slisp.pt(G$markzip)
    copy.dir(from, to, "Markdown mode")

    ## Copy BM mode
    from=paste0(G$bmzip, '.d/bm-master')              
    to=slisp.pt(G$bmzip)
    copy.dir(from, to, "BM mode")
    
    ## Copy BRemacs lib files
    makeDir(slisp.pt("bremacs"), "BRemacs library:")

    ## Get BRemacs lib files with ls or dir and parse into a string
    bfiles="
br-init-dbg.el  br-keys.elc    br-recentf.el   br-rnw.elc       br-simple-buffer-menu.el     
br-init.el      br-menico.el   br-recentf.elc  br-setmodes.el   br-simple-buffer-menu.elc  splith.svg      
br-keys.el      br-menico.elc  br-rnw.el       br-setmodes.elc  ess-init.R  splith.xpm
"
    #ess-init.old.R
    
    bfiles=gsub(" ", "\n", bfiles)
    bfiles=strsplit(bfiles, "\n")[[1]]
    bfiles=bfiles[nzchar(bfiles)]

    ## Download BRemacs lib files
    d=slisp.pt("bremacs")
    x=sapply(bfiles, function(f)
        download.git(makePath("src/bremacs/lib", f),  makePath(d, f)))
    download.git("src/bremacs/site-start.el",   slisp.pt("site-start.el")) 

    ## Environemnt diagnostic
    message("\nAdding ED tools")
    download.git("src/ed/bremacs.ed.cmd",      app.pt("ed/bremacs.ed.cmd")) 
    download.git("src/ed/bremacs-dbg.ed.cmd",  app.pt("ed/bremacs-dbg.ed.cmd")) 

}


makeStudio=function(){
### Make add LaTeX distro to BRemacs directory tree
### debug with
### makeBloomR('./work', tight=TRUE, what="studio", deb=1:2, gitsim=".")
    
    makeStudio.getLatexAddons()
    makeStudio.getPandoc()    
}


makeStudio.getLatexAddons=function(){

    ### Download LaTeX distro, Pandoc and knitr required LaTeX packages
    
    ## Latex variables  
    latdir=app.pt("latex")
    latbin.pt=makePath(latdir, "/texmfs/install/miktex/bin/latex.exe")


    ## Expand MikTeX 
    message("Expand MikTeX")
    out <- system(paste0(win.pt(G$mikinst), " -o", win.pt(latdir), " -y"),
                  intern = TRUE, invisible = FALSE)
    if(!file.exists(work.pt(latbin.pt)))
        stop(paste("An error occurred while extracting\n", G$mikinst,
                   "\nPlease, exit BloomR and try again."))

    
    ## Add extra packages
    lpacks=c(
        'fancyvrb',
        'microtype',
        'mptopdf',
        'upquote',
        'url',
        'parskip'
        )  
    x=sapply(lpacks, getLatex.pack, getLatex.packList(inst=TRUE))

}

getLatex.pack=function(pname, ipacks=NULL){
### Install a LaTeX Package, via  MiKTeX mpm --install
## ipacks (optional) is the list of installed packages.
## Used to speed-up if calling the function many times 
    
    mpm=app.pt("latex/texmfs/install/miktex/bin/mpm.exe")
    if(!file.exists(work.pt(mpm))) stop(paste('Unable to find:\n', mpm))

    if(is.null(ipacks)) ipacks=getLatex.packList(inst=TRUE)
    if(pname %in% ipacks) {
        message(shQuote(pname), ' already installed. Skipping.')
        return()
    }

    message('Installing package ', shQuote(pname))
    pack=paste0("--install=", pname)
    out=system(paste(win.pt(mpm), pack), intern = TRUE, invisible = FALSE)
    if(!getLatex.packCheck(pname)) stop("Unable to install LaTeX package ", shQuote(pname))

}

getLatex.packList=function(inst=FALSE){
### Get list of all packages from MiKTeX mpm --list
## If inst=TRUE, give vector of names of installed ones


    mpm=app.pt("latex/texmfs/install/miktex/bin/mpm.exe")
    if(!file.exists(work.pt(mpm))) stop(paste('Unable to find:\n', mpm))
    out=system(paste(win.pt(mpm), '--list'), intern = TRUE, invisible = FALSE)
    if(inst) {
        x=grep('^i', out, value=TRUE)
        sapply(strsplit(x, ' +'), `[`, 4)
    } else out
}

getLatex.packCheck=function(pname) # Check if a package is installed via MiKTeX mpm --list
    pname %in% getLatex.packList(inst=TRUE)    


makeStudio.getPandoc=function(){
### Download Pandoc  

    ## Pandoc variables (msi wants abs path)
    pandir = app.pt("pandoc")
    panbin = work.pt(makePath(pandir, "/bin/pandoc.exe"))
    pantmp=work.pt(makePath(pandir, "temp"))
    pantmp.abs=shQuote(normalizePath(pantmp, mustWork=FALSE))
    paninst.abs=shQuote(normalizePath(work.pt( G$paninst)))

    
    ## Create Pandoc dir
    existMake(pandir, TRUE, FALSE, "Pandoc dir:")

    ## Install Pandoc 
    cmd = paste("msiexec /qb /a", paninst.abs)
    cmd = paste0(cmd, " TARGETDIR=", pantmp.abs)
    out <- system(cmd, intern = TRUE, invisible = FALSE)
    file.rename(makePath(pantmp, "/pandoc"),
                work.pt(makePath(pandir, "/bin")))
    unlink(pantmp, recursive = TRUE, force = TRUE)
        
    if(!file.exists(panbin)) 
        stop(paste("An error occurred while extracting\n", G$paninst, 
            "\nPlease, exit BloomR and try again."))
}



###== Boot functions ==

### Make etc/Rprofile.site from PROF()
initScripts=function(){

    if(G$studio) return()

    message("\nMaking etc/Rprofile.site and shared directory")

    ## Make new Rprofile.site and keep old
    p=capture.output(PROF)  # Get PROF function definition 
    p=p[-c(1, length(p))]   # Remove "function {", "}"
    prof.new=work.pt(app.pt('R/etc/Rprofile.site'))
    prof.nat=work.pt(app.pt('R/etc/Rprofile.native'))

    ## Append with Unix line endings
    if(!is.file(prof.nat)){
        file.copy(prof.new, prof.nat)                  
        con= file(prof.new, open="ab")
        writeLines(text=p, con=con)
        close(con)
    }
    
    ## Get bloomr lib files including xlx.R from Github
    to=app.pt("R/share/bloomr")    
    makeDir(to,"BloomR share directory:")
    download.git("src/bloomr.init.R", app.pt("R/share/bloomr/bloomr.init.R"))
    download.git("src/bloomr.beta.R", app.pt("R/share/bloomr/bloomr.beta.R"))
    download.git("src/bloomr.api.R",  app.pt("R/share/bloomr/bloomr.api.R"))
    download.git("src/bloomr.R",      app.pt("R/share/bloomr/bloomr.R"))
    download.git("src/bloomr.sys.R",  app.pt("R/share/bloomr/bloomr.sys.R"))
    download.git("src/bloomr.test.R", app.pt("R/site-library/bloomr.test.R"))
    download.git("src/xlx/xlx.R",     app.pt("R/share/bloomr/xlx.R"))

    ## Testdata
    to=app.pt("R/share/bloomr/testdata")
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
                  
    ## Make R bootstrapper
    makeBoot()

}

makeBoot=function(){
### Make R bootstrapper

    ## Boot string    
    bloomr.run="
EnvSet,  BLOOMR,     %A_ScriptDir%
EnvSet,  HOME,       %A_ScriptDir%\\mybloomr
EnvSet,  JAVA_HOME,  %A_ScriptDir%\\%AppDir%\\openjdk/jre
;EnvSet, PATH,       %A_ScriptDir%\\%AppDir%\\openjdk/bin;%path%
Run, %AppDir%\\R\\bin\\x64\\Rgui.exe LANGUAGE=en
"
    bloomr.run=gsub("%AppDir%", G$appname, bloomr.run)
    makeBoot_(bloomr.run, "bloomr")
    
    if(G$bremacs){
        bremacs.run= "
EnvSet,  BLOOMR,     %A_ScriptDir%
EnvSet,  HOME,       %A_ScriptDir%\\main\\bremacs
EnvSet,  JAVA_HOME,  %A_ScriptDir%\\%AppDir%\\openjdk/jre
;EnvSet, PATH,       %A_ScriptDir%\\%AppDir%\\openjdk/bin;%path%
Run, %AppDir%\\bremacs\\bin\\runemacs.exe -q --no-splash
"        
        bremacs.run=gsub("%AppDir%", G$appname, bremacs.run)
        makeBoot_(bremacs.run, "bremacs")
    }   
    
}


makeBoot_=function(script.cont, stem){

    
    ## Make boot file
    ahkdir=work.pt(paste0(G$ahkzip, '.d/Compiler'))
    cat(script.cont, file=makePath(ahkdir, paste0(stem, ".run")))
   
    ## Get icon from GitHub
    to=makePath(paste0(G$ahkzip, '.d/Compiler'), paste0(stem, ".ico"))
    download.git(makePath("res", paste0(stem, ".ico")), to)
    
    ## Make exe
    message("\nMaking BloomR executable")
    cd=normalizePath(ahkdir)
    cd= paste0('cd "', cd, '" &')
    run=paste(
        "Ahk2Exe.exe /in", paste0(stem, ".run"), "/icon", paste0(stem, ".ico"), "/bin \"Unicode 32-bit.bin\"")
    shell(paste(cd, run), shell=Sys.getenv("COMSPEC"))    

    ## Move exe
    from=makePath(ahkdir, paste0(stem, ".exe"))
    to=work.pt(paste0(G$branch, "/", stem, ".exe"))
    file.rename(from, to)    
    
}

### "etc/Rprofile.site" source (braces on separate lines) 
PROF=function(){ #Keep this on separate line
    
    ## BloomR bootstrap
    ## ================
    
    source(paste0(R.home("share"), "/bloomr/bloomr.init.R"))
    
    
    ## end BloomR----------
}



###== Utilities ==

### Exe and Zip distro
makeBundle=function(bundle, # 'exe'/'zip'/'all'/'none'
                    ask
                    ){
### Create create exe, zip, both, or nothing   
### Every bundle value not 'exe'/'zip'/'all' means 'none'
    
    if(bundle=='all' || bundle=='exe') makeInst(ask)
    if(bundle=='all' || bundle=='zip') makeZip(ask)    
}


makeInst=function(ask){

    message('\nCreating BloomR green installer')
    ## Set name (nsi name is "BRsetup.exe")
    to="BloomR_setup_.exe"
    if(G$bremacs) to="BloomR+BRemacs_setup_.exe"
    if(G$studio) to="BloomR+Studio_setup_.exe"
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)
    download.git("bloomr.nsi", "bloomr.nsi")
    message('Creating green installer ', to, '\nThis may take a bit...')
    nsi='bloomr.nsi'
    nexe=paste0(G$nsiszip,'.d/App/NSIS/makensis.exe')
    nsrc=paste0("/dSRCDIR=", G$branch)
    cmd=paste(win.pt(nexe), "/v2", nsrc, win.pt(nsi))
    ret=system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
    file.rename(work.pt("BRsetup.exe"), work.pt(to))

}


makeZip=function(ask){

    message('\nCreating BloomR.zip')
    to="BloomR_setup_.zip"
    if(G$bremacs) to="BloomR+BRemacs_setup_.zip"
    if(G$studio) to="BloomR+Studio_setup_.zip"
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)    
    from=paste0('./', G$branch, '/*')  # In 7z ./ removes dir prefix from archive files
    zexe=get7zbin()
    #cmd=paste("cmd.exe /c cd /D", win.pt(G$work) , "&")
    cmd=paste(win.pt(zexe), "a", win.pt(to), win.pt(from))
    ret=system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}


###== Website helpers ==

### First SF project item matching regex filter (url made from prj name)
sfFirstbyProject=function (project, filtx, quiet=FALSE){
    if(!quiet) messagev('Searching last', project, 'version on SF.net')
    url=paste0("https://sourceforge.net/projects/", project, "/files/")
    ref="https://sourceforge.net"    
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
                    "//a[span[@class='name']]",  xmlGetAttr, "href")
    url=grep(filtx, url, value=TRUE, ignore.case=TRUE)[1] 
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    return (url)
}

### First SF url item matching regex filter (url given from prj name)
sfFirstbyUrl=function (url, versionx, quiet=FALSE){
    if(!quiet) messagev('Searching for version ', versionx, ' on\n', url)
    ref="https://sourceforge.net"
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[span[@class='name']]",  xmlGetAttr, "href")
    return (grep(versionx, url, value=TRUE, ignore.case=TRUE)[1])
}

### Follow the direct-download link 
sfDirLink=function (url, quiet=FALSE){

    if(!quiet) messagev('Find best mirror for\n', url)
    ref="http://sourceforge.net"
    page=download.html(url, refr=ref)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[@data-release-url]",  xmlGetAttr, "data-release-url")
    return (url)
}


## Get  CRAN Windows binaries release for a package 
cran.geturl=function(pack){

    ## CRAN links
    cranpage="http://cran.r-project.org/web/packages/"
    cranbin="http://cran.r-project.org/bin/"

    ## Get package page 
    url=paste0(cranpage, pack, "/index.html")
    page=download.html(url)    

    ## Get bin url: the first occurence on page is the dev version, second is release
    url=regmatches(page, gregexpr("windows/contrib.*?\\.zip", page))[[1]][2]
    paste0(cranbin, url)   
}


innourl.ver=function(){
    url=G$innourl
    messagev("Parsing page:\n", url, ' ...')
    page=download.html(url)    
    href=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=rev(grep("innoextract-*", href, valu=TRUE))[1]
    href=gsub("/", "", href)
    paste0(url, "/",  href, "/", href, "-windows.zip")
}

rurl.ver=function(){
    url=G$rurl
    messagev("Parsing page:\n", url, ' ...')
    page=download.html(url)    
    href=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("R.+win\\.exe", href, value=TRUE)
    paste0(url, "/",  href)
}


###== Donwload helpers ==


download.nice=function(from, to, overwrite, desc=""){
### Download relative to workdir with nice user info and overwrite management

    if(desc=="") desc=sub('.+/', '', to)
    to=work.pt(to)
    message("\nDownloading ", desc) 

    if(is.path.abs_(to)) {
        warn.path(to, "already exists.")
        if(!overwrite){
            message("Skipping action to preserve existing download.")
            return()
        }
    }

    ## Execute from callback 
    if(is.function(from)) from=from()
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


download.html=function(url, refr=NULL){
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


download.git=function(file, to, overwrite=TRUE, desc=""){
### Download (realtive to work.pt) from github or use local git dir
### Git dir can be set with makeBloomR(gitsim=...) and is shared via global var G$github.local 
    
    if(!nzchar(desc)) desc=file
    
    ## remote git
    if(!nzchar(G$github.local)) {
        from=makePath(G$github, file)
        download.nice(from, to, overwrite=TRUE, desc)
        
    ## local git
    } else {
        messagev('\nDownloading', desc)
        from=makePath(G$github.local, file)  # not relative to workdir 
        if(!is.path.abs_(from)) stop(from, "\nnot found in local Git repo:\n", G$github.local)
        to=work.pt(to)
        file.copy(from, to, overwrite=TRUE) 
    }

}



###== File System ==


makePath=function(parent, child){    
### Chain parent-child paths managing '/' middle slashes 
## You don't have to rememeber if you need to add or not that slash

    ## No trail for parent 
    parent=sub('/$', '',  parent)
    parent=sub('\\\\$', '',  parent)

    ## No lead for child
    child=sub('^/', '',  child)
    child=sub('^\\\\', '',  child)

    ## Now file.path inputs is secure
    file.path(parent, child)

}

work.pt=function(dir=""){    
### Prefix dir path with work dir path
### Work dir path is set with global G$work

    makePath(G$work, dir)
}

root.pt=function(dir=""){
### Prefix dir path with BloomR distro root path relative to workdir.
### Depends on distro under build: e.g. path/to/workdir/brCore

    makePath(G$branch, dir)
}


app.pt=function(dir=""){
### Prefix dir path with BloomR apps' path relative to workdir.
### App dir name depends on G$appname, its parent is BloomR root dir
### E.g. path/to/workdir/current-distro/G$appname

    x=root.pt(G$appname)
    makePath(x, dir)
}

slisp.pt=function(dir=""){
### Prefix dir path with BRemacs site-lisp path relative to workdir.
    
    x=app.pt("bremacs/share/emacs/site-lisp")
    makePath(x, dir)
}



win.pt=function(path){
### Given a Unix path relative to global workdir (G$work), quote, Windows-ize and prefix with wordir
### If G$work=="./workdir", "foo/bar" -> "\".\\wordir\\foo\\bar\""
### This function is used to send paths Windows shell (e.g. with shell)
    
    path=work.pt(path)
    path=shQuote(path)
    chartr("/", "\\", path)
}


is.path=function(path){ # Path exists relative to workdir
    is.path_(path)
}


is.path_=function(path){ # is.path()  work horse
    path=work.pt(path)
    is.path.abs_(path)
}

is.path.abs_=function(path){ # Path exists relative to currpath
    path=sub("/$", "", path) # "/" might be already removed
    file.exists(path)
}


is.file=function(file){ # Path exists as a file relative to workdir    
    is.path(file) && !dirtype_(file)
}

is.dir=function(dir){ # Path exists as a dir relative to workdir
    is.path(dir) && dirtype_(dir)
}

is.empty=function(dir){
### Dir (relative to workdir) is empty
### Also: T, if does not exist; F, if is a file

    if(is.file(dir)) return (FALSE)
    is.empty_(dir)
}

is.empty_=function(dir){ # is.empty workhorse
    length(dir(work.pt(dir), all.files=TRUE, no..=TRUE)) ==0
}


chk.file=function(file, desc=""){ # Break if path relative to workdir not a file
    if(!is.path(file)) stop(file, "\nis not a valid path")
    if(is.dir(file))   stop(file, "\nis a directory")
}

chk.dir=function(dir){ # Break if path relative to workdir not a dir
    if(!is.path(dir)) stop(dir, "\nis not a valid path")
    if(!is.dir(dir))  stop(dir, "is a file")
}

dirtype_=function(file){ # File relative to workdir is of type dir
    file.info(work.pt(file))$isdir
}



makeDir=function(dir, desc=""){
### Create a dir relative to workdir with optional desc message and stop on errors   

    if(nzchar(desc)) message("Creating ", desc )
    del.path(dir)
    makeDir_(dir, desc)
}

makeDir_=function(dir, desc) { # makeDir FS helper
    dir=work.pt(dir)
    dir.create(dir, showWarnings = FALSE)
    ## Unable to create? 
    if(!is.path.abs_(dir))
        stop("\nUnable to create ", desc, "\n", dir)
}

del.path=function(path){
### Delete path relative to workdir and break on fail 
    if(!is.path(path)) return()
    del.path_(path)
}

del.path_=function(path){ # del.path workhorse

    path=work.pt(path)
    unlink(path,recursive=TRUE, force=TRUE) #  non-existent file is not an unlink failure
    if(is.path.abs_(path)) {
        Sys.sleep(1.5)
        if(is.path.abs_(path)) stop("\nUnable to access\n", path)
    }
}

del.ask=function(path, ask, desc){
### Warn on relative to workdir path existence.
### if ASK flag is set, ask for delete confirmation, else just notify
### Actual delete is not peformed here    

    del.ask_(path, ask, desc)
}

del.ask_=function(path, ask, desc){ # del.ask workhorse
    path=work.pt(path)
    del.ask.abs_(path, ask, desc)
}

del.ask.abs_=function(path, ask, desc){
### Warn on path existence, relative to currpath.
### if ASK flag is et, ask for delete confirmation, else just notify

    warn.path(path, desc) ## Path warnings are better relative to currpath then workdir
    
    ## Confirmation not required
    if(!ask) {
        message("It will be deleted ...")
        return()
    }
    ## Require confirmation
    repeat{
        ans =toupper(readline("Delete it? (Y/n) : "))
        if(ans=="Y" || ans=="N" || ans=="") break
    }    
    if(ans=="N") stop("Stopped by user.")
}


copy.dir=function(from, to, desc=""){
### Copy FROM directory in TO parent and rename TO as FROM
### If `to' dir exists delete it. FROM/TO are relative to workdir 

    if(nzchar(desc)) message("\nAdding ", desc)
    del.path(to)
    copy.dir_(from, to)
}

copy.dir_=function(from, to){ # copy.dir work horse

    from=work.pt(from)
    to=work.pt(to)    
    file.copy(from, dirname(to), recursive=TRUE)
    file.rename(makePath(dirname(to), basename(from)), to)
}

globpath=function( # Add globbed child to parent relative to workdir
                  parent,   # relative to workdir
                  globchild # child with globbing    
                  ){
    wparent <- work.pt(parent)
    base <- basename(Sys.glob(makePath(wparent, globchild)))
    makePath(parent, base)    
}

existMake=function(dir, overwrite, ask, desc){
### If dir relative to workdir does not exist make it, otherwise might ask and skip creation 
### An empty-dir is considered non-existent. Note: if dir="", it is the workdir 

    ## Inform user with desc if any
    if(nzchar(desc)) message("\nCreating ", desc, '\n',  dir)

    ## Not a dirty dir
    if(!is.path(dir) || is.empty(dir)){
        makeDir(dir)
        return()
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

file.read=function( # Read file  
                   fpath # File path relative to workdir
                   )    
    readLines(work.pt(fpath))


uzip=function(from, to, desc, delTarget=TRUE){
### Unzip making path relative to global workdir
### Stops on errors and inform used with a desc argument

    message('\nExpanding ', desc, '...') 
    chk.file(from)
    if(delTarget) del.path(to)
    uzip_(from, to)
    
}

uzip_=function(from, to){ # uzip workhorse    

    from=work.pt(from)
    to=work.pt(to)
    if(length(unzip(from, exdir= to))==0)
        stop('\nUnable to perform extraction')  
}

uzip.7z=function(from, to, desc, delTarget=TRUE){
    zexe=get7zbin()
    if(is.path(to)) {
        message('\nDeleting exisiting ', desc)
        del.path(to)
    }    
    message('\nExtracting ', desc)
    message('This may take a bit ...')
    cmd=paste(win.pt(to), win.pt(from))
    cmd=paste0(win.pt(zexe), ' x -aoa -r -o', cmd)
    ret=system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}

get7zbin=function(){ # Get 7z.exe relative to workdir
    zipdir=paste0(G$pzip,'.d')
    versiondir=get7zbin.ver_(zipdir)
    subpath='/res/7z/7z.exe'
    makePath(zipdir, paste0(versiondir, subpath))
}

get7zbin.ver_=function(zipdir){ # get7zbin workhorse
    zipdir=work.pt(zipdir)
    dir(zipdir)
}


innoextract=function(from, to, desc, delTarget=TRUE){
    exe=getInnobin()
    if(is.path(to)) {
        message('\nDeleting exisiting ', desc)
        del.path(to)
    }    
    message('\nExtracting ', desc)
    message('This may take a bit ...')
    cmd=paste(win.pt(exe), win.pt(from),  "--output-dir", win.pt(to))
    ret=system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))
}


getInnobin=function(){ # Get 7z.exe relative to workdir
    innodir=paste0(G$innozip,'.d')
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
        #browser()
        deps <- NA
        if(pname %in% rownames(available)){
            deps <- available[pname,"Imports"]
            deps <- gsub("\\(.+?\\)", "", deps)
            deps <- gsub("\\n", "", deps)
            deps <- gsub(" +", "", deps)
            deps <- strsplit(deps, ",")[[1]]
        }

    #    print(pname) ; print(deps)
        if(!any(is.na(deps))) deps <- c(deps, getImports(deps, available))
     })))

    ## Remove initial
    setdiff(udeps, pnames)

}


### == Messages ==

warn.path=function(path, mess){
    message("Warning:\n", path, "\n", mess)
}

messagev=function(...) { # message for character vector
    message(paste(...))
}

### probably not used
   
### Check if we can overwrite non-empty dir and possibly stop
chk.write=function(path, over, desc="", stop=TRUE){
    ## Empty dir are overwritten without prompt
    ## File is considered a non-empty dir
     
    if(nzchar(desc)) desc=paste(desc, '\n')

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
warn.p=function(path, mess){
    messagev("Warning:", mess.p(path, mess))
}

### Exisintg paths
mess.p=function(path,  mess){
 #   if(nzchar(desc)) desc=paste(desc, '\n')   
    paste0("\n", path, "\n", mess)
}

### Exisintg paths stop
exit.p=function(path,  mess){ 
    stop(mess.p(path,  mess))
}

### Generic download info for custom download
mess.down=function(desc){
    messagev("\nDownloading", desc)
}

