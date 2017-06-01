###  BloomR source

##  TODO
##  Compile BRemacs packages (BM) on first run
##  Add  rClr, rEikon, RDatastream  in site-library?
##  Test BRemecs with rmd, rnw etc.
##  Fix getURL("https://www.google.com", cainfo="cacert.pem"), using http://curl.haxx.se/ca/cacert.pem which
##    causes:
##    Fix download error in: download.nice(G$apiurl, G$apizip, ..., cert=F), if cert is T
##    Fix download error in: download.nice(G$ahkurl, G$ahkzip ...) 
##  Create a java bin to test java directory variables
##  Finish help in src\bloomr.beta.Rmd
##
##  Usage:
##  Source this file and run:
##  makeBloomR("path/to/workDir")
##  You will get the BloomR green executable in workDir
##
##  Requirements:
##  XML and Rcurl packages. If missing it will try to download and install them.
##  R should be able to connect to the Internet.
##  .Platform$OS.type == "windows"
##
##  Credits:
##  R-Portable*.exe from sourceforge.net/projects/rportable
##  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
##  Rbbg_*.zip from http://r.findata.org/bin/windows/contrib/
##  peazip from http://sourceforge.net/projects/peazip
##  ahkscript from http://ahkscript.org
##  Alex Kasko java from https://bitbucket.org/alexkasko/openjdk-unofficial-builds
##  Nsis from nsis.sourceforge.net
##  innoextract from http://constexpr.org/innoextract
##  Icon set Simplicio CC 3.0 by Neurovit: http://neurovit.deviantart.com
##   retrieved at https://www.iconfinder.com/icons/48750/direction_right_icon
##


### Globals
G=new.env()

### Contribs
## Java alexkasko
G$javaurl="https://bitbucket.org/alexkasko/openjdk-unofficial-builds/downloads/"
G$javaurl.dom="https://bitbucket.org"
## win32
G$javaurl.bit="windows-i586-image.zip"
## win64
G$javaurl.bit="windows-amd64-image.zip"
G$javazip='openjdk'

## Bloomberg API for icedtea 
G$apiurl="https://bloomberg.bintray.com/BLPAPI-Stable-Generic/blpapi_java_3.8.8.2.zip"
G$apizip="blpapi"

## Rbbg win32         
G$rbbgurl="http://r.findata.org/bin/windows/contrib/"
## Rbbg win64
G$rbbgurl="http://r.findata.org/bin/windows64/contrib/"
G$rbbgzip="rbbg"

## Ahkscript
## G$ahkurl="http://ahkscript.org/download/ahk2exe.zip" # removed
G$ahkurl="https://autohotkey.com/download/ahk.zip"
G$ahkzip="ahk"

## Web certificates
G$certurl='http://curl.haxx.se/ca/cacert.pem'

## Github
G$github="https://raw.githubusercontent.com/AntonioFasano/BloomR/master"
G$github.local="" # Auto-set by makeBloomR() if gitsim=T, relative to workdir 

## Packages to download. Case sensitive
x="rJava zoo xts RCurl XML knitr"

## RCurl deps
x=paste(x, "bitops")

## Knitr deps
x=paste(x, "evaluate digest markdown yaml highr formatR stringr")

## Stringr deps
x=paste(x, "stringi magrittr")

## Markdown deps
x=paste(x, " mime")

## read.xlx deps
x=paste(x, "plyr pbapply Rcpp")
G$packlist=x
rm(x)
    
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
G$essurl='https://github.com/emacs-ess/ESS/archive/master.zip'
G$esszip='ess'
G$polyurl='https://github.com/vspinu/polymode/archive/master.zip'
G$polyzip='polymode'
G$markurl='https://github.com/jrblevin/markdown-mode/archive/master.zip'
G$markzip='markmode'
G$bmurl='https://github.com/joodland/bm/archive/master.zip'
G$bmzip='bmmode'

## Local paths
G$work=""
G$appname="main" # BloomR application folder name. Used by app.pt() 
G$branch=NULL #"brCore"

## Arguments
G$bremacs=NULL
G$ndown=0


## Dev style 
## Functions returning/accepting paths normally use paths relative to G$work.
## The latter ones will use a workhorse function with work.pt() if they need to access file system.

makeBloomR=function( # Build BloomR
                    work,         # work dir path, absolute or relative to cur path
                    tight=FALSE,  # reuse downloaded material in workdir
                    ndown=2,      # num of download attempts
                    zip=FALSE,    # if TRUE zip the BloomR output dir
                    ask=TRUE,     # asks if to overwrite existent workdir and BloomR installer
                    what='all',   # core/bremacs/all; build BloomR core, BRemacs or all of them 
                                  # if deb!=1:6, despite all only executes first branch (core)
                    ## For debug/test:
                    deb=1:6,      # defaults to 1:6 to execute all steps build steps, modify to debug.
                    gitsim=FALSE  # local path (abs. or relative)to simulate github downloads.  
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
    if(!loadLib("RCurl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Parse Arguments
    G$bremacs=FALSE
    if(what=="bremacs") G$bremacs=TRUE
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
    if(6 %in% deb) {makeInst(ask); if(zip) makeZip(ask)}

    if(what=='all' && deb==1:6) 
        makeBloomR(work=work, tight=TRUE,  
                   ndown=ndown, zip=zip, ask=ask,     
                   what="bremacs",
                   deb=deb, gitsim=gitsim)

    
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
        
    ## Get certificates from curl site 
    # download.nice(G$certurl, cert(), overwrite,
    #              "Curl Certificates", cert=FALSE)
    if(is.path.abs_(makePath(G$work, cert())) && !overwrite){
        warn.path(makePath(G$work, cert()), "already exists.")
    } else download.file(G$certurl, makePath(G$work, cert()))


    ## peazip
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
    
    ## Openjdk
    download.nice(javaurl.ver, G$javazip, overwrite,
                  "Java files")

    ## Bloomberg API
    download.nice(G$apiurl, G$apizip, overwrite,
                  "Bloomberg API", cert=FALSE)
        
    ## CRAN packages
    existMake("@packs", overwrite=!tight, ask=FALSE, "packages dir:") # @ to distinguish from unzipped dir   
    packs= strsplit(gsub('(^ +)|( +$)', '', G$packlist), split=' +')[[1]]    
    for(pack in packs) # Loop over packs and download them 
        download.nice(cran.geturl(pack), makePath("@packs", pack), overwrite,
                  pack)
    
    ## rbbg
    download.nice(rbbgurl.ver(), makePath("@packs", G$rbbgzip), overwrite,
                  "rbbg files")
    
    ## ahkscript
    ## given a not found error there is a temp fix
    ## download.nice(G$ahkurl, G$ahkzip, overwrite,
    ##               "ahkscript")
    if(is.path.abs_(makePath(G$work, G$ahkzip)) && !overwrite){
        warn.path(makePath(G$work, G$ahkzip), "already exists.")
    } else download.file(G$ahkurl, makePath(G$work, G$ahkzip))

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
        download.nice(G$polyurl, G$polyzip, overwrite,
                      "Polymode files")

        ## Markdown mode
        download.nice(G$markurl, G$markzip, overwrite,
                      "Markdown mode files")
        
        ## Bookmark (bm) mode
        download.nice(G$bmurl, G$bmzip, overwrite,
                      "BM mode files")


    }
    
}


### Expand components
expand=function(){
    
    ## peazip
    uzip(G$pzip, paste0(G$pzip,'.d'), 
          "Peazip binaries")
    
    ## innoextract
    uzip(G$innozip, paste0(G$innozip,'.d'), 
          "innoextract binaries")
    
    ## R files
    innoextract(G$rzip, paste0(G$rzip,'.d'),
                "R files")

    ## R portable files
    ## uzip.7z(G$rport, paste0(G$rport,'.d'), 
    ##       "R files")
        
    ## NSIS files
    uzip.7z(G$nsiszip, paste0(G$nsiszip, '.d'), 
            "NSIS files")

    ## openjdk 
    uzip.7z(G$javazip, paste0(G$javazip, '.d'), 
            "Java binaries")
    
    ## Bloomberg API
    uzip(G$apizip, paste0(G$apizip,'.d'), 
          "API binaries")

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

    G$branch="brCore"
    if(G$bremacs) G$branch="brEmacs"
    
    message("\nCreating BloomR tree")   
    existMake(G$branch, TRUE, FALSE, "BloomR root dir:")
    makeDir(app.pt(), "BloomR app dir:")


    ## Copy R and make site direcory
    from=paste0(G$rzip , '.d/app')
    to=app.pt("R")
    copy.dir(from, to, "main R files")
    makeDir(app.pt('R/site-library'), "BloomR library:")
    ## del.path(app.pt("R/unins000.dat"))
    ## del.path(app.pt("R/unins000.exe"))
    
    ## Copy java
    from=paste0(G$javazip,'.d'); x=work.pt(from)
    from=makePath(from, dir(x))
    to=app.pt(G$javazip)
    copy.dir(from, to, "Java modules")
    del.path(makePath(to, 'src.zip'))

    ## Copy Bloomberg API
    from=paste0(G$apizip,'.d'); x=work.pt(from)
    from=makePath(from, dir(x))
    to=app.pt(G$apizip)
    copy.dir(from, to, "Bloomberg API")

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
    from=paste0(G$esszip, '.d/ESS-master')              
    to=slisp.pt(G$esszip)
    copy.dir(from, to, "ESS")

    ## Copy Polymode
    from=paste0(G$polyzip, '.d/polymode-master')              
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




###== Boot functions ==

### Make etc/Rprofile.site from PROF()
initScripts=function(){

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
    
    library("rJava")
    library("Rbbg")
    
    
    ## end BloomR----------
}



###== Utilities ==

### Exe and Zip distro 
makeInst=function(ask){

    message('\nCreating BloomR green installer')
    ## Set name (nsi name is "BRsetup.exe")
    to="BloomR_setup_.exe"
    if(G$bremacs) to="BloomR+BRemacs_setup_.exe"
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
        "//a[@class='name']",  xmlGetAttr, "href")
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
        "//a[@class='name']",  xmlGetAttr, "href")
    return (grep(versionx, url, value=TRUE, ignore.case=TRUE)[1])
}

### Follow the direct-download link 
sfDirLink=function (url, quiet=FALSE){

    if(!quiet) messagev('Find best mirror for\n', url)
    ref="http://sourceforge.net"
    page=download.html(url, refr=ref)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[@class='direct-download']",  xmlGetAttr, "href")
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


## Get last versions    
javaurl.ver=function(url){
    url=G$javaurl
    messagev("Parsing page:\n", url, ' ...')
    if(!url.exists.cert(url, cert=TRUE))
        stop("Unable to open java download page:\n", url)       
    href=getURL(url, ssl.verifypeer=TRUE, cainfo=cert.abs_())   
    href=xpathSApply(htmlTreeParse(href, useInternalNodes=TRUE),
        "//a[@class='execute']", xmlGetAttr, "href")
    href=grep(paste0(G$javaurl.bit, "$"), href, value=TRUE)[1]
    paste0(G$javaurl.dom, href)
}

rbbgurl.ver=function(){
    url=G$rbbgurl
    messagev("Parsing page:\n", url, ' ...')
    if(!url.exists.cert(url, cert=TRUE)) stop("Unable to open rbbg page:\n", url)
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("^[[:digit:]]\\.[[:digit:]]", href, value=TRUE)
    url=paste0(url, href[length(href)])
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("\\.zip$", href, value=TRUE)
    paste0(url, href)
}

innourl.ver=function(){
    url=G$innourl
    messagev("Parsing page:\n", url, ' ...')
    if(!url.exists.cert(url, cert=FALSE)) stop("Unable to open ", url, " page:\n", url)
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=rev(grep("innoextract-*", href, valu=TRUE))[1]
    href=gsub("/", "", href)
    paste0(url, "/",  href, "/", href, "-windows.zip")
}

rurl.ver=function(){
    url=G$rurl
    messagev("Parsing page:\n", url, ' ...')
    if(!url.exists.cert(url, cert=FALSE)) stop("Unable to open ", url, " page:\n", url)
    href=xpathSApply(htmlTreeParse(getURL(url), useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("R.+win\\.exe", href, value=TRUE)
    paste0(url, "/",  href)
}


###== Donwload helpers ==

### Verify url exists using cert.abs_() certificate
url.exists.cert=function(url, cert=TRUE){
    ## cert=TRUE implies an exisiting pem cert.abs_()
    
    if(cert==FALSE) return(url.exists(url, ssl.verifypeer=FALSE))
    if(!file.exists(cert.abs_())) stop('\nCan\'t find certificate\n', cert.abs_())
    return(url.exists(url, ssl.verifypeer=TRUE, cainfo=cert.abs_()))
}

download.nice=function(from, to, overwrite, desc="", cert=TRUE){
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
    cert= if(cert) cert.abs_() else NULL

    ## Download ndown times and exit deleting file on errors
    for(i in 1:G$ndown)
        if(s<-download.bin(from, to, cert=cert)$succ) break
    if(!s){
        unlink(to, force = TRUE)
        stop('\nDownload error')
    }
   
    
   # if(!download.bin(from, to, cert)$succ) {
   #     unlink(to, force = TRUE)
   #     stop('\nDownload error')
   # }
    
}

download.html=function(url, refr=NULL, cert=TRUE){
### Download html page with simple progress and stop on error
    
    if(!url.exists.cert(url, cert)) stop('\nCan\'t find page\n', url)
    cert= if(cert) cert.abs_() else NULL    
    ret=getURL(url, referer=refr, noprogress=FALSE,
               ssl.verifypeer=!is.null(cert), cainfo=cert,
               progressfunction=function(down,up) cat("\rBytes:", down[2]))
    cat("\n")
    ret
}

cert=function() {# local web certificate relative to workdir
    "cacert.pem"
}

cert.abs_=function() {# local web certificate relative to currpath
    work.pt(cert())
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


### Download binary file returning list(succ, headers) 
download.bin=function(url, file, refr=NULL, cert=NULL, curl = NULL){
    redirect=!is.null(curl)
    if(is.null(curl)) curl = getCurlHandle()
    f = CFILE(file, mode="wb")
    headers =  basicHeaderGatherer()
    width= getOption("width") - 25  # output width (dots + data)
    dcur=0  # download status
    ## Referer
    opt=list(referer=NULL); opt$referer =refr

    ## Callback function for curlPerform
    dProgress=function(down, up, dcur, width){
        total=as.numeric(down[1]) # Total size as passed from curlPerform
        cur=as.numeric(down[2])   # Current size as passed from curlPerform
        x=cur/total
        px= round(100 * x)
        if(total==0)  x=cur
        ## if(!is.nan(x) &&  px>60) return(dcur) # Just to debug at 60%
        if(!is.nan(x) && cur!=dcur){
            sc=rev(which(total>= c(0, 1024^1, 1024^2, 1024^3)))[1]-1 # scale stats to total
            if(x>1)  { #Download size unknown 
                total<-px<-NA
                x=0
                sc=rev(which(cur>= c(0, 1024^1, 1024^2, 1024^3)))[1]-1 # scale to current
            }
            lb=c('B', 'KB', 'MB', 'GB')[sc+1]
            wx= round(width * x) # line width as as percent download
            cat(paste(c(         # Print |, dots if any, stats 
                        "\r  |", rep.int(".", wx), rep.int(" ", width - wx),
                        sprintf("| %g%s of %g%s %g%%",
                                round(cur/1024^sc, 2), lb, round(total/1024^sc, 2), lb, px)),
                      collapse = ""))
            flush.console() # if the outptut is buffered, it will go immediately to the console
            return(cur)
        }
        return(dcur)
    }

    ## Start download
    succ=tryCatch(
        curlPerform(url=url, .opts=opt, curl=curl, writedata=f@ref,
                    ssl.verifypeer=!is.null(cert), cainfo=cert,
                    progressfunction=function(down,up)
                        dcur<<-dProgress(down, up, dcur, width),
                    noprogress=FALSE, headerfunction = headers$update),
        error = function(x) cat("Curl Error:", x$message, '\n'))
    cat('\n')
    if(redirect) cat (paste('Redirect to', url), '\n'); 
    close(f)
    if(is.null(succ)) headers=NULL else headers=headers$value()
    if(!is.null(succ) && !(headers["status"] %in% c('200', '301', '302'))){
        cat('Server Error\n')
        print(headers)
        succ=NULL
    }
    ## Follow location via recursion
    loc=ifelse(is.null(headers), NA, headers["Location"])
    if(!is.na(loc)) download.bin(url=loc, file, refr=refr, cert=cert, curl=curl) else
    return(list(succ=!is.null(succ), headers=headers))
}


###== File System ==


makePath=function(parent, child){    
### Chains parent-child paths managing '/' middle slashes 
## You don't have to rememeber if you need to add or not that slash

    ## No trail for parent 
    parent=sub('/$', '',  parent)
    parent=sub('\\\\$', '',  parent)

    ## No lead for child
    child=sub('^/', '',  child)
    child=sub('^\\\\', '',  child)

    ## Now the input is secure
    file.path(parent, child)

}

work.pt=function(dir=""){    
### Prefix dir path with work dir path
### Work dir path is set with global G$work

    makePath(G$work, dir)
}

root.pt=function(dir=""){
### Prefix dir path with BloomR root path relative to workdir.
### Currently Bloomr root name is "bloomR"

    makePath(G$branch, dir)
}


app.pt=function(dir=""){
### Prefix dir path with BloomR apps' path relative to workdir.
### App dir name depends on G$appname, its parent is BloomR root dir

    x=root.pt(G$appname)
    makePath(x, dir)
}

slisp.pt=function(dir=""){
### Prefix dir path with BRemacs site-lisp path relative to workdir.
    
    x=app.pt("bremacs/share/emacs/site-lisp")
    makePath(x, dir)
}



win.pt=function(path){
### Quote Windows-ize and make relative to currpath
### E.g. "rel path/to/sub dir" -> "\"abs path\\to\\/sub dir\""
### This function is used to send path to the shell therefore output is relative to currpath
    
    #op <- options("useFancyQuotes")
    op=options(useFancyQuotes = FALSE)
    path=work.pt(path)
    path=dQuote(path)
    options(op)
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
### if ASK flag is et, ask for delete confirmation, else just notify

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


existMake=function(dir, overwrite, ask, desc){
### If dir relative to workdir does not exist make it, otherwise might ask and skip creation 
### An empty-dir is considered non-existent. Note: if dir="" it is the workdir 

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


