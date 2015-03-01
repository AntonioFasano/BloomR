
###  BloomR source
###  Candidate to release. 

##   TODO
##   

##  Requirements:
##  XML and Rcurl packages. If missing it will tray to download and install them.
##  R should be able to connect to the Internet.
##  .Platform$OS.type == "windows"
## 
##  Usage:
##  Source this file and run:
##  makeBloomR("path\to\workDir")
##  You will get the BloomR dir in the work dir
##
##  Credits:
##  R-Portable*.exe from sourceforge.net/projects/rportable
##  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
##  Rbbg_*.zip from http://r.findata.org/bin/windows/contrib/
##  peazip  from http://sourceforge.net/projects/peazip
##  ahkscript http://ahkscript.org
##  Icon set Simplicio CC 3.0 by Neurovit: http://neurovit.deviantart.com
##   retrieved at https://www.iconfinder.com/icons/48750/direction_right_icon
##


#### Globals

## java alexkasko
G.javaurl="https://bitbucket.org/alexkasko/openjdk-unofficial-builds/downloads/"
G.javaurl.dom="https://bitbucket.org"
## win32
G.javaurl.bit="windows-i586-image.zip"
## win64
G.javaurl.bit="windows-amd64-image.zip"
G.javazip='openjdk'

## Bloomberg API for icedtea 
G.apiurl="http://cdn.gotraffic.net/open/blpapi_java_3.7.1.1.zip"
G.apizip="blpapi_java"

## Rbbg win32         
G.rbbgurl="http://r.findata.org/bin/windows/contrib/"
## Rbbg win64
G.rbbgurl="http://r.findata.org/bin/windows64/contrib/"
G.rbbgzip="rbbg"

## Ahkscript
G.ahkurl="http://ahkscript.org/download/1.1/Ahk2Exe111500.zip"
G.ahkzip="ahk"

## Web certificates
G.certurl='http://curl.haxx.se/ca/cacert.pem'
G.certfile=""

## Github
G.github="https://raw.githubusercontent.com/AntonioFasano/BloomR/master"
G.github.local=""

## Packages to download. Include dependencies! Case sensitive
G.packlist=" rJava  zoo  xts knitr XML RCurl bitops"

## SF items
G.pzip="peazip"
G.rport="rportable"
G.nsisurl='portableapps'
G.nsiszip='NSIS'

## Local path
G.work=""

## Shortcuts 
T=TRUE; F=FALSE


## Build the BloomR
## work: work dir path, absolute or relative to cur path
## tight: reuse downloaded material in workdir
## ndown: num of download attempts; default to 2 
## zip: if TRUE zip the BloomR output dir
## ask: if TRUE (default), it asks to if overwrite exisint workdir or BloomeR build
## For debug/test:
## deb: defaults to 1:6 to execute all steps build steps, modify to debug.
## gitsim: if set to a (abs or rel) local path, github downloads will be simultaed from the gitsim path
makeBloomR=function(work, tight=FALSE, ndown=2, zip=FALSE, ask=TRUE, deb=1:6, gitsim=FALSE){

    ## Set work dir
    if(!nzchar(work)) stop("Please, specify a work directory as first arg!")

    G.work<<-work
    
    ## Set git dir
    G.github.local<<-""
    if(gitsim!=FALSE && nzchar(gitsim))
        if (file.info(gitsim)$isdir)
            G.github.local<<-gitsim else {
                stop(gitsim, "is not an existing dir")}

    ## Set certificate local path
    G.certfile<<-makePath(G.work, 'cacert.pem')
    
    ## Windows?
    if(.Platform$OS.type != "windows") stop("Sorry, Bloomberg only exists for Windows and so BloomR.")
    
    ##  Check for required package
    if(!loadLib("RCurl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Step 1
    if(1 %in% deb) existMake('', overwrite=!tight, ask, paste("working dir:\n", G.work))
    
    ## Step 2
    if(2 %in% deb) downloads(tight, ndown)

    ## Step 3
    if(3 %in% deb) expand()

    ## Step 4
    if(4 %in% deb) bloomrTree(ndown)
        
    ## Step 5
    if(5 %in% deb) initScripts(ndown)

    ## Step 6
    if(6 %in% deb) {makeExe(ask,ndown); if(zip) makeZip(ask)}
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
        cat("\nUnable to load", lib, "library\n")
        return (FALSE)
    }
    return (TRUE)
}

### Get components
downloads=function(tight, ndown){

    overwrite=!tight
        
    ## Get certificates from curl site
    download.nice(G.certurl, 'cacert.pem', overwrite, ndown,
                  "Curl Certificates")

    ## peazip
    cback=function(){
        url=sfFirstbyProject(G.pzip, '[[:digit:]]') #get release dir 
        url=sfFirstbyUrl(url, "portable[^\"]*?WINDOWS")
        sfDirLink(url)
    }
    download.nice(cback, G.pzip, overwrite, ndown,
                  "Peazip files")
    
    ## R
    cback=function(){
        url=sfFirstbyProject(G.rport, 'Portable')
        url=sfFirstbyUrl(url, '[[:digit:]]')        
        url=sfFirstbyUrl(url, 'exe[^.]')        
        sfDirLink(url)
    }
    download.nice(cback, G.rport, overwrite, ndown,
                  "main R files")

    cback=function(){
        url=sfFirstbyProject(G.nsisurl, G.nsiszip)
        url=sfFirstbyUrl(url, 'Additional')
        url=sfFirstbyUrl(url, '[[:digit:]]')
        sfDirLink(url)
    }
    download.nice(cback, G.nsiszip, overwrite, ndown,
                  "NSIS")

    
#    desc="main R files"
#    mess.down(desc)
#    fpath=makePath(G.work, G.rport)
#    if(chk.write(fpath, overwrite, desc, stop=FALSE)){
#        url=sfFirstbyProject(G.rport, 'Portable')
#        url=sfFirstbyUrl(url, '[[:digit:]]')        
#        url=sfFirstbyUrl(url, 'exe[^.]')        
#        url=sfDirLink(url)
#        sfBDown(url, fpath)
#    }

    ## Openjdk
    download.nice(javaurl.ver, G.javazip, overwrite, ndown,
                  "Java files", cert=TRUE)

    ## Bloomberg API
    download.nice(G.apiurl, G.apizip, overwrite, ndown,
                  "Bloomberg API")
        
    ## CRAN packages
    existMake("packs", overwrite=!tight, ask=FALSE, "packages dir:")    
    packs= strsplit(gsub('(^ +)|( +$)', '', G.packlist), split=' +')[[1]]    
    for(pack in packs) # Loop over packs and download them 
        download.nice(cran.geturl(pack), makePath("packs", pack), overwrite, ndown,
                  pack)
    
    ## rbbg
    download.nice(rbbgurl.ver(), makePath("packs", G.rbbgzip), overwrite, ndown,
                  "rbbg files")
    
    ## ahkscript
    download.nice(G.ahkurl, G.ahkzip, overwrite, ndown,
                  "ahkscript")
    
}


### Expand components
expand=function(){
    
    ## peazip
    uzip(G.pzip, paste0(G.pzip,'.d'), 
          "Peazip binaries")
    
    ## R files
    to=makePath(G.work, paste0(G.rport, '.d')) # output dir    
    from=makePath(G.work, G.rport)             # source
    #chk.write(to, overwrite, "R file directory")
    if(is.path(to)) {
        message('\nDeleting exisitng R files.')
        del.path(to)
    }
    
    message('\nExtracting main R files: this may take a bit.')
    zexe=getzip(G.work)
    cmd=paste(wPath(to), wPath(from))
    cmd=paste0(wPath(zexe), ' x -aoa -r -o', cmd)
    ret=system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

    
    ## NSIS files
    to=makePath(G.work, paste0(G.nsiszip, '.d')) # output dir    
    from=makePath(G.work, G.nsiszip)             # source
    if(is.path(to)) {
        message('\nDeleting exisitng NSIS files.')
        del.path(to)
    }
    message('\nExtracting NSIS files.')
    zexe=getzip(G.work)
    cmd=paste(wPath(to), wPath(from))
    cmd=paste0(wPath(zexe), ' x -aoa -r -o', cmd)
    ret=system( cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

    ## openjdk
    uzip(G.javazip, paste0(G.javazip,'.d'),
          "Java binaries")

    ## Bloomberg API
    uzip(G.apizip, paste0(G.apizip,'.d'), 
          "API binaries")

    ## CRAN packages
    message('\nExpanding packages', '...')
    from=makePath(G.work, "packs")
    #chk.write(makePath(G.work, "lib.d"), overwrite)
    del.path(makePath(G.work, "lib.d"))
    ## Loop and extract packs
    for(pack in  dir(from) )
        uzip(makePath('packs', pack), 'lib.d',
              paste('R package', pack), delTarget=FALSE)    
    
    ## ahkscript
    uzip(G.ahkzip, paste0(G.ahkzip,'.d'),
          "ahkscript")

}

### Make BloomR directory tree
bloomrTree=function(ndown){

    message("Creating BloomR tree")
    existMake('bloomR', TRUE, FALSE, "main BloomR dir:")

    ## Copy R and make site direcory
    from=makePath(G.work, paste0(G.rport, '.d/App/R-Portable'))
    to=makePath(G.work, "bloomR/main")
    copy.dir(from, to, "main R files")
    makeDir(makePath(G.work, 'bloomR/main/site-library'), "BloomR library:")
    
    ## Copy java
    from=makePath(G.work, paste0(G.javazip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(G.work, paste0("bloomR/main/", G.javazip))
    copy.dir(from, to, "Java modules")
    unlink(makePath(to, 'src.zip'))

    ## Copy Bloomberg API
    from=makePath(G.work, paste0(G.apizip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(G.work, paste0("bloomR/main/", G.apizip))
    copy.dir(from, to, "Bloomberg API")

    ## Copy libs
    message("\nAdding R libraries")
    lib.f=makePath(G.work, 'lib.d')
    lib.t=makePath(G.work, "bloomR/main/library")
    for(lib in dir(lib.f)){
        from=makePath(lib.f, lib)  
        to=makePath(lib.t, lib)
        copy.dir(from, to)
    }

    ## Download manuals
    message("\nDownloading BloomR help resources")
    download.git("README.html", "bloomR/README.html", ,ndown)
    makeDir(makePath(G.work, "bloomR/help"), "BloomR help directory:")    
    download.git("bloomr.html", "bloomR/help/bloomr.html", ,ndown) 
    download.git("bloomr.pdf", "bloomR/help/bloomr.pdf", ,ndown)
    download.git("xlx.help.html", "bloomR/help/xlx.help.html", ,ndown)     
    download.git("xlx.help.pdf", "bloomR/help/xlx.help.pdf", ,ndown)
    
}


###== Boot functions ==

### Make etc/Rprofile.site from PROF()
initScripts=function(ndown){

    message("\nMaking etc/Rprofile.site and shared directory")

    ## Make new Rprofile.site and keep old
    p=capture.output(PROF)  # Get PROF function definition 
    p=p[-c(1, length(p))]   # Remove "function {", "}"
    prof.new=makePath(G.work, 'bloomR/main/etc/Rprofile.site')
    prof.nat=makePath(G.work, 'bloomR/main/etc/Rprofile.native')

    ## Append with Unix line endings
    if(!is.file(prof.nat)){
        file.copy(prof.new, prof.nat)                  
        con= file(prof.new, open="ab")
        writeLines(text=p, con=con)
        close(con)
    }
    
    ## Get bloomr.R and xlx.R from Github
    to=makePath(G.work, "bloomR/main/share/bloomr")    
    makeDir(to,"BloomR share directory:")
    download.git("bloomr.R", "bloomR/main/share/bloomr/bloomr.R", ,ndown)
    download.git("bloomr.sys.R", "bloomR/main/share/bloomr/bloomr.sys.R", ,ndown)
    download.git("xlx.R", "bloomR/main/share/bloomr/xlx.R", ,ndown)

    
    ## Make personal dir with some sample files
    makeDir(makePath(G.work, 'bloomR/mybloomr'), "personal directory:")
    download.git("res/semic.csv", "bloomR/mybloomr/semic.csv", ,ndown)
    download.git("res/tickers.csv", "bloomR/mybloomr/tickers.csv", ,ndown)
    download.git("res/tickers.eqt.csv", "bloomR/mybloomr/tickers.eqt.csv", ,ndown)
    download.git("tryme.R", "bloomR/mybloomr/tryme.R", ,ndown)


               

    
    ## Make R bootstrapper
    makeBoot(ndown)

}

### Make R bootstrapper
makeBoot=function(ndown){

    ## Boot string    
    bloomr.run="
EnvSet, HOME,       %A_ScriptDir%\\mybloomr
EnvSet, JAVA_HOME, %A_ScriptDir%\\main\\openjdk\\jre
;EnvSet, PATH,       %A_ScriptDir%\\main\\openjdk\\bin;%path%
Run, main\\bin\\x64\\Rgui.exe --internet2 LANGUAGE=en
"
    
    ## Make boot file
    ahkdir=makePath(G.work, paste0(G.ahkzip, '.d'))
    cat(bloomr.run, file=makePath(ahkdir, "bloomr.run"))
   
    ## Get icon from GitHub
    to=makePath(paste0(G.ahkzip, '.d'), "bloomr.ico")
    download.git("bloomr.ico", to, ,ndown)
    
    ## Make exe
    cat("Making BloomR executable\n")
    cd=normalizePath(ahkdir)
    cd= paste0('cd "', cd, '" &')
    run="Ahk2Exe.exe /in bloomr.run /icon bloomr.ico /bin \"Unicode 32-bit.bin\""
    shell(paste(cd, run), shell=Sys.getenv("COMSPEC"))    

    ## Move exe
    from=makePath(ahkdir, "bloomr.exe")
    to=makePath(G.work, "bloomr/bloomr.exe")
    file.rename(from, to)    
    
}

### "etc/Rprofile.site" source (braces on separate lines) 
PROF=function(){ #Keep this on separate line
    
    ## BloomR bootstrap
    ## ================
    
    cat("Current working directory is\n", getwd(), "\n")
    
    ## Set default repository
    local({r <- getOption("repos")
           r["CRAN"] <- "http://cran.r-project.org"
           options(repos=r)
       })    
    library("rJava")
    library("Rbbg")
    source(paste0(R.home("share"), "/bloomr/bloomr.R"))
    source(paste0(R.home("share"), "/bloomr/bloomr.sys.R"))
    source(paste0(R.home("share"), "/bloomr/xlx.R"))
    
    ## end BloomR----------
}


###== Utlities ==

### Exe and Zip distro 
makeExe=function(ask,ndown){

    message('\nCreating BloomR.exe installer')
    to=makePath(G.work, "BloomR.exe")
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)

    download.git("bloomr.nsi", "bloomr.nsi", ,ndown)
    nsi=makePath(G.work, 'bloomr.nsi')   
    nexe=makePath(G.work, paste0(G.nsiszip,'.d/App/NSIS/makensis.exe'))
    cmd=paste(wPath(nexe), "/v2", wPath(nsi))
    ret=system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}


makeZip=function(ask){

    message('\nCreating BloomR.zip')
    to=makePath(G.work, "BloomR.zip")
    if(is.path(to)) del.ask(to, ask, "already exists")    
    del.path(to)
    
    from=makePath(G.work, 'bloomR/.././bloomR/*')   # In 7z ./ removes dir prefix from archive     
    zexe=getzip(G.work)
    #cmd=paste("cmd.exe /c cd /D", wPath(G.work) , "&")
    cmd=paste(wPath(zexe), "a", wPath(to), wPath(from))
    ret=system(cmd, intern=FALSE, wait =TRUE, show.output.on.console =FALSE, ignore.stdout=TRUE) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}


###== Website helpers ==

### First SF project item matching regex filter (url made from prj name)
sfFirstbyProject=function (project, filtx, quiet=FALSE){
    if(!quiet) cat('Searching last', project, 'version on SF.net\n')
    url=paste0("http://sourceforge.net/projects/", project, "/files/")
    ref="http://sourceforge.net"    
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[@class='name']",  xmlGetAttr, "href")
    url=grep(filtx, url, value=TRUE)[1] 
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    return (url)
}

### First SF url item matching regex filter (url given from prj name)
sfFirstbyUrl=function (url, versionx, quiet=FALSE){
    if(!quiet) cat('Searching for version ', versionx, ' on\n', url, '\n')
    ref="http://sourceforge.net"
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=TRUE),
        "//a[@class='name']",  xmlGetAttr, "href")
    return (grep(versionx, url, value=TRUE)[1])
}

### Follow the direct-download link 
sfDirLink=function (url, quiet=FALSE){

    if(!quiet) cat('Find best mirror for\n', url, '\n')
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
    url=G.javaurl
    cat("Parsing page:\n", url, ' ...\n')
    if(!url.exists(url, ssl.verifypeer=FALSE, cainfo=G.certfile))
        stop("Unable to open java download page:\n", url)       
    href=getURL(url, ssl.verifypeer=FALSE, cainfo=G.certfile)   
    href=xpathSApply(htmlTreeParse(href, useInternalNodes=TRUE),
        "//a[@class='execute']", xmlGetAttr, "href")
    href=grep(paste0(G.javaurl.bit, "$"), href, value=TRUE)[1]
    paste0(G.javaurl.dom, href)
}

rbbgurl.ver=function(){
    url=G.rbbgurl
    cat("Parsing page:\n", url, ' ...\n')
    if(!url.exists(url)) stop("Unable to open rbbg page:\n", url)
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("^[[:digit:]]\\.[[:digit:]]", href, value=TRUE)
    url=paste0(url, href[length(href)])
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=TRUE), "//a", xmlGetAttr, "href")
    href=grep("\\.zip$", href, value=TRUE)
    paste0(url, href)
}




###== Donwload helpers ==

### Nice user info and overwrite managements for downloads 
download.nice=function(from, to, overwrite, ndown, desc="", cert=FALSE){

    if(desc=="") desc=sub('.+/', '', to)
    to=makePath(G.work, to)
    message("\nDownloading ", desc) 

    if(is.path(to)) {
        warn.path(to, "already exists.")
        if(!overwrite){
            message("Skipping action to preserve existing download.")
            return()
        }
    }

    ## Execute from callback 
    if(is.function(from)) from=from()
    message("from:\n", from)
    cert= if(cert != FALSE) G.certfile else  NULL

    ## Download ndown times and exit deleting file on errors
    for(i in 1:ndown)
        if(s<-download.bin(from, to, cert)$succ) break
    if(!s){
        unlink(to, force = TRUE)
        stop('\nDownload error')
    }
   
    
   # if(!download.bin(from, to, cert)$succ) {
   #     unlink(to, force = TRUE)
   #     stop('\nDownload error')
   # }
    
}

### Download html page with simple progress and stop on error
download.html=function(url, refr=NULL){
    
    if(!url.exists(url)) stop('\nCan\'t find page\n', url)
    ret=getURL(url, referer=refr, noprogress=FALSE, progressfunction=
           function(down,up) cat("\rBytes:", down[2]))
    cat("\n")
    ret
}

### Download from github or use local git
download.git=function(file, to, overwrite=TRUE, ndown, desc=""){

    if(!nzchar(desc)) desc=file
    
    ## remote git
    if(!nzchar(G.github.local)) {
        from=makePath(G.github, file)
        download.nice(from, to, overwrite=TRUE, ndown, desc, G.certfile)
        
    ## local git
    } else {
        cat('\nDownloading', desc, "\n")
        #if(!chk.write(to, overwrite, desc, stop=FALSE)) return()
        from=makePath(G.github.local, file)                       
        to=makePath(G.work, to)
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
    opt=list(referer=NULL); opt$referer =NULL 

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
                    ssl.verifypeer=FALSE, cainfo=cert,
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
    if(!is.na(loc)) download.bin(url=loc, file, refr, curl=curl) else
    return(list(succ=!is.null(succ), headers=headers))
}


###== File System ==


### Chains parent-child paths managing '/' middle slashes 
makePath=function(parent, child){    
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

### Conv: "my/sub dir" -> "\"my\\sub dir\""
wPath=function(path){
    op <- options("useFancyQuotes")
    options(useFancyQuotes = FALSE)
    path=dQuote(path)
    options(op)
    chartr("/", "\\", path)
}

### Path exists
is.path=function(path){
    path=sub("/$", "", path)
    file.exists(path)
}

### Path exists as a file
is.file=function(file){
    is.path(file) && !file.info(file)$isdir
}

### Path exists as a dir
is.dir=function(file){
    is.path(file) && file.info(file)$isdir
}

### Dir is empty
is.empty=function(dir){
### Also: T, if does not exist; F, if is a file
    if(is.file(dir)) return (FALSE)
    length(dir(dir, all.files=TRUE, no..=TRUE)) ==0
}

### Break if path not a file
chk.file=function(file, desc=""){
    if(!is.path(file)) stop(file, "\nis not a valid path")
    if(is.dir(file))   stop(file, "\nis a directory")
}

### Break if path not a dir
chk.dir=function(dir){
    if(!is.path(dir)) stop(dir, "\nis not a valid path")
    if(!is.dir(dir))  stop(dir, "is a file")
}
            
### Create a dir with optional desc message and stop on errors
makeDir=function(dir, desc=""){    
    if(nzchar(desc)) message("Creating ", desc )
    del.path(dir)
    dir.create(dir, showWarnings = FALSE)
    ## Unable to create? 
    if(!is.path(dir))
        stop("\nUnable to create ", desc, "\n", dir)
}

### Delete path and break on fail 
del.path=function(path){
    if(!is.path(path)) return()
    unlink(path,recursive=TRUE, force=TRUE) #  non-existent file is not an unlink failure
    if(is.path(path)) {
        Sys.sleep(1.5)
        if(is.path(path)) stop("\nUnable to access\n", path)
    }
}

### Warn on path existence and, if ASK flag set, ask for delete confrimation, else just notify
del.ask=function(path, ask, desc){

    warn.path(path, desc)
    
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

## Copy FROM directory in TO parent and rename TO as FROM
## If `to' dir exists delete it 
copy.dir=function(from, to, desc=""){
    if(nzchar(desc)) message("\nAdding ", desc)
    del.path(to)
    file.copy(from, dirname(to), recursive=TRUE)
    file.rename(makePath(dirname(to), basename(from)), to)
}

### If dir does not exist make it, otherwise:
### might ask and skip creation 
existMake=function(dir, overwrite, ask, desc){

    ## DIR is relative to G.work.
    ## An empty-dir is considered non-existent 

    ## Inform user with desc if any
    if(nzchar(desc)) message("\nCreating ", desc, '\n',  dir)

    dir=makePath(G.work, dir)
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

### Unzip making path relative to global workdir
### Stops on errors and inform used with a desc argument
uzip=function(from, to, desc, delTarget=TRUE){
    from=makePath(G.work, from)
    to=makePath(G.work, to)
    message('\nExpanding ', desc, '...') #cat('\nExpanding', desc, '...\n')
    chk.file(from)
    #chk.write(makePath(to, basename(from)), overwrite, desc)
    if(delTarget) del.path(to)
    if(length(unzip(from, exdir= to))==0)
        stop('\nUnable to extract perform extraction')  
}

### Get 7z.exe
getzip=function(work){   
    x= makePath(G.work, paste0(G.pzip,'.d'))
    makePath(x, paste0(dir(x), '/res/7z/7z.exe'))
}



### == Messages ==

warn.path=function(path, mess){
    message("Warning:\n", path, "\n", mess)
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
       

### Create a dir and exit if there is no right to overwrite and the dir exists
#makeDir=function(dir, overwrite, desc=""){
# 
#    ## Inform user with desc if any
#    if(nzchar(desc)) cat("\nCreating", desc, dir, "\n")
#    
#    ## Check writing rights 
#    if(!chk.write(dir, overwrite, desc)) return
# 
#    ## Go
#    if(overwrite) del.path(dir)    
#    dir.create(dir, showWarnings = FALSE)  
# 
#    ## Unable to create? 
#    if(!is.path(dir))
#        stop("\nUnable to create ", desc, dir)
#    
#}
#    

### Download binary file with SF refer
#sfBDown=function(url, file){
#    cat(paste('Downloading', url, '\n'))
#    if(!download.bin(url, file, refr="http://sourceforge.net" )$succ)
#        stop('\nDownload error')
#}

### Exsisting paths warn
warn.p=function(path, mess){
    cat("Warning:", mess.p(path, mess), "\n")
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
    cat("\nDownloading", desc, "\n")
}


