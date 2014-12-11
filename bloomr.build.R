
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
javaurl="https://bitbucket.org/alexkasko/openjdk-unofficial-builds/downloads/"
javaurl.dom="https://bitbucket.org"
## win32
javaurl.bit="windows-i586-image.zip"
## win64
javaurl.bit="windows-amd64-image.zip"
javazip='openjdk'

## Bloomberg API for icedtea 
apiurl="http://cdn.gotraffic.net/open/blpapi_java_3.7.1.1.zip"
apizip="blpapi_java"

## Rbbg win32         
rbbgurl="http://r.findata.org/bin/windows/contrib/"
## Rbbg win64
rbbgurl="http://r.findata.org/bin/windows64/contrib/"
rbbgzip="rbbg"

## Ahkscript
ahkurl="http://ahkscript.org/download/1.1/Ahk2Exe111500.zip"
ahkzip="ahk"

## Web certificates
certurl='http://curl.haxx.se/ca/cacert.pem'
certfile=""

## Github
github="https://raw.githubusercontent.com/AntonioFasano/BloomR/master"
github.local=""

## Packages to download. Include dependencies! Case sensitive
packlist=" rJava  zoo  xts knitr "

pzip="peazip"; rport='rportable'

T=TRUE; F=FALSE

## Build the BloomR
## work: work dir path, absolute or relative to cur path
## overwrite: set to false to avoid overwrite an existing non empty work dir
## zip: if TRUE to zip the BloomR output dir
## For debug/test:
## deb: defaults to 1:6 to execute all steps build steps, modify to debug.
## gitsim: if set to a (abs or rel) local path, github downloads will be simultaed from the gitsim path
makeBloomR=function(work, overwrite=TRUE, zip=FALSE, deb=1:6, gitsim=FALSE){

    ## Set work dir
    if(!nzchar(work)) stop("Please, specify a work directory as first arg!")

    ## Set git dir
    github.local<<-""
    if(gitsim!=FALSE && nzchar(gitsim))
        if (file.info(gitsim)$isdir)
            github.local<<-gitsim else {
                stop(gitsim, "is not an existing dir")}

    ## Set certificate local path
    certfile<<-makePath(work, 'cacert.pem')
    
    ## Windows?
    if(.Platform$OS.type != "windows") stop("Sorry, Bloomberg only exists for Windows and so BloomR.")
    
    ##  Check for required package
    if(!loadLib("RCurl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Step 1
    if(1 %in% deb) makeDir(work, overwrite, "working dir:")
    
    ## Step 2
    if(2 %in% deb) downloads(work, overwrite)

    ## Step 3
    if(3 %in% deb) expand(work, overwrite)

    ## Step 4
    if(4 %in% deb) bloomrTree(work, overwrite)
        
    ## Step 5
    if(5 %in% deb) initScripts(work, overwrite)

    ## Step 6
    if(6 %in% deb && zip) makeZip(work, overwrite)
}

###== Main steps ==

### Load CRAN packs 
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
downloads=function(work, overwrite){

    ## Get certificates from curl site
    download.nice(work, certurl, 'cacert.pem', overwrite,
                  "Curl Certificates")

    ## peazip
    desc="Peazip files"
    down.mess(desc)
    fpath=makePath(work, pzip)
    if(chk.write(fpath, overwrite, desc, stop=FALSE)){
        url=sfFirstbyProject(pzip, '[[:digit:]]') #get release dir 
        url=sfFirstbyUrl(url, "portable[^\"]*?WINDOWS")
        url=sfDirLink(url)
        sfBDown(url, fpath)   
    }
    
    ## portabler
    desc="main R files"
    down.mess(desc)
    fpath=makePath(work, rport)
    if(chk.write(fpath, overwrite, desc, stop=FALSE)){
        url=sfFirstbyProject(rport, 'Portable')
        url=sfFirstbyUrl(url, '[[:digit:]]')        
        url=sfFirstbyUrl(url, 'exe[^.]')        
        url=sfDirLink(url)
        sfBDown(url, fpath)
    }

    ## Openjdk    
    download.nice(work, javaurl.ver(), javazip, overwrite,
                  "Java files")

    ## Bloomberg API
    download.nice(work, apiurl, apizip, overwrite,
                  "Bloomberg API")
        
    ## CRAN packages
    makeDir(makePath(work, "packs"), TRUE, "packages dir:")
    packs= strsplit(gsub('(^ +)|( +$)', '', packlist), split=' +')[[1]]    
    for(pack in packs) # Loop over packs and download them 
        download.nice(work, cran.geturl(pack), makePath("packs", pack), overwrite,
                  pack)
    
    ## rbbg
    download.nice(makePath(work, "packs"), rbbgurl.ver(), rbbgzip, overwrite,
                  "rbbg files")
    
    ## ahkscript
    download.nice(work, ahkurl, ahkzip, overwrite,
                  "ahkscript")
    
}


### Expand components
expand=function(work, overwrite){
    
    ## peazip
    uzip(work, pzip, paste0(pzip,'.d'), overwrite,
          "Peazip binaries")
    
    ## R files
    cat('\nExtract main R files: this may take a bit.','\n')

    to=makePath(work, paste0(rport, '.d')) # output dir    
    from=makePath(work, rport)             # source
    chk.write(to, overwrite, "R file directory")

    zexe=getzip(work)
    cmd=paste(wPath(to), wPath(from))
    cmd=paste0(wPath(zexe), ' x -aoa -r -o', cmd)
    ret=system( cmd, intern=F, wait =T, show.output.on.console =F, ignore.stdout=T) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

    ## openjdk
    uzip(work, javazip, paste0(javazip,'.d'), overwrite,
          "Java binaries")

    ## Bloomberg API
    uzip(work, apizip, paste0(apizip,'.d'), overwrite,
          "API binaries")

    ## CRAN packages
    cat('\nExpanding packages', '...\n')
    from=makePath(work, "packs")
    chk.write(makePath(work, "lib.d"), overwrite)
    if(overwrite) del.dir(makePath(work, "lib.d"))
    ## Loop and extract packs
    for(pack in  dir(from) )
        uzip(work, makePath('packs', pack), 'lib.d', overwrite,
              paste('R package', pack), delTarget=FALSE)    
    
    ## ahkscript
    uzip(work, ahkzip, paste0(ahkzip,'.d'), overwrite,
          "ahkscript")

}

### Make BloomR directory tree  
bloomrTree=function(work, overwrite){

    ## Make dir
    to=makePath(work, 'bloomR')
    chk.write(to, overwrite, "Creating BloomR tree")
    makeDir(to, overwrite, "main BloomR dir:") 

    ## Move R and make site direcory
    from=makePath(work, paste0(rport, '.d/$_OUTDIR/R-Portable'))
    to=makePath(work, "bloomR/main")
    move.dir(from, to)
    makeDir(makePath(work, 'bloomR/main/site-library'), overwrite, "BloomR library:")
    
    ## Move java
    from=makePath(work, paste0(javazip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(work, paste0("bloomR/main/", javazip))
    move.dir(from, to)
    unlink(makePath(to, 'src.zip'))

    ## Move Bloomberg API
    from=makePath(work, paste0(apizip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(work, paste0("bloomR/main/", apizip))
    move.dir(from, to)

    ## Move libs
    lib.f=makePath(work, 'lib.d')
    lib.t=makePath(work, "bloomR/main/library")
    for(lib in dir(lib.f)){
        from=makePath(lib.f, lib)  
        to=makePath(lib.t, lib)
        move.dir(from, to)
    }

    ## Download manuals
    cat("\nDownloading BloomR help resources\n")
    download.git(work, "README.html", "bloomR/README.html", overwrite)
    makeDir(makePath(work, "bloomR/help"), overwrite, "BloomR help directory:")    
    download.git(work, "bloomr.html", "bloomR/help/bloomr.html", overwrite)        
    download.git(work, "bloomr.pdf", "bloomR/help/bloomr.pdf", overwrite)
    download.git(work, "xlx.html", "bloomR/help/xlx.html", overwrite)        
    download.git(work, "xlx.pdf", "bloomR/help/xlx.pdf", overwrite)
        
}


###== Boot functions ==

### Make etc/Rprofile.site from PROF()
initScripts=function(work, overwrite){

    cat("Making etc/Rprofile.site and shared directory\n")

    ## Make Rprofile.site
    p=capture.output(PROF)  # Get PROF function definition 
    p=p[-c(1, length(p))]   # Remove "function {", "}"
    file=makePath(work, 'bloomR/main/etc/Rprofile.site')

    ## Append with Unix line endings
    con= file(file,open="ab")
    writeLines(text=p, con=con)
    close(con)
    
    ## Get bloomr.R and xlx.R from Github
    to=makePath(work, "bloomR/main/share/bloomr")    
    makeDir(to, overwrite, "BloomR share directory:")
    download.git(work, "bloomr.R", "bloomR/main/share/bloomr/bloomr.r", overwrite)
    download.git(work, "xlx.R", "bloomR/main/share/bloomr/xlx.r", overwrite)

    
    ## Make personal dir
    makeDir(makePath(work, 'bloomR/mybloomr'), overwrite, "personal directory:")

    ## Make R bootstrapper
    makeBoot(work, overwrite)

}

### Make R bootstrapper
makeBoot=function(work, overwrite){

    ## Boot string    
    bloomr.run="
EnvSet, HOME,       %A_ScriptDir%\\mybloomr
EnvSet, JAVA_HOME, %A_ScriptDir%\\main\\openjdk\\jre
;EnvSet, PATH,       %A_ScriptDir%\\main\\openjdk\\bin;%path%
Run, main\\bin\\x64\\Rgui.exe --internet2 LANGUAGE=en
"
    
    ## Make boot file
    ahkdir=makePath(work, paste0(ahkzip, '.d'))
    cat(bloomr.run, file=makePath(ahkdir, "bloomr.run"))
   
    ## Get icon from GitHub
    to=makePath(paste0(ahkzip, '.d'), "bloomr.ico")
    download.git(work, "bloomr.ico", to, overwrite)
    
    ## Make exe
    cat("Making BloomR executable\n")
    cd=normalizePath(ahkdir)
    cd= paste0('cd "', cd, '" &')
    run="Ahk2Exe.exe /in bloomr.run /icon bloomr.ico /bin \"Unicode 32-bit.bin\""
    shell(paste(cd, run), shell=Sys.getenv("COMSPEC"))    

    ## Move exe
    from=makePath(ahkdir, "bloomr.exe")
    to=makePath(work, "bloomr/bloomr.exe")
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
    source(paste0(R.home("share"), "/bloomr/xlx.R"))
    
    ## end BloomR----------
}


###== Utlities ==

### Zip distro 
makeZip=function(work, overwrite){

    cat('\nCreating BloomR.zip\n')
    to=makePath(work, "BloomR.zip")

    chk.write(to, overwrite)
    del.dir(to)
        
    from=makePath(work, 'bloomR/.././bloomR/*')   # In 7z ./ removes dir prefix from archive     
    exdir=makePath(work, paste0(pzip,'.d'))
    zexe=getzip(work)
    cmd=paste("cmd.exe /c cd /D", wPath(work) , "&")
    cmd=paste(wPath(zexe), "a", wPath(to), wPath(from))
    ret=system(cmd, intern=F, wait =T, show.output.on.console =F, ignore.stdout=T) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

}


###== Website helpers ==

### First SF project item matching regex filter (url made from prj name)
sfFirstbyProject=function (project, filtx, quiet=FALSE){
    if(!quiet) cat('Searching last', project, 'version on SF.net\n')
    url=paste0("http://sourceforge.net/projects/", project, "/files/")
    ref="http://sourceforge.net"    
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=T),
        "//a[@class='name']",  xmlGetAttr, "href")
    url=grep(filtx, url, value=T)[1] 
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    return (url)
}

### First SF url item matching regex filter (url given from prj name)
sfFirstbyUrl=function (url, versionx, quiet=FALSE){
    if(!quiet) cat('Searching for version ', versionx, ' on\n', url, '\n')
    ref="http://sourceforge.net"
    if(substr(url,1,1)=='/') url=paste0(ref, url)#relative to absolute
    page=download.html(url)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=T),
        "//a[@class='name']",  xmlGetAttr, "href")
    return (grep(versionx, url, value=T)[1])
}

### Follow the direct-download link 
sfDirLink=function (url, quiet=FALSE){

    if(!quiet) cat('Find best mirror for\n', url, '\n')
    ref="http://sourceforge.net"
    page=download.html(url, refr=ref)    
    url=xpathSApply(htmlTreeParse(page, useInternalNodes=T),
        "//a[@class='direct-download']",  xmlGetAttr, "href")
    return (url)
}

### Download binary file with SF refer
sfBDown=function(url, file){
    cat(paste('Downloading', url, '\n'))
    if(!download.bin(url, file, refr="http://sourceforge.net" )$succ)
        stop('\nDownload error')
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

###== Donwload helpers ==

### Nice user info and overwrite managements for downloads 
download.nice=function(work, from, to, overwrite, desc="", cert=FALSE){
    to=makePath(work, to)
    if(nzchar(desc)) desc=from
    cat("\nDownloading", desc, "\n")    
    if(!chk.write(to, overwrite, desc, stop=FALSE)) return()
    cat("from\n", from, "\n")
    cert= if(cert != FALSE) certfile else  NULL
    if(!download.bin(from, to, cert)$succ) stop('\nDownload error')
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
download.git=function(work, file, to, overwrite, desc=""){

    if(!nzchar(desc)) desc=file
    
    ## remote git
    if(!nzchar(github.local)) {
        from=makePath(github, file)
        download.nice(work, from, to, overwrite, desc, certfile)
        
    ## local git
    } else {
        cat('\nDownloading', desc, "\n")
        if(!chk.write(to, overwrite, desc, stop=FALSE)) return()
        from=makePath(github.local, file)                       
        to=makePath(work, to)
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
                    ssl.verifypeer=F, cainfo=cert,
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

### Break on path not a file
chk.file=function(file, desc=""){
    if(!is.path(file)) exit.p(file, desc, "is not a valid path")
    if(is.dir(file))   exit.p(file, desc, "is a directory")
}

### Break on path not a dir
chk.dir=function(dir){
    if(!is.path(dir)) exit.p(file, desc, "is not a valid path")
    if(!is.dir(dir))  exit.p(file, desc, "is a file")
}

### Check if we can overwrite non-empty dir and possibly stop
chk.write=function(path, over, desc="", stop=TRUE){
### Empty dir are overwritten without prompt
    
    if(nzchar(desc)) desc=paste(desc, '\n')

    ## Ret if non-exisitng path
    if(!is.path(path)) return(TRUE)
    
    ## Stop/warn if dir is non-empty and no right to overwrite
    ## note: a file is considered a non-empty dir
    if(is.path(path) && !is.empty(path)){   
        if(over){
            warn.p(path, desc, "already exists")}
        else {
            if(stop)  exit.p(path, desc, "already exists")
            warn.p(path, desc, "already exists\nSkipping action!")
            return(FALSE)
        }
    }
    return(TRUE)
}
       
### Delete dir and break on fail 
del.dir=function(dir){
    unlink(dir,recursive=TRUE)
    if(is.path(dir)) {
        Sys.sleep(1.5)
        if(is.path(dir)) stop("\nUnable to access\n", dir)
    }
}



### Create a dir with overwrite options and dir desc, in case of error 
makeDir=function(dir, overwrite, desc=""){

    ## Inform user with desc if any
    if(nzchar(desc)) cat("\nCreating", desc, dir, "\n")
    
    ## Check rights 
    chk.write(dir, overwrite, desc)

    ## Go
    ###########################if(del) del.dir(dir)    
    dir.create(dir, showWarnings = FALSE)  

    ## Unable to create? 
    if(!is.path(dir))
        stop("\nUnable to create ", desc, dir)
    
}
    

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

## Rename directory `from' -> `to'
## If `to' dir exists delete it 
move.dir=function(from, to){
    del.dir(to)
    file.rename(from, to)
}


### Unzip adding a prefix path to both source and destination
### Stops on errors and inform used with a desc asrgument
uzip=function(base, from, to, overwrite, desc, delTarget=TRUE){

    from=makePath(base, from)
    to=makePath(base, to)
    cat('\nExpanding', desc, '...\n')
    chk.file(from)
    chk.write(makePath(to, basename(from)), overwrite, desc)
    if(delTarget) del.dir(to)
    if(length(unzip(from, exdir= to))==0)
        stop('\nUnable to extract perform extraction')  
}

### Get 7z.exe
getzip=function(work){   
    x= makePath(work, paste0(pzip,'.d'))
    makePath(x, paste0(dir(x), '/res/7z/7z.exe'))
}



### == Messages ==

### Exisintg paths
mess.p=function(path, desc, mess){
 #   if(nzchar(desc)) desc=paste(desc, '\n')   
    paste0("\n", desc, path, "\n", mess)
}

### Exisintg paths stop
exit.p=function(path, desc, mess){ 
    stop(mess.p(path, desc, mess))
}

### Exisintg paths warn
warn.p=function(path, desc, mess){
    cat("Warning:", mess.p(path, desc, mess), "\n")
}

### Generic download info for custom download
down.mess=function(desc){
    cat("\nDownloading", desc, "\n")
}



## Get download versions    
javaurl.ver=function(url){
    url=javaurl
    cat("Parsing page:\n", url, ' ...\n')
    if(!url.exists(url, ssl.verifypeer=FALSE, cainfo=certfile))
        stop("Unable to open java download page:\n", url)       
    href=getURL(url, ssl.verifypeer=F, cainfo=certfile)   
    href=xpathSApply(htmlTreeParse(href, useInternalNodes=T),
        "//a[@class='execute']", xmlGetAttr, "href")
    href=grep(paste0(javaurl.bit, "$"), href, value=TRUE)[1]
    paste0(javaurl.dom, href)
}

rbbgurl.ver=function(){
    url=rbbgurl
    cat("Parsing page:\n", url, ' ...\n')
    if(!url.exists(url)) stop("Unable to open rbbg page:\n", url)
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=T), "//a", xmlGetAttr, "href")
    href=grep("^[[:digit:]]\\.[[:digit:]]", href, value=TRUE)
    url=paste0(url, href[length(href)])
    href=xpathSApply(htmlTreeParse(url, useInternalNodes=T), "//a", xmlGetAttr, "href")
    href=grep("\\.zip$", href, value=TRUE)
    paste0(url, href)
}


   
