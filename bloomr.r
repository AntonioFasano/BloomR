
# Set the create the R/Bloomberg Portable dir
#
# Requirements
#  R-Portable*.exe from sourceforge.net/projects/rportable
#  blpapi_java*.tar from http://www.openbloomberg.com/open-api/
#  Rbbg_*.zip from http://r.findata.org/bin/windows/contrib/
#  peazip portable

#Usage:
# rPort=function("path\to\workDir")
#Set working dir


## Globals
pzip="peazip"; rport='rportable'

## java icedtea 
javaurl=paste0('https://bitbucket.org/alexkasko/openjdk-unofficial-builds/downloads/',
    'openjdk-1.7.0-u45-unofficial-icedtea-2.4.3-windows-i586-image.zip')
javazip='openjdk'
## API for icedtea 
apiurl="http://cdn.gotraffic.net/open/blpapi_java_3.7.1.1.zip"
apizip="blpapi_java"

certurl='http://curl.haxx.se/ca/cacert.pem'

T=TRUE; F=FALSE

makeBloomR= function(work, overwrite=TRUE, deb=1:4){
### work='work'
### deb: 0 don't overwrite, 1,2,3 step to execute 
                                        
    ##  Main component names    
    #overwrite=ifelse(0 %in% deb, F, T) 
    if(!loadLib("RCurl")) return(1)
    if(!loadLib("XML")) return(1)

    ## Step 1
    if(1 %in% deb)
        if(!makedir (work, overwrite)) return(1)
    
    ## Step 2
    if(2 %in% deb) downloads(work, overwrite)

    ## Step 3
    if(3 %in% deb) expand(work)

    ## Step 4
    if(4 %in% deb) bloomrTree(work)
    
    ## Step 5
    if(5 %in% deb) libScripts(work)
    
}

### Create a dir with overwrite options 
makedir=function(dname, overwrite){
    dir.create(dname, showWarnings = FALSE)
    x=dir(dname, all.files=TRUE)
    x=x[-which(x=='.')]
    x=x[-which(x=='..')]

    if (length(x)>0) if(!overwrite) {
        cat(dname, "\nAlready exists and is not empty\n")
        return (FALSE)
    } else 
        cat("Warning! ", dname, "\nalready contains\n", dir(dname), '\n')
    return (TRUE)
}

### Check/Load lib
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

### Get components
downloads=function(work, overwrite){

    ### Get certificates from curl site
    cert=makePath(work, 'cacert.pem')
    if(overwrite || !file.exists(cert))
        if(!download.bin(certurl, cert)$succ)  stop('\nDownload error')

    ###  Get peazip
    fpath=makePath(work, pzip)
    if(overwrite || !file.exists(fpath)){                     
        url=sfFirstbyProject(pzip, '[[:digit:]]') #get release dir 
        url=sfFirstbyUrl(url, "portable[^\"]*?WINDOWS")
        url=sfDirLink(url)
        sfBDown(url, fpath)   
    } else cat(pzip, 'already exists', '\n')
    
    ###  Get R-portable
    fpath=makePath(work, rport)
    if(overwrite || !file.exists(fpath)){
        url=sfFirstbyProject(rport, 'Portable')
        url=sfFirstbyUrl(url, '[[:digit:]]')        
        url=sfFirstbyUrl(url, 'exe[^.]')        
        url=sfDirLink(url)
        sfBDown(url, fpath)   
    } else cat(rport, 'already exists', '\n')

    ###  Get Java
    fpath=makePath(work, javazip)
    if(overwrite || !file.exists(fpath)){
        cat(paste('Downloading', javaurl, '\n'))
        if(!download.bin(javaurl, fpath, cert=cert)$succ)  stop('\nDownload error')
    }
    
    ###  Get Bloomberg API
    fpath=makePath(work, apizip)
    if(overwrite || !file.exists(fpath)){
        cat(paste('Downloading', apiurl, '\n'))
        if(!download.bin(apiurl, fpath)$succ)  stop('\nDownload error')
    }
    
}


expand=function(work){
    
    ## peazip
    file=makePath(work, pzip)
    exdir=makePath(work, paste0(pzip,'.d'))
    cat('Create', exdir, '\n') 
    if(length(unzip(file, exdir = exdir))==0)
        stop('\nError in peazip extraction')  
    pexe=makePath(exdir, paste0(dir(exdir), '/res/7z/7z.exe'))

    ## rportable 
    cat('Extract main R files: this may take a bit.','\n')
    cmd=paste0('-o', dQuote(makePath(work, paste0(rport, '.d')))) #output dir    
    cmd=paste(dQuote(pexe), 'x -aoa -r', cmd, dQuote(makePath(work, rport)))
    cmd=chartr("/", "\\", cmd)
    ret=system( cmd, intern=F, wait =T, show.output.on.console =F, ignore.stdout=T) 
    if(ret) stop(paste('\n', cmd, '\nreported a problem'))

    ## Java
    file=makePath(work, javazip)
    exdir=makePath(work, paste0(javazip,'.d'))
    cat('Create', exdir, '\n') 
    if(length(unzip(file, exdir = exdir))==0)
        stop('\nError in java extraction')

    ### Bloomberg API
    fpath=makePath(work, apizip)
    exdir=makePath(work, paste0(apizip,'.d'))
    cat('Create', exdir, '\n') 
    if(length(unzip(file, exdir = exdir))==0)
        stop('\nError in API extraction')  

    
 
}

### Make BloomR directory tree  
bloomrTree=function(work){
    
    makedir(makePath(work, 'bloomR'), overwrite)

    ## Move R
    from=makePath(work, paste0(rport, '.d/$_OUTDIR/R-Portable'))
    to=makePath(work, "bloomR/main")
    file.rename(from, to)
    makedir(makePath(work, 'bloomR/main/site-library'), overwrite)
    
    ## Move java
    from=makePath(work, paste0(javazip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(work, paste0("bloomR/main/", javazip))
    file.rename(from, to)
    unlink(makePath(from, 'src.zip'))

    ## Move Bloomberg API
    from=makePath(work, paste0(apizip,'.d'))
    from=makePath(from, dir(from))
    to=makePath(work, paste0("bloomR/main/", apizip))
    file.rename(from, to)
    unlink(makePath(from, 'src.zip'))

    ## Make etc/Rprofile.site 
    makeProfile(work)

    ## Create shortcuts
    makeShorts(work)
    
}

### Make etc/Rprofile.site from PROF()
makeProfile=function(work){
    
    p=capture.output(PROF)  # Get PROF funct definition 
    p=p[-c(1, length(p))]   # Remove "function {", "}"
    file=makePath(work, 'bloomR/main/etc/Rprofile.site')
    cat(p, sep='\n', fill=T, file=file, append=T)


}


### Make shortcuts to run R  
makeShorts=function(work){

    start="
set \"RPORT=%~dp0\"
cd %RPORT% 
set JAVA_HOME=%RPORT%main\\openjdk
path %JAVA_HOME%\\bin;%path%
set HOME=mystuff
start main\\bin\\i386\\Rgui.exe   --internet2 LANGUAGE=en
"
    cat(start, file=makePath(work, 'bloomR/BloomR.cmd'))

    ## make no-java short? 
}

### Make package install CLI and R scripts
libScripts=function(work){

    ## Libaries to install
    packs="   zoo  xts  "
    packs= strsplit(gsub('(^ +)|( +$)', '', packs), split=' +')
    
    ## main\firstRun.cmd to run r script
    first="bin\\i386\\Rscript.exe --vanilla site-library\\firstRun.r" 
    cat(first, file=makePath(work, '/bloomR/main/firstRun.cmd'))

    ## r script main/site-library/firstRun.r
    slib=makePath(work, '/bloomR/main/site-library/firstRun.r')
    cat('', file=slib) #clean file 
    a="\ninstall.packages('"
    b="', lib='site-library', repos='http://cran.r-project.org')"    
    for(i in packs)
        cat(paste0(a, i, b), file=slib, append=T)
    ##  install.packages('*', lib='site-library', repos="http://cran.r-project.org")

    ## Exit nicely
    str="
cat('Any key to exit ...')
readLines(con='stdin', 1)
"
    cat(str, file=slib, append=T)
   
}


### Create path removing trail/lead sep from parent/child
makePath=function(parent, child){

    ## No trail for parent 
    parent=sub('/$', '',  parent)
    parent=sub('\\\\$', '',  parent)

    ## No lead for child
    child=sub('^/', '',  child)
    child=sub('^\\\\', '',  child)
    
    file.path(parent, child)

}


### Download html page with simple progress and stop on error
download.html=function(url, refr=NULL){
    
    if(!url.exists(url)) stop('\nCan\'t find page\n', url)
    ret=getURL(url, referer=refr, noprogress=FALSE, progressfunction=
           function(down,up) cat("\rBytes:", down[2]))
    cat("\n")
    ret
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


### "etc/Rprofile.site" source (braces on separate lines) 
PROF=function(){ #Keep this on separate line
    
# === #
# FAS #
# === #



#Set default repository
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org"
       options(repos=r)
})
BloomR.lib="./FAS/library/"

#To install new packages use
#
#  install.packages("myPack", BloomR.lib)
#----------------------------------------




#BBG stuff
#=========
library("rJava")
library("Rbbg")

bbg.jar=function(){
	jarpath=paste0(R.home(), "/blpapi_java/bin")
        Sys.glob(file.path(jarpath,  "blpapi-[0-9]*.jar"))
    }



bbg.open=function() blpConnect(blpapi.jar.file=bbg.jar())
bbg.close=function(conn)  blpDisconnect(conn)

#end BBG stuff-------------------------------------------


delete.all= function() rm(list=ls(all=TRUE))

#Time extension functions
#=========================
`%+%` <- function(x,y) UseMethod("%+%")
`%+%.Date` <- function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
`%-%` <- function(x,y) UseMethod("%-%")
`%-%.Date` <- function(date,n) seq(date, by = paste (-n, "months"), length = 2)[2]
year=function(d) as.numeric(format(d, "%Y"))
`year<-`=function (d, value) {
    d <-as.Date(paste0(value, format(d, "-%m-%d")))}
month=function(d) as.numeric(format(d, "%m"))
`month<-`=function (d, value) {
    d <-as.Date(paste0(format(d, "%Y-"),  value, format(d, "-%d")))}
day=function(d) as.numeric(format(d, "%d"))
`day<-`=function (d, value) {
    d <-as.Date(paste0(format(d, "%Y-%m-"),  value))}
last.day=function(d){
    x=d %+% 1 #add a month
    day(x)=1  #set to 1st
    day(x-1)  #get day before
}
day.mod=function(d,n){
    as.Date(paste0(format(d, "%Y-%m-"), n))}

day.us=function(d1, d2){
    #set to first of month
    x1=day.mod(d1,1);x2=day.mod(d2,1);
    x=seq(x1, x2, by="1 month")
    #last day of each month in seq
    x=sapply(x, last.day)
    #count 31d-months
    x=length(which(x>30))
    #substract 1 for each 31d-month
    as.numeric(d2-d1-x)
}
#--------- end Time extensions

    
}  # Keep this on separate line




