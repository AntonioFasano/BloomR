
### BloomR admin functions

quit <- function(save = "no", status = 0, runLast = TRUE) base:::quit(save, status, runLast)
q    <- function(save = "no", status = 0, runLast = TRUE) base:::quit(save, status, runLast)


.br.getLatex.pack=function(pname, ipacks=NULL){
### Install a LaTeX Package, via  MiKTeX mpm --install
## ipacks (optional) is the list of installed packages.
## Used to speed-up if calling the function many times 
    
    mpm <- dbr.brmain("latex/texmfs/install/miktex/bin/x64/mpm.exe")
    if(!file.exists(mpm)) stop(paste('Unable to find:\n', mpm))

    if(is.null(ipacks)) ipacks=.br.getLatex.packList(inst=TRUE)
    if(pname %in% ipacks) {
        message(shQuote(pname), ' already installed. Skipping.')
        return()
    }

    message('Installing package ', shQuote(pname))
    pack=paste0("--install=", pname)
    out=system(paste(.br.wpath(mpm), pack), intern = TRUE, invisible = FALSE)
    if(!.br.getLatex.packCheck(pname)) stop("Unable to install LaTeX package ", shQuote(pname))

}


.br.getLatex.packList=function(inst=FALSE){
### Get list of all packages from MiKTeX mpm --list
## If inst=TRUE, give vector of names of installed ones

    mpm <- dbr.brmain("latex/texmfs/install/miktex/bin/x64/mpm.exe")
    if(!file.exists(mpm)) stop(paste('Unable to find:\n', mpm))
    out=system(paste(.br.wpath(mpm), '--list'), intern = TRUE, invisible = FALSE)
    if(inst) {
        x=grep('^i', out, value=TRUE)
        sapply(strsplit(x, ' +'), `[`, 4)
    } else out
}


.br.getLatex.packCheck=function(pname) # Check if a package is installed via MiKTeX mpm --list
    pname %in% .br.getLatex.packList(inst=TRUE)    

.br.wpath=function(s) # Convert path Unix->Win and quote
    shQuote(gsub('/', '\\\\',  s)) 


    
download_bin <- function(url, file){
### Download binary file returning SUCCESS. Based on curl

    library(curl)

    s <- tryCatch(
        curl_download(url, file, quiet = FALSE),
        error = function(x) cat("\nDownloading error:\n", x$message, '\n'),
        finally=cat("\n"))
    ifelse(is.null(s), FALSE, TRUE)
}



#.br.testBR=function(logfile=NULL){ # Several tests (at boot)
# 
#    ## Create log object 
#    log=dbr.log(logfile)
# 
#    ## Test JVM path
#    if(Sys.getenv('JAVA_HOME') == '') log$err("JAVA_HOME enviroment variable is empty.")
#    if(!dir.exists(Sys.getenv('JAVA_HOME')))
#       log$err("Can't JAVA_HOME directory\n", Sys.getenv('JAVA_HOME'))
#    else
#        log$add("JAVA_HOME directory is:\n\t", Sys.getenv('JAVA_HOME'))
# 
#    ## Test required packages
#    x=sapply(c('rJava', 'xts'),
#        function(pkg)
#            if(length(find.package(pkg, quiet=TRUE))==0)  log$err(pkg, "package not found"))
# 
#    ## Test bbg jar
#    ret=dbr.jar()
#    if(!file.exists(ret)) log$err("Bloomberg jar", ret, "not found.")
# 
#}


#br.getLatexAddons=function(){
#### Download LaTeX distro, Pandoc and knitr required LaTeX packages
# 
#    require(XML)
#    require(curl)
# 
#    ## Latex variables 
#    laturl="http://miktex.org/portable"
#    latsize=700   # min space in mega
#    latdir=dbr.brmain("latex")
#    latinst=paste0(latdir, "/mikport.exe")
#    latbin=paste0(latdir, "/texmfs/install/miktex/bin/latex.exe")
#    ## Check connection
#    if(!has_internet())
#        stop("It seems you have no internet connection")
# 
#    ## Check space
#    xx=paste("cmd /c dir", shQuote(R.home()), "/-c")
#    x=rev(strsplit(rev(system(xx, intern=TRUE))[1], " ")[[1]])[3]
#    x=as.numeric(x)    
#    if(x/1000^2 < latsize)  stop("Not enough space on your drive")
#    
# 
#    latlnk="http://mirrors.ctan.org/systems/windows/miktex/setup/windows-x86/miktex-portable.exe"
#    
#    ## Create Latex dir
#    if(file.exists(latdir))
#        unlink(latdir, recursive = TRUE, force = TRUE)
#    if(file.exists(latdir))
#        stop(paste("Unable to remove existing\n", latdir, "\nPlease, exit BloomR and try again."))
#    dir.create(latdir)
# 
#    ## Start download
#    message("Downloading ", latlnk)
#    if(!download_bin(latlnk, latinst)){
#        stop("An error occured while downloading LaTeX")
#    } else {
#        out  <-  system(paste(.br.wpath(latinst), "-y"), intern = TRUE, invisible = FALSE)   
#    }
# 
#    if(!file.exists(latbin))
#        stop(paste("An error occurred while extracting\n", latinst,
#                   "\nPlease, exit BloomR and try again."))
#        
#    ## Add extra packages
#    lpacks=c(
#        'fancyvrb',
#        'microtype',
#        'mptopdf',
#        'upquote',
#        'url',
#        'parskip', 'framed', 'titling', 'booktabs', 'etoolbox'
#        )  
#    x=sapply(lpacks, .br.getLatex.pack, .br.getLatex.packList(inst=TRUE))
# 
#        
#    ## Download Pandoc  
#    br.getPandoc()
# 
#}


#br.getPandoc=function(){
#### Download Pandoc  
# 
#    require(XML)
#    require(curl)
# 
#    ## Pandoc variables 
#    panurl = "https://github.com/jgm/pandoc/releases"
#    pansize = 100
#    pandir = dbr.brmain("pandoc")
#    paninst = paste0(pandir, "/pandocSetup.exe")
#    panbin = paste0(pandir, "/bin/pandoc.exe")
# 
#    ## Check connection
#    if (!has_internet()) stop("It seems you have no internet connection")
# 
#    ## Check space
#    xx = paste("cmd /c dir", shQuote(R.home()), "/-c")
#    x = rev(strsplit(rev(system(xx, intern = TRUE))[1], " ")[[1]])[3]
#    x = as.numeric(x)
#    if (x/1000^2 < pansize) 
#        stop("Not enough space on your drive")
# 
# 
#    ## Get download link
#    req <- curl_fetch_memory(panurl, new_handle())
#    req <- rawToChar(req$content)        
#    lnks = xpathSApply(htmlTreeParse(req, useInternalNodes = TRUE), 
#        "//a", xmlGetAttr, "href")
#    lrel =grep("pandoc-[.0-9]+-windows.msi$", lnks, ignore.case=TRUE, value=TRUE)[1] 
#    if (!nzchar(lrel)) 
#        stop("I can't find a parsable Pandoc download.") 
# 
#    panlnk = paste0("https://github.com", lrel)
# 
#    ## "Forbidden" status message is expected for binaries from url.exists().
#    ## A bad value would be " "Not Found"  
#    ##if(url.exists(panlnk, ssl.verifypeer = FALSE, .header=TRUE)['statusMessage'] !=  "Forbidden")
#    ##    stop("Unable to download\n", panlnk) 
# 
#    ## Create Pandoc dir
#    if (file.exists(pandir)) 
#        unlink(pandir, recursive = TRUE, force = TRUE)
#    if (file.exists(pandir)) 
#        stop(paste("Unable to remove existing\n", pandir, "\nPlease, exit BloomR and try again."))
#    dir.create(pandir)
# 
#    ## Start download
#    message("Downloading ", panlnk)
#    if (!download_bin(panlnk, paninst)) {
#        stop("An error occured while downloading Pandoc")
#    }
#    else {
#        cmd = .br.wpath(paste0(pandir, "/temp"))
#        cmd = paste0("TARGETDIR=", cmd)
#        cmd = paste("msiexec /qb /a", .br.wpath(paninst), cmd)
#        out <- system(cmd, intern = TRUE, invisible = FALSE)
#        file.rename(paste0(pandir, "/temp/pandoc"), paste0(pandir, 
#            "/bin"))
#        unlink(paste0(pandir, "/temp"), recursive = TRUE, force = TRUE)
#    }
#    if (!file.exists(panbin)) 
#        stop(paste("An error occurred while extracting\n", paninst, 
#            "\nPlease, exit BloomR and try again."))
#}


#.br.getPandoc.release=function(){
### Returns a vector with:
### the latest Pandoc release as a character and
### TRUE if there is a Windows executable for that release.
# 
#    require(XML)
#    require(curl)
#    panurl = "https://github.com/jgm/pandoc/releases/latest"
# 
#    ## Check connection
#    if (!has_internet()) stop("It seems you have no internet connection")
# 
#    ## Get redirect last release link 
#    req <- curl_fetch_memory(panurl, new_handle())
#    req <- rawToChar(req$content)        
#    panlnk=xpathSApply(htmlTreeParse(req, useInternalNodes=TRUE), "//a",  xmlGetAttr, "href")
#    lrel=rev(strsplit(panlnk, '/')[[1]])[1] # Version name inside link 
# 
#    ## Get download link
##    x=getURL(panlnk, ssl.verifypeer = FALSE)
#    req <- curl_fetch_memory(panlnk, new_handle())
#    req <- rawToChar(req$content)        
#    lnks = xpathSApply(htmlTreeParse(req, useInternalNodes=TRUE), "//a",  xmlGetAttr, "href")
#    panlnk=grep(paste0(lrel, '-windows'), lnks, value=TRUE)
# 
#    ## Return version and if win exe 
#    winexe=ifelse(length(panlnk)==0, FALSE, TRUE)
#    c(lrel, winexe)
# 
# }

    
#download.bin=function(url, file, refr=NULL, cert=NULL, curl = NULL){
#### Download binary file returning list(succ, headers) based on RCurl
#### Based on outdated RCurl so likely to be rmoved. See download_bin for curl
# 
#    library(RCurl)
# 
#    redirect=!is.null(curl)
#    if(is.null(curl)) curl = getCurlHandle()
#    f = CFILE(file, mode="wb")
#    headers =  basicHeaderGatherer()
#    width= getOption("width") - 25  # output width (dots + data)
#    dcur=0  # download status
#    ## Referer
#    opt=list(referer=NULL); opt$referer =NULL 
# 
#    ## Callback function for curlPerform
#    dProgress=function(down, up, dcur, width){
#        total=as.numeric(down[1]) # Total size as passed from curlPerform
#        cur=as.numeric(down[2])   # Current size as passed from curlPerform
#        x=cur/total
#        px= round(100 * x)
#        if(total==0)  x=cur
#        ## if(!is.nan(x) &&  px>60) return(dcur) # Just to debug at 60%
#        if(!is.nan(x) && cur!=dcur){
#            sc=rev(which(total>= c(0, 1024^1, 1024^2, 1024^3)))[1]-1 # scale stats to total
#            if(x>1)  { #Download size unknown 
#                total<-px<-NA
#                x=0
#                sc=rev(which(cur>= c(0, 1024^1, 1024^2, 1024^3)))[1]-1 # scale to current
#            }
#            lb=c('B', 'KB', 'MB', 'GB')[sc+1]
#            wx= round(width * x) # line width as as percent download
#            cat(paste(c(         # Print |, dots if any, stats 
#                        "\r  |", rep.int(".", wx), rep.int(" ", width - wx),
#                        sprintf("| %g%s of %g%s %g%%",
#                                round(cur/1024^sc, 2), lb, round(total/1024^sc, 2), lb, px)),
#                      collapse = ""))
#            flush.console() # if the outptut is buffered, it will go immediately to the console
#            return(cur)
#        }
#        return(dcur)
#    }
# 
#    ## Start download 
#    succ=tryCatch(
#        curlPerform(url=url, .opts=opt, curl=curl, writedata=f@ref,
#                    ssl.verifypeer=F, cainfo=cert,
#                    progressfunction=function(down,up)
#                        dcur<<-dProgress(down, up, dcur, width),
#                    noprogress=FALSE, headerfunction = headers$update),
#        error = function(x) cat("Curl Error:", x$message, '\n'))
#    cat('\n')
#    if(redirect) cat (paste('Redirect to', url), '\n'); 
#    RCurl:::close(f)
#    if(is.null(succ)) headers=NULL else headers=headers$value()
#    if(!is.null(succ) && !(headers["status"] %in% c('200', '301', '302'))){
#        cat('Server Error\n')
#        print(headers)
#        succ=NULL
#    }
#    ## Follow location via recursion
#    loc=ifelse(is.null(headers), NA, headers["Location"])
#    if(!is.na(loc)) download.bin(url=loc, file, refr, curl=curl) else
#    return(list(succ=!is.null(succ), headers=headers))
#}



