
### BloomR admin functions


### Download LaTeX distro 
br.getLaTeX=function(){

    require(XML)
    require(RCurl)

    ## Latex variables 
    laturl="http://miktex.org/portable"
    latsize=600   # min space in mega
    latdir=R.home("latex")
    latexe=paste0(latdir, "/mikport.exe")
    latbin="C:/binp/R-Portable/App/R-Portable/latex/miktex/bin/latex.exe"

    ## Check connection
    if(!is.character(getURL("www.google.com")))
        stop("It seems you have no internet connection")

    ## Check space
    xx=paste("cmd /c dir", shQuote(R.home()), "/-c")
    x=rev(strsplit(rev(system(xx, intern=TRUE))[1], " ")[[1]])[3]
    x=as.numeric(x)    
    if(x/1000^2 < latsize)  stop("Not enough space on your drive")
    
    ## Get download link 
    x=getURL(laturl) 
    latlnk = xpathSApply(htmlTreeParse(x, useInternalNodes=TRUE),
        "//a[@class='dllink']",  xmlGetAttr, "href")
    if(!nzchar(latlnk))  stop("I can't find a parsable LaTeX download")

    ## Create Latex dir
    if(file.exists(latdir))
        unlink(latdir, recursive = TRUE, force = TRUE)
    if(file.exists(latdir))
        stop(paste("Unable to unlink\n", latdir, "\nPlease, exit BloomR and try again."))
    dir.create(latdir)

    ## Start download
    if(!download.bin(latlnk, latexe)$succ){
        stop("An error occured while downloading LaTeX")
    } else {
        out  <-  system(paste(latexe, "-y"), intern = TRUE, invisible = FALSE)   
    }

    if(!file.exists(latbin))
        stop(paste("An error occurred while extracting\n", latexe,
                   "\nPlease, exit BloomR and try again."))
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



