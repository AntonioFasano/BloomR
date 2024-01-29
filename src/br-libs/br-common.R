## Two-step knitting
## -----------------
## The first step generatates (purls) the BloomR lib.
## The second step, thanks to step1 functions, runs demos and generate docs,
##  
## Note: Demos chunks need lib functions from step 1,
##       unless we put all demos at the end, after lib functions,
##       which we don't like. 
##  
## First Step
## ----------
## 1. Knit chunks tagged with opts.label='brfuncs' and step2=F. This will:
##    a) make <lib>.R, whose functions are stored in <lib> environment (via store()),
##    b) load functions in tagged chunks to be used in step2 demos.
##     
## 2. Generate <lib>.tmp.Rmd with auto-generated argument descriptions. 
##    Descs are extracted from special formatted comments by parseargs().
##  
## Second Step
## -----------
## 1. Knit chunks in <lib>.tmp.Rmd tagged with opts.label="demo*" and step2=T.
##    This runs the demo code and generate <lib>.md with code examples.
## 2. Add the TOC to <lib>.md vai topics().
## 3. Render to html/PDF
##  
## See "setup" chunk for definition of  "demo*" option templates.
## TODO (cf bloomr.Rmd) For beta code, source(".../bloomr.R") is usually required to execute step2. See "betaonly" chunk.


## TO CHECK
## To source in Rmds consider
## source("your-script.R", local = knitr::knit_global())
# or sys.source("your-script.R", envir = knitr::knit_global())

FRAMES <- sys.frames()
SCRIPTPATH <- NULL
#THISLIB <- NULL

knit.twice <- function(this.file) {# knit this Rmd

#    ## Polymode tends to start R synced with git root dir.
#    ## We check the Rmd path is absolute or as expected    
#    GITPOS <- "/src/br-libs"
#    abspt <- function(pt) unlist(strsplit(pt, split="/"))[[1]] == "" || regexpr("^.:(/|\\\\)", pt) != -1L
#    is.rel <- !abspt(this.file)
#    bad.dir <- sub(paste0("^", dirname(dirname(getwd()))), "", getwd()) !=  GITPOS
# 
#    ## Try to fix a bad path, when not abolute
#    if(bad.dir && is.rel) 
#        if(dir.exists(file.path(normalizePath("."), "src/br-libs"))) {
#            setwd("src/br-libs") # Polymode
#        } else {
#            stop("Rmd file expected to be in '", GITPOS, "'. The given absolute path is\n",
#                 normalizePath(this.file), "\nUse an absolute path or check 'GITPOS' var in knit.twice()")
#        }

    SCRIPTPATH <- script.path()
    assign("THISLIB", get("THISLIB", parent.frame()), knitr::knit_global())
    ## other possibiliteis could be (not testes) when you soruce this file 
    ##  source("br-common.R", local = knitr::knit_global())
    
    ## Keep this.file arg as-is if absolute, or adjust to this script dir.
    ## Then cd script
    abspt <- function(pt) unlist(strsplit(pt, split="/"))[[1]] == "" || regexpr("^.:(/|\\\\)", pt) != -1L
    is.rel <- !abspt(this.file)
    if(is.rel) this.file <- file.path(dirname(SCRIPTPATH), basename(this.file))
    if(!file.exists(this.file)) stop("Unable to find:\n", normalizePath(this.file))
    newdir <- normalizePath(dirname(this.file))
    olddir <- normalizePath(setwd(newdir))
    this.file <- basename(this.file)

    
    ## In BloomR use the shipped pandoc/LaTeX
    is.bloomr <- Sys.getenv("bloomr_branch") != ""
    wp <- function(expr) # Execute expression with modified PATH
        if(is.bloomr) .br.pathexe(substitute(expr), quiet = FALSE) else expr

    ## Prepare to knit
    knitr::opts_chunk$set(tidy.opts=list(width.cutoff=60))
    fse <- tools:::file_path_sans_ext(this.file)

    ## Step 1 
    step2 <- FALSE
    knitr::knit(this.file)
    parseargs(paste0(fse, '.Rmd'), paste0(fse, '.tmp.Rmd'))
    
    ## Step 2 
    step2 <- TRUE
    knitr::knit(paste0(fse, '.tmp.Rmd'), output=paste0(fse, '.md'))
    topics(paste0(fse, '.Rmd'), paste0(fse, '.md'))
    wp(rmarkdown::render(paste0(fse, '.md'), "html_document"))
    wp(rmarkdown::render(paste0(fse, '.md'), rmarkdown::pdf_document()))
    unlink(paste0(fse, '.tmp.Rmd'))    
    file.rename(paste0(fse, '.tmp.R'), paste0(fse, '.R'))    

    ## Fix issue with purl commenting everything
    txt <- readLines(paste0(fse, ".R"))
    txt <- sapply(txt, function(line) sub("^#  ", "", line))
    writeLines(txt, paste0(fse, ".R"))

    ## Inform on new wdir
    if(newdir != olddir) message("\nNOTE: Workdir was adjusted to\n", newdir, ".")


    ## if with markdown lib & pandoc (but topics would need a rewrite):
    ## shell("pandoc bloomr.md -o bloomr.pdf", shell=Sys.getenv("COMSPEC"))
    ## markdownToHTML("bloomr.md", "bloomr.html") 
    ## markdownToHTML("README.md", "README.html")
    
}
## In first eval = F chunk:
## THISLIB <- "bloomr.time"
## source(<thisfile>)
## knit.twice("bloomr-time.Rmd")



doc.setup <- function() { quote({
### Chunk templates and defaults
    knitr::opts_chunk$set(echo=TRUE)
    OTS <- knitr::opts_template$set
    ## Step1 template: Load in memory and copy to <lib>.R
    OTS(brfuncs=   list(purl=TRUE,  eval=!step2,   include=FALSE))
    ## Step1 template: Load in memory and copy to <lib>.R
    OTS(demofull= list(purl=FALSE, eval=step2,  include=TRUE))  # run with code + output   
    OTS(demorun=  list(purl=FALSE, eval=step2,  include=TRUE, echo=FALSE)) # run with output, but no code 
    OTS(demohid=  list(purl=FALSE, eval=step2,  include=FALSE)) # run only (without code or output)
    OTS(democode= list(purl=FALSE, eval=FALSE,  include=TRUE))  # show code only, no run  

    ## Purl hook (suggested in the past, but probably no necessary today)
    knitr::knit_hooks$set(purl = knitr::hook_purl)
})}



### Store br.* objects in dedicated namespace
store.chunk <- list(
    ## knit.twice will
    asgnlib = expression(
        assign(THISLIB, new.env(parent=asNamespace("stats")))
    ), 

    ## func: store(func);  var: store("var")
    store = function(sym, reg = FALSE) {
        if(is.function(sym)) {
            name <- deparse(substitute(sym)) 
            val <- sym
        } else {
            name <- sym
            val <- get(sym)
        }
 
        assign(name, val, envir = get(THISLIB))
        if(reg) {
            ## name  <- "%+%.Date"
            mtcs <- regmatches(name, regexec("(.+)(\\.)(.+)", name))[[1]]
            genname <- mtcs[2]
            class   <- mtcs[4]
            method  <- mtcs[1]
            registerS3method(genname, class, method, get(THISLIB))
            ## registerS3method("%+%", "Date", "%+%.Date", bloomr.time)
        }
        rm( list=name, envir=parent.frame())
    }
)


store.parse <- function(libname) { # makes store.chunk list into a text to feed a chunk
### USAGE ```{r store, code = store.parse(<libname>) ...}

    with(store.chunk, {
        c(paste("THISLIB", "<-", shQuote(libname)),
          as.character(asgnlib),
          "store <- ",
          deparse(dput(store)))
    })
}

lib.attach <- function() {# Make visible br.* in bloomr env and base ns
### To feed the chunk: ```{r attach, code = lib.attach() ...}
    
    expression({
        attach(get(THISLIB), name = THISLIB)
        rm(store)
        rm(list=c(THISLIB, "THISLIB"))
    }) |>
        as.character()
}

### Generate topics index 
read.head <- function(rmdfile) {x=readLines(rmdfile); x[grep("^=+", x)-1]}
topics <- function(rmdfile, mdfile){
    x <- read.head(rmdfile)
    x=sub("\\{", "]\\(", x )
    x=sub("\\}", "\\)", x )
    idx=paste0("[", x, "   ")
    idx=c("\nR topics documented:", "-----------", idx)
    txt <- readLines(mdfile)
    yaml.end=grep("^---", txt)[2] 
    txt=c(txt[1:yaml.end], idx, txt[(yaml.end+1):length(txt)])
    writeLines(txt, mdfile)
}

### Auto arguments
parseargs <- function(rmdfile, tmpfile) {
## Generate a new tmp Rmd with BloomR function arguments descriptions.
##    These are embedded in comments preceding (or in line with) the arguments and detected via a magic prefix tag
## The magic tag is hash (#) followed by a quote ('), henceforth denoted as HQ. In fact, to avoid side effects,
##    I can't explicitly write the magic tag. 
## Description comments have the format "HQ comment...". They can be in line with or precede the related arguments.
##    Standalone desc comments (i.e. non-inline) should immediately precede their argument, i.e. no-blank lines.
##    To break long lines, you can stack standalone comments above the argument, again without blank lines in the middle.
## Once extracted from comments, the actual markdown description is written when a special placeholder is met.
##    The placeholder is "HQ @args". You are expected to insert it in the markdown following the function and 
##    it will be replaced with the descriptions extracted from the comments in the function definition
##    HQ tag is always followed by a space (not a tab). Pay attention to casual use to avoid side effects. 
## Here is an example (replace HQ tag with hash and quote): 
##    
##    foo
##    ===
##    An important function.
##     
##    Usage
##    ------
##        foo(x,y)
##     
##      
##    Arguments
##    ----------
##     
##    HQ @args
##       
##    ```{r foo OPTIONS}      
##    foo <- function(
##                    HQ This is an important argument, and
##                    HQ it deserves special attention.
##                    x,
##                    y  HQ That's important too.
##                    ){
##     
##        BODY
##    }
##    ```

    
    txt=readLines(rmdfile)
    HQ=paste0("#", "'")

    ## Outer HQ comments
    ## -----------------
    ocrex.loose=sprintf("^ *%s +", HQ)   #-> " *HQ +"
    ocrex=sprintf("%s+[^@]", ocrex.loose) #-> " *HQ ++[^@]", ++ need perl

    ## Test standard outer comments in the middle of HQ comments
    x= grep("^ *#[^']", txt)
    if(length(x)) {
        if(x[1]==1) x=x[-1]
        if(x[length(x)]==length(txt)) x=x[-length(x)]
        xx=lapply(x, function(xx) {
            if(grepl(ocrex, txt[xx-1], , perl=TRUE) &  grepl(ocrex, txt[xx+1], perl=TRUE)) {        
                stop("Detected standard outer comments in the middle of HQ comments.\n",
                     paste(txt[xx+-1:1], collapse="\n" ))}    
        })
    }
    
    ## Get outer arg comments
    ocomp= grep(ocrex, txt, perl=TRUE)
    ocom=txt[ocomp]
    ## debug 
    ## ocom=letters[1:9]
    ## ocomp=c(1, 3:6, 10, 21:23)

    ## Identify contiguos outer comments
    ccom=c(TRUE, diff(ocomp)!=1)

    ## Stack all to first of contiguos comments 
    ocomp2=NULL; ocom2=NULL
    for(i in seq_along(ocomp)) {
        if(ccom[i]) {
            pos=ocomp[i]
            ocomp2=c(ocomp2, pos)
            val=sub(ocrex.loose, '', ocom[i])
            ocom2=c(ocom2, val)
        } else {
            val=paste(val, sub(ocrex.loose, '', ocom[i]))
            ocom2[length(ocom2)]=val        
        }    
    }
    ocomp=ocomp2; ocom=ocom2

    ##  Get args lines after comments
    findAgs=function(pos) { # find non comment line postion, given outer HQ comment position 
        while(startsWith(trimws(txt[pos]), "#")) pos=pos+1
        pos
    }      
    ocomp.ags=unlist(sapply(ocomp, findAgs))
    ocom.ags=txt[ocomp.ags]

    ## Test blanks after HQ comments
    ocom.ags=trimws(ocom.ags)
    if(any(sapply(ocom.ags, nchar) == 0))
        stop("Some HQ outer comments are followed by blank lines")
              
    ## Parse args line  
    end=regexpr("[=,)]|$", ocom.ags)
    ocom.ags=substr(ocom.ags, 1, end-1) 

    if(is.null(ocom)) ocom <- ocomp <- character(0)
    
    ## Inner HQ comments
    ## -----------------
    icrex.loose=sprintf("[^ ]+ *%s ", HQ)   #-> "[^ ]+ *HQ "

    ## Get inner arg comments
    icomp= grep(icrex.loose, txt)
    icom=txt[icomp]

    ## Extract args
    end=regexpr("[=,#]", icom)
    icom.ags=substr(icom, 1, end-1) 
    icom.ags=trimws(icom.ags)
    icom=sub(paste0(".+", HQ, " +"), '', icom)


    ## Match @args tag
    ## ---------------
    acrex=sprintf(" *%s +@args", HQ)   #-> " *HQ +@args"
    
    ## Get @args positions
    acomp= grep(acrex, txt)

    ## Merge argument slots and HQ comments by position
    df <- data.frame

    if(length(ocom)) ocom <- paste0(ocom.ags, "\n:   ", ocom)
    if(length(icom)) icom <- paste0(icom.ags, "\n:   ", icom)
    m <- merge(
        df(pos=ocomp, com=ocom),
        df(pos=icomp, com=icom),
        suffixes=c('.out','.in'),
        by="pos", all=TRUE)

    apos <- if(length(acomp)) TRUE else acomp
    m <- merge(
        df(pos=acomp, apos=apos),
        m,
        by="pos", all=TRUE)
    
    m[2]=!is.na(m[2])
    m[is.na(m)]=""
    m=df(m[1:2], com=apply(m[3:4], 1, paste, collapse=""))

    ## Split by comment by slots  and cat
    spl=rep(0, nrow(m))
    spl[m$apos] =  m$pos[m$apos]
    spl=cumsum(spl)
    spl=split(m$com, spl)
    spl=sapply(spl, function(x) paste(x[-1], collapse="\n\n"))


    ## Replace commment slots with comments 
    for(i in seq_along(acomp)){
        txt[acomp[i]] = spl[i]    
    }
    writeLines(txt,  tmpfile)
  
}

## Adapted from bloomr.build.R
script.path <- function() { # Identify the sourced script and its parent for G$me and G$prjdir 

    ## The standard method is 'sys.frame(1)$ofile', but we deal with
    ## non-top-level source calls, e.g. a source() nested in function.
    maybe.script <- lapply(FRAMES, \(frame) frame$ofile) |> unlist()
    if(length(maybe.script) > 1 || void(maybe.script)) stop("I am unable to identify the sourced script")
    maybe.dir <- dirname(maybe.script)

    ## The method is reliable, but we check that the project dir is a git
    ## dir and has the main build script
    proofs <- c("br-common.R") %in% dir(maybe.dir, all.files = TRUE)
    if(!all(proofs))
        stop("The file you sourced, identified as below, does not seem to come from the original BloomR project")
    maybe.script
}

## From bloomr.build.R
void <- function(x) # Similar to !nzchar(var) but works if var is NA or NULL
    is.null(x) || is.na(x) || nchar(as.character(x)) == 0


## may be useful for debug
return(SCRIPTPATH)
