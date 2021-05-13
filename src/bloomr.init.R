### BloomR bootstrap 
 
## Get API functions
assign('bloomr.api',  new.env(parent=asNamespace("base")), envir=asNamespace("base"))
source(paste0(R.home("share"), "/bloomr/bloomr.api.R"), local=bloomr.api)
attach(bloomr.api)

## Get user functions
source(paste0(R.home("share"), "/bloomr/bloomr.R"))

## Get addon functions 
assign('bloomr.addons',  new.env(parent=asNamespace("base")), envir=asNamespace("base"))
source(paste0(R.home("share"), "/bloomr/bloomr.sys.R"), local=bloomr.addons)
source(paste0(R.home("share"), "/bloomr/xlx.R"), local=bloomr.addons)
attach(bloomr.addons)

## Load libs
local(x <- utils:::capture.output(library(Rblpapi), type = c("message")))
library(eikonapir)
set_app_id("6ffad79674dd44a2b343bd5dd7d3359aa4c7c6fe")
library(stats)
library(zoo, warn.conflicts=FALSE)
library(xts)

## Several tests
#.br.testBR()

## Set default repository (to install packaes)
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org"
       options(repos=r)
   })    


## Set  work directory and show user info
local({    
    infofile <- file.path(dirname(R.home()), "bloomr.txt")
    if(!file.exists(infofile)) stop(paste("I cannot find:\n", x, "\nConsider to reinstall the product."))

    info <- readLines(infofile)
    edition <- info[3]
    info <- paste(info[-3], collapse = "\n")

    ## Clean vanilla console
    if(nzchar(Sys.getenv("vanilla"))) 
        system("powershell -ExecutionPolicy Bypass -command (New-Object -ComObject Wscript.Shell).SendKeys([string][char]12)")
    
    info.ex <- paste0("This is BloomR version ", info)
    rver <- paste0("Based on ", R.version.string)
    info.ex <- paste0(info.ex, "\n",  rver)
    homedir <- normalizePath(R.home("../../mybloomr"), winslash="/", mustWork=FALSE)
    if(!dir.exists(homedir)) stop(paste("I cannot find 'mybloomr' directory with the path:\n", homedir,
                              "\nIf you cannot fix it, consider to reinstall the product."))
    setwd(homedir)
    cdir <- paste0("Current working directory is\n", getwd())
    info.ex <- paste0(info.ex, "\n", cdir)
    
    linelen <- max(nchar(strsplit(info.ex, "\n")[[1]]))
    linedraw <- paste0(rep("=", linelen), collapse="")
    info.ex <- paste0(linedraw, "\n", info.ex, "\n", linedraw)
    message(info.ex)
})
