### BloomR bootstrap 
 
## Get user functions
source(R.home("share/bloomr/bloomr.R"))

## Get system functions
#assign('bloomr.sys',  new.env(parent=asNamespace("stats")))
bloomr.sys <-  new.env(parent=asNamespace("stats"))
source(R.home("share/bloomr/bloomr.sys.R"), local=bloomr.sys)
attach(bloomr.sys, warn.conflicts = FALSE)
rm(bloomr.sys)

## Get time functions
source(R.home("share/bloomr/bloomr.time.R"))

## xlx 
local({
    bloomr.xlx <- new.env(parent=asNamespace("stats"))
    source(R.home("share/bloomr/xlx.R"), local=bloomr.xlx)
    attach(bloomr.xlx)
})

## Eikon
library(eikonapir)
set_app_id("6ffad79674dd44a2b343bd5dd7d3359aa4c7c6fe")
## This should avoid the requestInfo in the global env. Test before distribute
bloomr.eikonreq <- new.env()
bloomr.eikonreq$requestInfo <- requestInfo
rm(requestInfo)
attach(bloomr.eikonreq)
rm(bloomr.eikonreq)
## Delete if the hack above works 
assign("eikon.fallback", function() {
    library(eikonapir)
    set_app_id("6ffad79674dd44a2b343bd5dd7d3359aa4c7c6fe")
}, "bloomr.sys")


## Load other libs
local(x <- utils:::capture.output(library(Rblpapi), type = c("message")))
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
    ## BloomR version/edtion 
    info <- br.info(msg = FALSE)
    version <- info[1]
    build <- info[2]
    edition <- paste(tools::toTitleCase(.br.edition()), "Edition")
    info.ex <- paste("This is BloomR version", version, edition)
    info.ex <- c(info.ex, build)

    ## R version 
    rver <- paste0("Based on ", R.version.string)
    info.ex <- c(info.ex, rver)

    ## Homedir
    homedir <- normalizePath(R.home("../../mybloomr"), winslash="/", mustWork=FALSE)
    if(!dir.exists(homedir)) stop(paste("I cannot find 'mybloomr' directory with the path:\n", homedir,
                                        "\nIf you cannot fix it, consider to reinstall the product."))
    setwd(homedir)
    info.ex <- c(info.ex, "Home directory is:", homedir)

    ## Box
    linelen <- max(nchar(info.ex))
    linedraw <- strrep("=", linelen)
    info.ex <- c(linedraw, info.ex, linedraw)
    writeLines(info.ex)
})
