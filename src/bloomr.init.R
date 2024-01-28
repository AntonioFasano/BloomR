### BloomR Bootstrap 
### ================

### Bootstrap Debug
### ---------------
## *Possible in BRemacs* launching apps/ed/bremacs-dbg.cmd
## For Core you can manually run in cmd.exe:
## Set "BREMACSDBG=1" & apps/ed/core.cmd
.bootstrap.debug <- Sys.getenv("BREMACSDBG") == "1"
if(.bootstrap.debug) message("BloomR: Debug/verbose mode on")

### Get system functions
### --------------------
# bloomr.sys <-  new.env(parent=asNamespace("stats"))
# source(R.home("share/bloomr/bloomr.sys.R"), local=bloomr.sys)
# ## q/quit() are redefined so this warns on debug
# attach(bloomr.sys, warn.conflicts = .bootstrap.debug)
# rm(bloomr.sys)

## Manual attach libs
load.lib <- function(name, atc.name = name){
    assign(name, new.env(parent = asNamespace("stats")))
    rfile <- paste0(name, ".R")
    source(file.path(R.home("share/bloomr"), rfile), local = get(name))
    attach(get(name), name = atc.name, warn.conflicts = .bootstrap.debug)
    rm(name)
}


### Attach BloomR libs
### ------------------
load.lib("bloomr.sys")
source(R.home("share/bloomr/bloomr-bbg.R"))
source(R.home("share/bloomr/bloomr-rmd.R"))
source(R.home("share/bloomr/bloomr-time.R"))
load.lib("xlx")
rm(load.lib)

### Eikon
### -----
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


### Load other libs
### ---------------
local(x <- utils:::capture.output(library(Rblpapi), type = c("message")))
library(stats)
library(zoo, warn.conflicts=FALSE)
library(xts)

### Several tests To do
##.br.testBR()

### Set default repository (to install packages)
### -------------------------------------------
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org"
       options(repos=r)
   })    

### Patch install.package() dialogs not appearing in BR/Emacs
### ---------------------------------------------------------
install.packs.debug <- vector('character')
con <- textConnection('install.packs.debug', 'wr', local = TRUE)
sink(con, type = "message")
if(Sys.getenv("bloomr_branch") == "bremacs"){
    untrace(utils::install.packages)
    trace(utils::install.packages, 
          tracer=quote({wasloaded='package:tcltk' %in% search(); require(tcltk)}) ,
    exit=quote(if(!wasloaded) detach('package:tcltk', unload=TRUE)),
    print=FALSE
    )
}
sink(NULL, type = "message")
close(con)
rm(con)
if(.bootstrap.debug) message(install.packs.debug) else rm(install.packs.debug) 


### Set  work directory and show user info
### --------------------------------------
local({    
    ## BloomR version/edtion 
    info <- br.info(msg = FALSE)
    version <- info[1]
    build <- info[2]
    edition <- paste(tools::toTitleCase(.br.edition()), "Edition")
    info.ex <- paste("This is BloomR version", version, edition)
    info.ex <- c(info.ex, build)

    ## R version 
    rver <- paste("Based on:", R.version.string, "--", dQuote(R.version$nickname))
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

### Remove clutter
### --------------
rm(.bootstrap.debug)
