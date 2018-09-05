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


## Set and test work directory
x=file.path(dirname(R.home()), "bloomr.txt")
if(!file.exists(x))stop(paste("I cannot find:\n", x, "\nConsider to reinstall the product."))

x=readLines(x)
x=paste0("This is BloomR version ", x)
xx=paste0(x, "\n")

x=paste0("Based on ", R.version.string)
xx=paste0(xx,  x, "\n")

x=normalizePath(R.home("../../mybloomr"), winslash="/", mustWork=FALSE)
if(!dir.exists(x)) stop(paste("I cannot find 'mybloomr' directory with the path:\n", x,
                              "\nIf you cannot fix it, consider to reinstall the product."))
setwd(x)
x=paste0("Current working directory is\n", getwd())
xx=paste0(xx, x, "\n")

x=max(nchar(strsplit(xx, "\n")[[1]]))
x=paste0(rep("=", x), collapse="")
xx=paste0(x, "\n", xx)
xx=paste0(xx, x)

message(xx)
rm(x,xx)

