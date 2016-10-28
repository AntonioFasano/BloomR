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

## Several tests
.br.testBR()

## Set default repository
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org"
       options(repos=r)
   })    

cat("Current working directory is\n", getwd(), "\n")


