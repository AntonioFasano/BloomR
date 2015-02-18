## Download 'bloomr.build.R' from Gthub and build it inside 'scriptdir/data'
## Intended for fast internal testing - Readme method is the standard way
## Use it by drag and drop in R GUI

source_github <- function(u) {
    ## load package
    if(!require(RCurl)) {
        install.packages( "RCurl"  , repos= "http://cran.r-project.org/") 
        require(RCurl)
    }
    

    ## read script lines from website and evaluate
    script <- getURL(u, ssl.verifypeer = FALSE)

    script =gsub("\\r", "", script) # unix line endings
    eval(parse(text = script),envir=.GlobalEnv)
}  


### Set outdir
##cdir=getwd()
script.path= parent.frame(2)$ofile
bdir= paste0(dirname(script.path), format(Sys.time(), "/%y%m%d-%H%M"))


### Start download and build
ans <- readline(sprintf("I will create BloomeR in %s . (Y/n)? ", bdir))
if(tolower(ans)=="y" || ans==""){
    source_github(
      "https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R"
        )
    makeBloomR(bdir)
} else print('bye') 

 
