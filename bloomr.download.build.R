## Download 'bloomr.build.R' from Gthub and build it inside './bloomr.bin'
## Intended for fast internal testing - Readme method is the standard way

## Get  CRAN Windows binaries release for a package 
cran.geturl=function(pack){

    ## CRAN links
    cranpage="http://cran.r-project.org/web/packages/"
    cranbin="http://cran.r-project.org/bin/"

    ## Get package page 
    url=paste0(cranpage, pack, "/index.html")
    page=download.html(url)    

    ## Get bin url: the first occurence on page is the dev version, second is release
    url=regmatches(page, gregexpr("windows/contrib.*?\\.zip", page))[[1]][2]
    paste0(cranbin, url)   
}

source_github <- function(u) {
  # load package
  if(!require(RCurl)) install.packages( "RCurl"  , repos= "http://cran.r-project.org/") 


  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)

  script =gsub("\\r", "", script) # unix line endings
  eval(parse(text = script),envir=.GlobalEnv)
}  

cdir=getwd()
bdir= paste0(cdir, format(Sys.time(), "/%y%m%d-%H%M"))



ans <- readline(sprintf("I will create BloomeR in %s . (Y/n)? ", bdir))
if(tolower(ans)=="y" || ans==""){
    source_github(
      "https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R"
        )
    makeBloomR(bdir)
} else print('bye') 

 
