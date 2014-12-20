## Download 'bloomr.build.R' from Gthub and build it inside './bloomr.bin'
## Intended for fast internal testing - Readme method is the standard way

source_github <- function(u) {
  # load package
  require(RCurl)

  # read script lines from website and evaluate
  script <- getURL(u, ssl.verifypeer = FALSE)

  script =gsub("\\r", "", script) # unix line endings
  eval(parse(text = script),envir=.GlobalEnv)
}  


ans <- readline("I will create BloomeR in <current-dir>/bloomr.bin. (Y/n)? ")
if(tolower(ans)=="y" || ans==""){
    source_github(
      "https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R"
        )
    makeBloomR("bloomr.bin")
} else print('bye') 

 