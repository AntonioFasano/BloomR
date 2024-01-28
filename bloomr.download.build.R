
### Download 'bloomr.build.R' from Github and ask to build it inside a date-based dir in the cwd
## Intended for fast internal testing - Readme method is the standard way

### Link to this file:
## https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.download.build.R


## script.path= parent.frame(2)$ofile

loadLib <- function(lib){
    if (!lib %in% rownames(installed.packages())){
        repo <- getOption("repos")[["CRAN"]]
        if(repo=="@CRAN@") repo <- "http://cran.r-project.org"
        install.packages(lib, repos=repo)
    }
    if(!eval(parse(text=paste('require(', lib,')')))){
        message("\nUnable to load", lib, "library")
        return (FALSE)
    }

    if(!has_internet()){
        message("\nThere is no internet connection.")
        return (FALSE)
    } 
    
    return (TRUE)
}

source_github <- function(u) {

    req <- curl_fetch_memory(u)
    if(req$status_code >= 400){
        message("The download of the build script from GitHub failed with status code ", req$status_code)
        return (FALSE)
    } else {
        script <- rawToChar(req$content)
        script <- gsub("\\r", "", script) # remove unix line endings
        ## message( script )    
        eval(parse(text = script), envir=.GlobalEnv)
        return(TRUE)
    }
}


main <- function(){

    buildscript <- "https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R"
    
    ## Load curl package
    if(!loadLib("curl")) return(1)

    ## Load build script
    if(!(source_github(buildscript))) return(1)

    ## Create download path
    dirname <- format(Sys.time(), "bloomr-%y%m%d-%H%M")
    dirpath <- file.path(getwd(), dirname)
    
    ## Start download and build
    ans <- readline(sprintf("I will create BloomeR in %s . (Y/n)? ", dirpath))
    if(tolower(ans)=="y" || ans=="")  makeBloomR(dirpath) else print('bye')

}
 
main()
