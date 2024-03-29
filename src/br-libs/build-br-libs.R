### Executes the build chunk of each Rmd found in this directory and knits main README.md
## USAGE: Source this file and run knit.all()

## For interactive use (C-l)  
if(FALSE){
    run.build.chunk("bloomr-bbg.Rmd")
    run.build.chunk("bloomr-rmd.Rmd")
    run.build.chunk("bloomr-time.Rmd")
    knit.all()
}


## Find README
README <- "../../README.md" # based on GITPOS in run.build.chunk()

run.build.chunk <- function(rmd.file) {

    ## Polymode tends to start R synced with git root dir.
    ## We check the Rmd path is absolute or as expected    
    GITPOS <- "/src/br-libs"
    abspt <- function(pt) unlist(strsplit(pt, split="/"))[[1]] == "" || regexpr("^.:(/|\\\\)", pt) != -1L
    is.rel <- !abspt(rmd.file)
    bad.dir <- sub(paste0("^", dirname(dirname(getwd()))), "", getwd()) !=  GITPOS

    ## Try to fix a bad path, when not abolute
    if(bad.dir && is.rel) 
        if(dir.exists(file.path(normalizePath("."), "src/br-libs"))) {
            setwd("src/br-libs") # Polymode
        } else {
            stop("Rmd file expected to be in '", GITPOS, "'. The given absolute path is\n",
                 normalizePath(rmd.file), "\nUse an absolute path or check 'GITPOS' var in knit.twice()")
        }

    
    lines <- readLines(rmd.file)
    heads.pos <- grep(knitr::all_patterns$md$chunk.begin, lines)
    build.start <- heads.pos[ grep("``` *\\{r +build", lines[heads.pos]) ]
    if(length(build.start) != 1)
        stop("Unable to find build chunck (or too many)")
    foots.pos <- grep(knitr::all_patterns$md$chunk.end, lines)
    build.end <- foots.pos[foots.pos > build.start][1]
    build <- lines[ (build.start+1) : (build.end-1) ]
    eval(parse(text = build), envir = e <- new.env()) 

    ## Inform on new wdir
    if(bad.dir && is.rel) message("\nNOTE: Workdir was adjusted to ", GITPOS, ".")
}

knit.all <- function() {
    run.build.chunk("bloomr-bbg.Rmd")
    run.build.chunk("bloomr-rmd.Rmd")
    run.build.chunk("bloomr-time.Rmd")
    ## Run last to benefit the auto-workdir by run.build.chunk()
    rmarkdown::render(README, rmarkdown::html_document())
}


