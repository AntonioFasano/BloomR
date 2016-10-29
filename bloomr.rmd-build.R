
###  Convert rmd and markdown files into R, HTML and PDF files

##  Requirements:
##  Pandoc.exe (https://github.com/jgm/pandoc/releases) should be in your path.
##  R knitr package (http://yihui.name/knitr)
##  R markdown package (http://cran.r-project.org/web/packages/markdown)
##  To generate PDF files you need to have LaTeX install. MiKTeX is recommend
## 
##  Usage:
##  Just source this file
##  You will get the *(r)md files converted

## Customise 
HELPDIR="src"

## Check for pandoc and LaTeX
if(!nzchar(Sys.which('pandoc'))) stop("Pandoc not found in search path.",
                                      "To install it visit:\n",
                                      "https://github.com/jgm/pandoc/releases")
if(!nzchar(Sys.which('latex'))) warning("LaTeX not found in search path")

## Check load knitr and markdown
x="Please, install it before proceeding"
if(!require(knitr)) stop("knitr package missing.", x)
if(!require(markdown)) stop("knitr markdown missing.", x)

## Check for directory src and cd to it
if(!dir.exists(HELPDIR)) stop("Can't find directory:\n", 
                              normalizePath(HELPDIR, winslash="/", mustWork=FALSE))
OLDDIR=getwd()
setwd(HELPDIR)
message("PWD now:")
getwd()



## Set margins 
opts_chunk$set(tidy.opts=list(width.cutoff=60))
knit2=function(file){
    require(knitr); require(rmarkdown)
    fse=tools:::file_path_sans_ext(file)
    step2=FALSE
    knit(file)
    step2=TRUE
    knit(file)
    topics()
    render(paste0(fse, '.md'), "html_document")
    render(paste0(fse, '.md'), pdf_document())
    render("../README.md", html_document())
}

message("bloomr.rmd -> bloomr.R/md/html/pdf")
message("README.md -> README.pdf")
knit2("bloomr.Rmd")

#cat("bloomr.rmd -> bloomr.md\n") # Note r chunks are all set to "include=FALSE"
#knit("bloomr.rmd", quiet=TRUE)
#cat("bloomr.rmd -> bloomr.r\n") 
#purl("bloomr.rmd")
#cat("bloomr.md -> bloomr.html\n") 
#markdownToHTML("bloomr.md", "bloomr.html")
#cat("README.md -> README.html\n") 
#markdownToHTML("README.md", "README.html")
#cat("bloomr.md -> bloomr.pdf\n") 
#shell("pandoc bloomr.md -o bloomr.pdf", shell=Sys.getenv("COMSPEC"))

## Show function summary
read.head=function() {x=readLines('bloomr.rmd'); x[grep("^=+", x)-1]}
cat("\nBloomR facility functions summary:", read.head(), sep='\n')

## Cd to old dir 
setwd(OLDDIR)
message("PWD back to:")
getwd()
