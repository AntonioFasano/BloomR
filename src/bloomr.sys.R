
### BloomR admin functions

quit <- function(save = "no", status = 0, runLast = TRUE) base:::quit(save, status, runLast)
q    <- function(save = "no", status = 0, runLast = TRUE) base:::quit(save, status, runLast)

.br.home <- function(dir="") {
### Return "apps-path/dir",
### where "apps-path" is the directory hosting BloomR apps, e.g. R, BRemacs.
### because the apps path host R, the function is based on `R.home()`

    file.path(dirname(R.home()), dir)

}

br.info <- function(msg = TRUE) {
    infofile <- file.path(dirname(R.home()), "bloomr.txt")
    if(!file.exists(infofile)) stop(paste("I cannot find:\n", x, "\nConsider to reinstall the product."))
    info <- readLines(infofile)
    if(msg) writeLines(info)
    invisible(info)
}

.br.edition <- function() {
    info <- br.info(msg = FALSE) 
    edition <- info[length(info)]
    tolower(trimws(sub("edition", "", edition)))
}


.br.wpath=function(s) # Convert path Unix->Win and quote
    shQuote(gsub('/', '\\\\',  s))



