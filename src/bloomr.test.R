
## Unit test for xlx.r

## ToDO: tests for blanks and for formats 


callme=function(...){
    cat("\nEvaluating:", '\n')
    cmd=sys.call()[[2]]   
    cat(deparse(cmd), '\n')
    out=suppressMessages(eval(cmd, envir=parent.frame()))
    out
}

msg=function(...){
    cat('Expected', ..., '')
}

msgn=function(string){
    cat(string, '\n')
}

err=function(n){
    cat('\n')
    stop("Failed on step ", n, " of this test!")
}

tests=function(num, test){
    if(length(test)>1) stop("Test with multiple logicals! Must be single T/F")
    if(!test) err(num)
}

testm=function(num, test){
    if(!all(test)) err(num)
}

main=function(havebbg=TRUE){

if(havebbg){
    message("Assuming you are logged to  Bloomberg")
    fpath=function(x) paste0("mybloomr/",  x)
    con=br.open()    
}else{
    message("Assuming you are not logged to Bloomberg")
    source('bloomr.R')
    fpath=function(x) paste0("res/",  x)
    con=NULL
}


## Test with tickers 
ticks=read.csv(fpath("tickers.csv"), as.is=TRUE)

out=callme(br.bulk.csv(con, fpath("tickers.csv")))
msg("list of ", length(ticks), "with lengths",
    apply(ticks, 2, function(col)  length(col[nzchar(col)])))
tests(1, length(out)==length(ticks))
testm(2,  mapply(function(x,y) ncol(x)==length(y[nzchar(y)]), out, ticks))
msgn("Success!")

br.close(con)

}
