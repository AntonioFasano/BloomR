
## Unit test for xlx.r

## ToDO: tests for blanks and for formats 

## Customise test spread
SPREAD='res/xlx.test.xlsx'
NNS=2  # number of non-empty sheets

source('bloomr.R')

#ret.args=function(...){
#     sys.call()  
#}


callme=function(...){
    cat("\nEvaluating:", '\n')
    cmd=sys.call()[[2]]   
    cat(deparse(cmd), '\n')
    out=suppressMessages(eval(cmd))
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

test=function(num, test){
    if(length(test)>1) stop("Test with multiple logicals! Must be single T/F")
    if(!test) err(num)
}

testm=function(num, test){
    if(!all(test)) err(num)
}



main=function(havebbg=TRUE){

if(havebbg) con=br.open() else con=NULL

## Test with tickers 
ticks=read.csv("res/tickers.csv", as.is=TRUE)

out=callme(br.bulk.csv(con, "res/tickers.csv"))
msg("list of ", length(ticks), "with lengths",
    apply(ticks, 2, function(col)  length(col[nzchar(col)])))
tests(1, length(out)==length(ticks))
testm(2,  mapply(function(x,y) ncol(x)==length(y[nzchar(y)]), out, ticks))
msgn("Success!")

br.close(con)

}

trash=function(){

## Flat call    
out=callme()
msg("list of", NNS, "sheets as DF, w/out headers.")
con(1, (class(out)== 'list'))
con(2, length(out)== NNS)
con(3, class(out[[1]])== "data.frame")
con(4, class(out[[2]])== "data.frame")
con(5, !is.head.eu(out[[1]]))
msgn("Success!")

## Single sheet 
out=callme(sheets=tolower("survey2"), header.sheets=TRUE)
msg("single DF, with headers (called lower case).")
con(1, class(out)== 'data.frame') 
con(2, is.head.eu(out))
msgn("Success!")

## Single, case, no-heade
nohead=callme(sheets=toupper("survey2"), header.sheets=FALSE)
msg("single DF, without headers (called upper case).\n")
con(1, !is.head.eu(nohead))
msg("First row here as names in previous")
con(2, head.eq.1st(out, nohead)) # first head, sec not
msgn("Success!")

## Swap order and case
out=callme(sheets=c(toupper("survey2"), tolower("survey1")))
msg("order and case of output as in input.")
con(1, all(names(out) == c(toupper("survey2"), tolower("survey1"))))
msgn("Success!")

## Test headers 
#  ------------


## Test sheets headers 
head=  callme(sheets=c("survey1", "survey2"), header.sheets=c(TRUE, TRUE))
nohead=callme(sheets=c("survey1", "survey2"), header.sheets=c(FALSE, FALSE) )
msg("sheets with/without: headers names == first rows.")
con(1, head.eq.1st(head, nohead))
msgn("Success!")


## Test range headers 
head=  callme(ranges=c("education", "students"), header.ranges=c(TRUE, TRUE))
nohead=callme(ranges=c("education", "students"), header.ranges=c(FALSE, FALSE) )
msg("ranges with/without headers: names == first rows.")
con(1, head.eq.1st(head, nohead))
msgn("Success!")

## Test mix range, sheet headers 
head=   callme(
    ranges="education", header.ranges=TRUE, sheets="survey2", header.sheets=TRUE)
nohead=callme(
    ranges="education", header.ranges=FALSE, sheets="survey2", header.sheets=FALSE)
msg("sheets & ranges with/without headers: names == first rows.")
con(1, head.eq.1st(head, nohead))
msgn("Success!")

}

##          main()
