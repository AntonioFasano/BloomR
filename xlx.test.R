
## Unit test for xlx.r

## ToDO: tests for blanks and for formats 

## Customise test spread
SPREAD='res/xlx.test.xlsx'
NNS=2  # number of non-empty sheets

source('xlx.R')

callme=function(...){

    cat("\nEvaluating:", '\n')
    cmd=sub("^.+?\\(", "", paste(deparse(sys.call()), collapse="" ))
    cmd=paste0("read.xlx(\"", SPREAD, "\", ", cmd)
    cat(cmd, '\n')
    out=suppressMessages(eval(parse(text=cmd)))
    #cat("-------------\nEvaluated:\n", cmd, '\n')
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

## Test with single condition 
tests=function(num, test){
    if(length(test)>1) stop("Test with multiple logicals! Try vector version")
    if(!test) err(num)
}

## Test with multiple conditions 
testm=function(num, test){
    if(!all(test)) err(num)
}


## all nums in strings vec
allnum=function(x) all(!is.na(strtoi(x)))
## all letters in strings vec
alllet=function(x) all(is.na(strtoi(x)))

#Euristically assume a header when objects' names are all letters
is.head.eu=function(out){ 
    if(class(out)=='data.frame') return (alllet(names(out)))
    if(class(out)=='list') return(
                all(unlist(lapply(out, function(item) alllet(names(item))))))
    return(FALSE) # because not DF or a list
}

## Names of items with headers equal first rows of items withut 
head.eq.1st=function(out.h, out.nh){

    if(class(out.h)!=class(out.nh)) {
        message("\n Objects for comparison are of different types")
        return(FALSE)
    }

    ## Compare DF outputs
    if(class(out.h)=='data.frame') return(
                all(names(out.h) == t(out.nh)[,1]))
                
    ## Compare list outputs
    if(class(out.h)=='list') return(
                all(unlist(lapply(seq_along(out.h), function(i) 
                                  all(names(out.h[[i]]) == t(out.nh[[i]])[,1])))))
    
    return(FALSE) # because not DF or a list    
}


main=function(){

## Flat call    
out=callme()
msg("list of", NNS, "sheets as DF, w/out headers.")
tests(1, (class(out)== 'list'))
tests(2, length(out)== NNS)
tests(3, class(out[[1]])== "data.frame")
tests(4, class(out[[2]])== "data.frame")
tests(5, !is.head.eu(out[[1]]))
msgn("Success!")

## Single sheet 
out=callme(sheets=tolower("survey2"), header.sheets=TRUE)
msg("single DF, with headers (called lower case).")
tests(1, class(out)== 'data.frame') 
tests(2, is.head.eu(out))
msgn("Success!")

## Single, case, no-heade
nohead=callme(sheets=toupper("survey2"), header.sheets=FALSE)
msg("single DF, without headers (called upper case).\n")
tests(1, !is.head.eu(nohead))
msg("First row here as names in previous")
tests(2, head.eq.1st(out, nohead)) # first head, sec not
msgn("Success!")

## Swap order and case
out=callme(sheets=c(toupper("survey2"), tolower("survey1")))
msg("order and case of output as in input.")
testm(1, names(out) == c(toupper("survey2"), tolower("survey1")))
msgn("Success!")

## Test headers 
#  ------------


## Test sheets headers 
head=  callme(sheets=c("survey1", "survey2"), header.sheets=c(TRUE, TRUE))
nohead=callme(sheets=c("survey1", "survey2"), header.sheets=c(FALSE, FALSE) )
msg("sheets with/without: headers names == first rows.")
tests(1, head.eq.1st(head, nohead))
msgn("Success!")


## Test range headers 
head=  callme(ranges=c("education", "students"), header.ranges=c(TRUE, TRUE))
nohead=callme(ranges=c("education", "students"), header.ranges=c(FALSE, FALSE) )
msg("ranges with/without headers: names == first rows.")
tests(1, head.eq.1st(head, nohead))
msgn("Success!")

## Test mix range, sheet headers 
head=   callme(
    ranges="education", header.ranges=TRUE, sheets="survey2", header.sheets=TRUE)
nohead=callme(
    ranges="education", header.ranges=FALSE, sheets="survey2", header.sheets=FALSE)
msg("sheets & ranges with/without headers: names == first rows.")
tests(1, head.eq.1st(head, nohead))
msgn("Success!")

}

##          main()
