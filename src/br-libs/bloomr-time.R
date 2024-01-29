## ----store, code = store.parse(THISLIB), opts.label='brfuncs'----------------------------------------------------------------------
THISLIB <- 'bloomr.time'
assign(THISLIB, new.env(parent = asNamespace("stats")))
store <-
function (sym, reg = FALSE)
{
    if (is.function(sym)) {
        name <- deparse(substitute(sym))
        val <- sym
    }
    else {
        name <- sym
        val <- get(sym)
    }
    assign(name, val, envir = get(THISLIB))
    if (reg) {
        mtcs <- regmatches(name, regexec("(.+)(\\.)(.+)", name))[[1]]
        genname <- mtcs[2]
        class <- mtcs[4]
        method <- mtcs[1]
        registerS3method(genname, class, method, get(THISLIB))
    }
    rm(list = name, envir = parent.frame())
}

## ----MISCFUNC, opts.label='brfuncs'------------------------------------------------------------------------------------------------

br.try.date=function(d){ # convert vector d to a date vector if possible or return null
### Any element should be POSIXlt, POSIXct, Date, "%Y/%m/%d", or "%Y-%m-%d"

    if(identical(class(d), c("POSIXlt", "POSIXt"))) return(d)
    if(identical(class(d), c("POSIXct", "POSIXt"))) return(d)
    if(identical(class(d), "Date")) return(d)

    if(is.character(d)) {

        try=as.Date(d, "%Y%m%d")
        if(!anyNA(try)) return(try)

        try=as.Date(d, "%Y/%m/%d")
        if(!anyNA(try)) return(try)

        try=as.Date(d, "%Y-%m-%d")
        if(!anyNA(try)) return(try)
    }

    return(NULL)
}

br.is.same.class=function(...){ # Check if all argumets have the same class
### Mostly intended to check if dates are homogeneous

    L=lapply(list(...), class)
    length(unique(L))==1
}


store(br.try.date)
store(br.is.same.class)



## ----time, opts.label='brfuncs'----------------------------------------------------------------------------------------------------
`%+%` <- function(x,y) UseMethod("%+%")
`%+%.Date` <- function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]
`%-%` <- function(x,y) UseMethod("%-%")
`%-%.Date` <- function(date,n) seq(date, by = paste (-n, "months"), length = 2)[2]

year=function(d, n=NULL){
    if(is.null(n)) d=as.numeric(format(d, "%Y")) else year(d)=n
    d
}
`year<-`=function (d, value) d <-as.Date(paste0(value, format(d, "-%m-%d")))

month=function(d, n=NULL){
    if(is.null(n)) d=as.numeric(format(d, "%m")) else month(d)=n
    d
}
`month<-`=function (d, value) d <-as.Date(paste0(format(d, "%Y-"),  value, format(d, "-%d")))

day=function(d, n=NULL){
    if(is.null(n)) d=as.numeric(format(d, "%d")) else day(d)=n
    d
}
`day<-`=function (d, value) d <-as.Date(paste0(format(d, "%Y-%m-"),  value))

last.day=function(d){
    x=d %+% 1 #add a month
    day(x)=1  #set to 1st
    day(x-1)  #get day before
}

day.us=function(d1, d2){
    #set to first of month
    x1=day(d1,1);x2=day(d2,1);
    x=seq(x1, x2, by="1 month")
    #last day of each month in seq
    x=sapply(x, last.day)
    #count 31d-months
    x=length(which(x>30))
    #substract 1 for each 31d-month
    as.numeric(d2-d1-x)
}

store(day)
store(month)
store(year)
store(`day<-`)
store(`month<-`)
store(`year<-`)
store(`%+%`)
store(`%-%`)
store(`%+%.Date`, reg = TRUE)
store(`%-%.Date`, reg = TRUE)
store(last.day)
store(day.us)


## ----attach, code = lib.attach(), opts.label='brfuncs'-----------------------------------------------------------------------------
{
    attach(get(THISLIB), name = THISLIB)
    rm(store)
    rm(list = c(THISLIB, "THISLIB"))
}

