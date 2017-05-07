## ----store, opts.label='purlme'------------------------------------------
## Purl this first
## Store br.* objects in bloomr env in base namespace
assign('bloomr',  new.env(parent=asNamespace("base")), envir=asNamespace("base"))

## func: store(func);  var: store("var")
store=function(sym){
    if(is.function(sym)){
        name=deparse(substitute(sym)) 
        val=sym
    } else {
        name=sym
        val=get(sym)
    }
    
    assign(name, val, envir=bloomr)
    rm( list=name, envir=parent.frame())
}


## ----br.hist.csv, opts.label='purlme'------------------------------------
br.hist.csv=function(
                     #' the connection token returned from br.open().
                     #' If `NULL` simulated values are generated.   
                     con,
                     file, #' path to CSV file.

                     #' case insensitive string denoting the Bloomberg field queried. Defaults to "PX_LAST".
                     #' If the field is wrong or not accessible, data will be empty,
                     #' but no error will be raised.
                     field="PX_LAST",

                     #' start date. Can be a Date object or an ISO string without separators (YYYYMMDD).
                     #' Defaults to 5 days before current date.  
                     start=Sys.Date()-5,
                    
                     end.date=Sys.Date(), #' end date. Same format as `start`. Defaults to current date.  

                     #' Logical or integer vector for selecting CSV columns (ticker groups).
                     #' Defaults to all columns.  
                     cols=NULL,

                     #' to be set to FALSE for (non-English) CSV, using semicolon as separator.  
                     comma=TRUE,

                     #'  If a string, it denotes the security type and is added to all tickers;
                     #' if TRUE "Equity", will be added; if FALSE (the default), nothing will be added.  
                     addtype=FALSE,

                     #' if TRUE, security types will be removed from names of list or xts output.
                     #' It defaults to FALSE.  
                     showtype=FALSE,

                     #' if TRUE (the default) time series are formatted as xts objects.
                     #' else as a data frame.  
                     use.xts=TRUE,
                     
                     #' if TRUE (the default) xts objects in the same group are merged using all rows and
                     #' using NAs for missing observations.
                     merge.xts=TRUE,

                     ## br.bdh args
                     #' list of Bloomberg options names. Require `option.values` too.
                     option.names = NULL,
                     #' list of Bloomberg options values related to `option.names`.
                     option.values = NULL,

                     #' if TRUE (the default) only trading days are used,
                     #' else non-trading days are added as NA values. 
                     only.trading.days = TRUE,

                     ## Simulation args                      
                     price=TRUE,
                     mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                     same.dates=FALSE, empty.sec=0,
                     weekend=TRUE, holidays=NULL)
{

    ## Check csv file
    if(!file.exists(file)) stop(paste('Unable to find the csv file', file))
    if(comma) csv=read.csv(file=file, as.is=TRUE) else{
        csv=read.csv2(file=file, as.is=TRUE)}

    ## Check cols arg
    if(!is.null(cols)){
        if(is.logical(cols) && !length(cols)==ncol(csv))
            stop(paste('Length of logical vector', paste(cols, collapse=' '),
                       'not equal to number of groups in', file))

        if(is.integer(cols) && max(cols)>ncol(csv))
            stop(paste('Unable to subset groups in', file, 'with columns', paste(cols, collapse=' ')))

        if(!is.logical(cols) && !all(cols%%1==0)) stop(paste(
         "'Col argument should be an integer or a logical vector of the same length of the groups in", file))
        csv=csv[cols]        
    }

    ## Get group names and count
    gnams=names(csv)
    gcnt=ncol(csv)

    ## Loop groups in csv
    grps=list()
    for(g in 1:gcnt){
        message('Processing ', gnams[g],  ' ...')
        x=list(br.hist(
            con=con, tiks=csv[[g]], field=field, start=start, end.date=end.date,
            
            addtype=addtype, showtype=showtype,
            use.xts=use.xts, merge.xts=merge.xts,
            
            option.names=option.names, option.values=option.values,
            only.trading.days=only.trading.days,
            
            price=price, same.dates=same.dates, empty.sec=empty.sec,
            weekend=weekend, holidays=holidays))
        
        names(x)=gnams[g]
        grps=c(grps, x)        
    }
    if(length(grps)==1) grps=grps[[1]]
    grps
}

store(br.hist.csv)


## ----br.hist, opts.label='purlme'----------------------------------------
br.hist=function(con,
                 
                 #' character vector of the tickers queried for data
                 tiks, field="PX_LAST", start=Sys.Date()-7, end.date=Sys.Date(),
                                            
                 addtype=FALSE, showtype=FALSE,

                 #' if TRUE (the default) time series are formatted as xts objects else as a data frame.  
                 use.xts=TRUE,

                 #' if TRUE (the default) xts objects are merged using all rows
                 #' and using NAs for missing observations.
                 merge.xts=TRUE,   
                 
                 ## br.bdh args
                 option.names = NULL, option.values = NULL,
                 only.trading.days = TRUE,

                 ## Simulation args                      
                 price=TRUE,
                 mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                 same.dates=FALSE, empty.sec=0,
                 weekend=TRUE, holidays=NULL )
{
    

    ## Check connection
    if(!is.null(con) && !.br.is.con(con)) stop('Invalid connection parameter') 
    
    ## Check tickers (skip possible empty CSV cells)
    if(!is.character(tiks)) stop('Tickers should be in the form of a character vector')
    tiks=tiks[!is.na(tiks)] 
    tiks=tiks[tiks!='']
    
    ## Check security type to add/show
    .br.check.type(addtype)
    .br.check.type(showtype)
    if(addtype==TRUE)  addtype="Equity"
    if(addtype!=FALSE) tiks=paste(tiks, addtype)
    if(!showtype) tiks.show=.br.cuttype(tiks) else tiks.show=tiks
    
    ## Check xts library availability
    if(use.xts && !require("xts", quietly=TRUE, character.only=TRUE)) stop("Can't find library xts")
 
    ## Check empty.sec is a ratio
    if(empty.sec<0 || empty.sec>1) stop("'empty.sec' must be between 0 and 1")
    
    ## br.bdh() and sample() pars
    bdhpars=list(
        con=con, field=field, start.date=start, end.date=end.date, 
        option.names=option.names, option.values=option.values,
        only.trading.days=only.trading.days)
    
    sampars=list(nrow=NULL, price=price, start=start, end.date=end.date, 
                 field=field,
                 use.xts=FALSE,
                 mean=mean, sd=sd, jitter=jitter,
                 rand.dates=!same.dates,                 
                 weekend=weekend, holidays=holidays)

    
    #emptydf=structure(list(date = logical(0), PX_LAST = logical(0)),
    #                  .Names = c("date", toupper(field)), row.names = integer(0), class = "data.frame")

    
    ## Get data as list of matrices
    LL=lapply(tiks, function(tik){
        message('Loading ', tik)
        if(is.null(con)) do.call("br.sample", sampars)
          else do.call("br.bdh", c(security=tik, bdhpars))        
    })

    ## Randomly identify empty.sec
    if(is.null(con)){        
        x=round(length(tiks.show) * empty.sec)
        empty=sample(length(tiks.show), x)
        LL[empty]=rep(list(NULL), x)
    }
        
    ## Convert matrices to XTS's
    if(use.xts){
        LL = lapply(LL, function(ts) {
            if(!is.null(ts)) setNames(xts(as.numeric(ts[,-1]), as.Date(ts[,1])), toupper(field)) else NA
        })
    }
    
    ## Merge XTS's
    if(use.xts && merge.xts){
        ## If there are only NAs, output NULL
        LL=if(all(is.na(LL))) unlist(LL)
           else 
               if(length(LL)>1) do.call("merge.xts", LL) else LL[[1]]
    }

    ## Return with ticker's names
    setNames(LL, tiks.show)
        
    ## Zero-rows xts
    ## In the future we can consider to use zero rows xts instead of NAs
    ## To convert LL=list(NA, NA, ...)-> empty xts: xts(t(unlist(LL)), Sys.Date())[-1]
    ## To set names for zero-rows xts:  dimnames(zero_xts)= list(NULL,names)
    ## Currently zero-rows xts's do not play with merge.xts, which should be redefined 
  
}

store(br.hist)


## ----br.idx, opts.label='purlme'-----------------------------------------
br.idx=function(con, index, field="PX_LAST", start=Sys.Date()-7, end.date=Sys.Date(),

                include.idx=TRUE, showtype=FALSE,
                use.xts=TRUE, merge.xts=TRUE,

                ## br.bdh args
                option.names = NULL, option.values = NULL,
                only.trading.days = TRUE,

                ## Simulation args                                      
                nsec=10, sec.names = NULL,
                
                price=TRUE,
                mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                same.dates=FALSE, empty.sec=0,
                weekend=TRUE, holidays=NULL)
{   
    ## Check connection
    if(!is.null(con) && !.br.is.con(con)) stop('Invalid connection parameter')

    ## Check index format. Add 'INDEX' if missing
    if(!is.character(index)) stop('Index should be a string')
    if(length(index)>1) stop('Only one index')
    if(!grepl("INDEX$", toupper(index))) index=paste(index, 'INDEX') 

    ## Get index members
    if(is.null(con)) tiks=paste0('memb', 1:nsec) else{
        tiks=bds(con, index, 'INDX_MEMBERS')
        tiks=paste(tiks[[1]], 'Equity')
    }

    ## Check sec.names
    if(is.null(con) && !is.null(sec.names)) {
        if(!is.character(sec.names)) stop("'sec.names' should be a character vector")
        if(length(sec.names)!=nsec)
            stop("'sec.names' length should be equal to the number of index constituents")
        tiks=sec.names
    }
    
    ## Include index?
    if(include.idx) tiks=c(index, tiks)

    ## Get data
    br.hist(con=con, tiks=tiks, field=field, start=start, end.date=end.date,
        
            addtype=FALSE, showtype=showtype,
            use.xts=TRUE, merge.xts=TRUE,
        
            option.names=option.names, option.values=option.values,
            only.trading.days=only.trading.days,

            price=TRUE,
            mean=mean, sd=sd, jitter=jitter,
            same.dates=same.dates, empty.sec=empty.sec,
            weekend=TRUE, holidays=NULL)       
}

store(br.idx)


## ----br.sample, opts.label='purlme'--------------------------------------
br.sample=function(nrow=NULL,  price=TRUE,
                   start=Sys.Date() - 7, end.date=Sys.Date(), 
                   field="FIELD",
                   use.xts=TRUE, 
                   mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                   rand.dates=TRUE, weekend=TRUE, holidays=NULL,
                   nsec=NULL, no.na=NULL, df=NULL, sec.names=NULL, empty.sec=NULL, same.dates=NULL)
{

    ## Deprecated args
    .br.sample.deprecated(nsec, no.na, df, sec.names, empty.sec, same.dates)

    if(!require("xts", quietly=TRUE, character.only=TRUE))  stop("Can't find library xts")
   
    ## Test/set args 
    dates=.br.test.dates(start, end.date, holidays)
    if(is.null(nrow)) nrow=dates$end - dates$start
    nrow=as.numeric(nrow)
      
    ## Jitter 
    mean.jit= mean + runif(1, -jitter, jitter)
    
    ## Generate TS with fixed or random dates
    r= if(rand.dates) sample(1:nrow,1) else nrow 

    ## Generate time series as a DF
    TS=data.frame(d=sort(sample(dates$start+1:nrow-1,r)), f=round(rnorm(r,mean.jit, sd),3))
    if(price) TS$f=abs(TS$f) # Price always non-negative
    names(TS)=c("date", toupper(field))

        
    ## Remove weekends and holidays
    if(weekend){
        w=format(TS$date, "%u")
        TS= TS[w!="6" & w!="7", ]       
    }    
    if(!is.null(holidays)){
         TS=TS[!TS$date %in% holidays, ]       
    }

    ## Convert TSs to XTS's
    TS=as.matrix(TS)
    attr(TS, "types")=c("DATE","FLOAT64")
    if(nrow(TS)==0) TS=NULL

    
        
    ## Convert TSs to XTS's
    if(use.xts){
        TS=if(!is.null(TS)) xts(as.numeric(TS[,-1]), as.Date(TS[,1])) else NA
    }

    TS
}

store(br.sample)


## ----MISCFUNC, opts.label='purlme'---------------------------------------

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


## ----br.bdh, opts.label='purlme'-----------------------------------------
br.bdh=function(
                con, security, fields="PX_LAST",
                start.date=Sys.Date() - 7, end.date=Sys.Date(), 
                override_fields  = NULL, override_values = NULL,               
                option.names = NULL, option.values = NULL,
                only.trading.days = TRUE
                ) {
    
    if(is.null(con)) stop("'con' is NULL, but br.bdh() does not support simulated mode.")


    ## Set overrides
    if(is.null(override_fields)) {
        override_fields="IGNORE"
        override_values = ""
    }

    ## Test/set dates
    dates=.br.test.dates(start.date, end.date, asChar=TRUE)
    option.names=c(option.names, "startDate")
    option.values=c(option.values, dates$start)
    if(!is.null(end.date)) {
        option.names = c(option.names, "endDate")
        option.values = c(option.values, dates$end)
    }


    ## Non trading days
    if(!only.trading.days) {
        option.names=c("nonTradingDayFillOption", "nonTradingDayFillMethod", option.names)
        option.values=c("ALL_CALENDAR_DAYS", "NIL_VALUE", option.values)
    }

    ## Get reference data
    ref=con$blh(security, .jarray(fields),
            .jarray(override_fields), .jarray(override_values),
            .jarray(option.names), .jarray(option.values))
    ret=ref$getData()   
    ret=if(!is.null(ret)) .jevalArray(ret, simplify=TRUE) else ret

    ## Set data attributes
    if(!is.null(ret)) attr(ret, "types")=ref$getDataTypes()
    ## For info on data types Rbbg:::convert.to.type
    ## Consider also ref$getColumnNames()

    ret
}


.br.test.dates=function(start, end, holidays=NULL, asChar=FALSE){

    if(is.null(start <- br.try.date(start)))
        stop(paste('Invalid date', start)) 

    if(is.null(end <- br.try.date(end)))
        stop(paste('Invalid date', end))

    if(!is.null(holidays) &  is.null(holidays <- br.try.date(holidays)))
        stop("Some dates are not recognised: ", paste(holidays, collapse=" "))

    if(!br.is.same.class(c(list(start, end), holidays))){
        message("Start date: ", paste(class(start), collapse=" "))
        message("End date: ", paste(class(end), collapse=" "))
        if(!is.null(holidays))
            message("holidays: ", paste(class(holidays), collapse=" "))
        stop("Not all date variable have the same class")        
    }
    
    if(start>=end) stop("Start date should be set before end date!")

    if(asChar){
        start=format(start, format="%Y%m%d")
        end=format(end, format="%Y%m%d")
    }
    list(start=start, end=end, holidays=holidays)
    

}

.br.sample.deprecated=function(nsec=NULL, no.na=NULL, df=NULL,
                               sec.names=NULL, empty.sec=NULL, same.dates=NULL){

    if(!is.null(no.na))
        stop("'no.na' option is now deprecated:",
             " just use rand.dates=TRUE or apply na.omit to br.sample() output.")

    if(!is.null(df))
        stop("'df' option is now deprecated. Use: 'use.xts=FALSE'")
    
    if(!is.null(sec.names))
        stop("'sec.names' option is now deprecated, as br.sample() now gives only a single time series.")

    if(!is.null(empty.sec))
        stop("'empty.sec' option is now deprecated, as br.sample() now gives only a single time series.")

    if(!is.null(nsec))
        stop("'nsec' option is now deprecated, as br.sample() now gives only a single time series.\n",
             "Use 'br.hist(con=NULL, ...)' to simulate multiple securities.")

    if(!is.null(same.dates))
        stop("'same.dates' option is now deprecated. Use: 'rand.dates=FALSE'")

}


# br.bdh(con, "MSFT US EQUITY", "PX_LAST", "20170202", "20170210", override_fields="PRICING_SOURCE", override_values="CG")
# br.bdh(con, "MSFT US EQUITY", "PX_LAST", "20170202", "20170210")

###=============
  


####
# debug(br.hist) ; br.hist(con, c("MSFT US Equity", "AMZN US Equity"), start="20170225")
#debug(br.hist.csv)
#debug(br.hist)
# br.hist.csv(NULL, "wei.csv", same.dates=TRUE, empty.sec=.2)
# br.bdh(con, "MSFT US Equity", start="20170225")
# source("newbulk3.R")


store(br.try.date)
store(br.is.same.class) 
store(br.bdh)



## ----attach, opts.label='purlme'-----------------------------------------
### Make visible br.* in bloomr env and base ns
attach(bloomr)
rm(store)

