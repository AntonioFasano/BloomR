## ----store, opts.label='brfuncs'-----------------------------------------
## Store br.* objects in bloomr env in base namespace
#if( ! grepl("^bloomr\\.beta\\.", current_input()))
#    assign('bloomr',  new.env(parent=asNamespace("base")), envir=asNamespace("base"))

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


## ----br.bdh, opts.label='brfuncs'----------------------------------------
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
store(br.bdh)

## ----br.hist.csv, opts.label='brfuncs'-----------------------------------
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


## ----br.bulk.desc, opts.label='brfuncs'----------------------------------
br.bulk.desc=function(con, tiks) {

    LL = lapply(tiks, function(tik){
        message('Reading ', tik)
        br.desc(con, tik)             
    })
    names(LL)=tiks
    LL
}
store(br.bulk.desc)

## ----br.idx, opts.label='brfuncs'----------------------------------------
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


## ----br.hist, opts.label='brfuncs'---------------------------------------
br.hist=function(con,
                 
                 #' Character vector of the tickers queried for data
                 tiks, field="PX_LAST",
                 #' Start date can be a POSIXlt/ct, Date, ISO string, UK string with slashes or dashes
                 start=Sys.Date()-7, end.date=Sys.Date(), #' same format of start
                                            
                 addtype=FALSE, showtype=FALSE,

                 #' if TRUE (the default) time series are formatted as xts objects else as a data frame.  
                 use.xts=TRUE,

                 #' if TRUE (the default) xts objects are merged using all rows
                 #' and using NAs for missing observations.
                 merge.xts=TRUE,   
                 
                 ## br.bdh args
                 option.names = NULL, option.values = NULL,
                 override.names = NULL, override.values = NULL,                 
                 only.trading.days = TRUE,

                 ## Simulation args                      
                 price=TRUE,
                 mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                 same.dates=FALSE, empty.sec=0,
                 weekend=TRUE, holidays=NULL)
{



    .br.session$set("staten", 0)
    
    ## Check connection
    usesample=br.is.sim()
    if(!usesample && !.br.is.con(con)) stop('Invalid connection parameter') 
    if(.br.is.dev()) usesample=FALSE
    
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
    
    ## br.raw() and sample() pars
    rawpars=list(
        #con=con,
        field=field, start.date=start, end.date=end.date, 
        optnams=option.names, optvals=option.values,
        ovrnams=override.names, ovrvals=override.values,
        alldays=!only.trading.days)
    
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
        if(usesample) do.call("br.sample", sampars)
          else do.call(".br.raw", c(security=tik, rawpars))
    })

    ## Randomly identify empty.sec
    if(usesample){        
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


.br.raw=function( # Get low level  historical data. For internal use
                 security, field="PX_LAST",
                 ## Start/End date can be a POSIXlt/ct, Date, ISO string, UK string with slashes or dashes.
                 start.date, end.date=Sys.Date(),
                 alldays=FALSE, # shortcut for BBG nonTradingDayFillOption 
                 optnams=NULL, optvals=NULL,
                 ovrnams=NULL, ovrvals=NULL
                 ){



    ## Test/set dates
    dates=.br.test.dates(start.date, end.date, asChar=TRUE)
    
    ## In dev mode use stored test data 
    if(.br.is.dev()) {
        tsecs=.br.choose.testdata(security=security, field=field,
                  startDate=dates$start, endDate=dates$end,  # ISO string
                  alldays=alldays, # shortcut for BBG nonTradingDayFillOption 
                  optnams=optnams, optvals=optvals,
                  ovrnams=ovrnams, ovrvals=ovrvals)
        

        ## On each call take the next available test sec 
        n=get(".br.session", pos = "bloomr")$staten+1 
        l=length(tsecs)
        pos = if(n <=l) n else n - ((n -1) %/% l) * l
        .br.session$set("staten", n)
        d=tsecs[[pos]]

    } else {



        d=.br.raw_(security=security, field=field,
                  startDate=dates$start, endDate=dates$end,  # ISO string
                  alldays=alldays, # shortcut for BBG nonTradingDayFillOption 
                  optnams=optnams, optvals=optvals,
                  ovrnams=ovrnams, ovrvals=ovrvals)

    }
    x=.br.convert.to.type(d$vals, d$types)

    ## This a design choice, as merge.xts() does not currently play well with zero-rows objects
    ## If changed, fix br.hist() xts conversion and br.sample() NULL set and xts conversion 
    if(nrow(x)==0) NULL else x
}


.br.raw_=function( # br.raw work horse
                 security, field="PX_LAST",
                 startDate, endDate,    # ISO string
                 alldays=FALSE,         # shortcut for BBG nonTradingDayFillOption 
                 optnams=NULL, optvals=NULL,
                 ovrnams=NULL, ovrvals=NULL){

    if(is.null(ovrnams)) {
        za=.jarray(character(0), "java/lang/String")
        ovrnams=za
        ovrvals=za        
    } else {
        ovrnams=.jarray(ovrnams)
        ovrvals=.jarray(ovrvals)
    }

    #start=ifelse(is.character(start), start, format(start, "%Y%m%d"))
    #end=ifelse(is.character(end), end, format(end, "%Y%m%d"))

    ## API allows to pass only start date    
    optnams=c(optnams, "startDate", "endDate")
    optvals=c(optvals, startDate, endDate)

    if(alldays) {
        optnams=c(optnams, "nonTradingDayFillOption", "nonTradingDayFillMethod")
        optvals=c(optvals, "ALL_CALENDAR_DAYS", "NIL_VALUE")
    }

    ref=.jnew("org/findata/blpwrapper/Connection") 

    r=.jcall(ref, "Lorg/findata/blpwrapper/DataResult;", "blh",
             security, .jarray(field), 
             ovrnams,          # override fields
             ovrvals,          # override values
             .jarray(optnams), # option names
             .jarray(optvals)  # option _values
             )
    matrix.data  = r$getData()
    column.names = r$getColumnNames()
    data.types   = r$getDataTypes()

    if (is.null(matrix.data)) {
        matrix.data <- matrix(, nrow = 0, ncol = length(column.names))
    } else {
        matrix.data <- .jevalArray(matrix.data, simplify = TRUE)
    }
    colnames(matrix.data)= column.names

    list(symb=security, vals=matrix.data, types=data.types)
    
}

## from Rbbg:
.br.convert.to.type <- function( # Convert br.raw_ string data to proper format. Internal 
                    df.data, data_types) {
  for (i in 1:(dim(df.data)[2])) {
    string_values = as.vector(df.data[,i])

    new_values <- switch(data_types[i],
        FLOAT64 = as.numeric(string_values),
        INT32 = as.numeric(string_values),
        INT64 = as.numeric(string_values),
        STRING = string_values,
        DATE = string_values,
        DATETIME = string_values,
        NOT_APPLICABLE = string_values,
        CHAR = string_values == 'Y', # Assumes CHAR is only used for Boolean values and can be trusted to return 'Y' or 'N'.
        stop(paste("unknown type", data_types[i]))
        )
    df.data[,i] <- new_values
  }

  return(df.data)
}

.br.choose.testdata=function(  # Choose what test data to use for what args
    security, field="PX_LAST",
    startDate, endDate,    # ISO string
    alldays=FALSE,         # shortcut for BBG nonTradingDayFillOption 
    optnams=NULL, optvals=NULL,
    ovrnams=NULL, ovrvals=NULL){

### To get local available labels
### local({load("testdata/testdata.low.Rdata"); names(D)})

    
    ## Standard call
    tnams=c("msft_daily", "amzn_daily", "liq_daily")

    ## All days set
    if(alldays) 
        tnams=c("msft_daily_all", "amzn_daily_all", "liq_daily_all")

    ## Monthly freq
    if(grepl("periodicitySelection", optnams) &&
       optvals[grep("periodicitySelection", optnams)]=="MONTHLY")
        tnams=c("msft_month", "amzn_month", "liq_month")


    ## Set manually from global env
    if(exists("TNAMS") && !is.null(TNAMS)) tnams=TNAMS

    ## Get data and test labels exist
    D=local({
        load("testdata/testdata.low.Rdata")
        D
    })
    if(!all(tnams %in% names(D)))
        stop(sprintf("In:\n%s\nnot all of:\n%s\nare found.",
                     paste(names(D), collapse=" "), paste(tnams, collapse=" ")))

    D[tnams]

    
}


store(br.hist)
store(.br.raw)
store(.br.raw_)
store(.br.convert.to.type)
store(.br.choose.testdata)


## ----br.desc, opts.label='brfuncs'---------------------------------------
br.desc=function(con, tik)
{

    ## Check connection
    if(!.br.is.con(con)) stop('Invalid connection parameter') 

    ## Check ticker format
    if(!is.character(tik)) stop('The ticker should be a string')
    if(length(tik)>1) stop('Only one  ticker')
       
    ## Short description fields as data frame
    des=paste0('ds00', 1:9)
    des=des[-7] # not usually working 
    x=bdp(con, tik, des)
    x=data.frame(t(x), stringsAsFactors=FALSE)

    ## Long description field
    xx=bds(con, tik, 'CIE_DES_BULK')

    ## Merge fields add long desc to DF
    if(is.null(xx)) xx=data.frame(NA)
    colnames(xx) = colnames(x)
    rnams=c(rownames(x), rownames(xx))
    x=rbind(x,xx)
    rownames(x)=rnams
    x
}
store(br.desc)

## ----br.md2pdf, opts.label='brfuncs'-------------------------------------

br.md2pdf=function(md.file, pdf.file){
### Make a markdown file into a PDF
## It assumes that you have installed the BloomR LaTeX addons

    ## Set pandoc and LaTeX exe and dir 
    panexe=dbr.brmain("pandoc/bin/pandoc.exe")
    if(!file.exists(panexe))
        stop(paste("Unable to find:", panexe, '\nDid you install BloomR LaTeX addons?'))
    latbin=dbr.brmain("latex/miktex/bin")
    if(!file.exists(latbin))
        stop(paste("Unable to find:", latbin, '\nDid you install BloomR LaTeX addons?'))

    ## Shell escape
    panexe=.br.wpath(panexe)

    ## Set system Path to LaTeX bin
    old.path=Sys.getenv('Path')
    x=paste0(Sys.getenv("Path"), ';', gsub('/', '\\\\',  latbin))
    Sys.setenv(Path=x)

    cmd=paste(panexe, .br.wpath(md.file), '-o', .br.wpath(pdf.file))
    out  <-  system(cmd, intern = TRUE, invisible = FALSE)

    ## Restore origina system Path
    Sys.setenv(Path=old.path)

    ## Return errors if any
    if(!is.null(attr(out, 'status')))  message(paste(out, collapse="\n"))

    invisible(out)

}
store(br.md2pdf)

## ----br.sample, opts.label='brfuncs'-------------------------------------
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


## ----bbg-internal, opts.label='brfuncs'----------------------------------

## Check connection token
.br.is.con=function(con) identical(attr(con, 'jclass'), "org/findata/blpwrapper/Connection")

## Legal security types
.br.types=c('Govt', 'Corp', 'Mtge', 'M-Mkt', 'Muni', 'Pfd', 'Equity', 'Comdty', 'Index', 'Curncy')

## Check security type
.br.check.type=function(type) {
    if(is.character(type)){
	x=toupper(type)
	xx=toupper(get(".br.types", pos = "bloomr"))
	if(!any(xx %in% x)) stop(paste(x, 'not in', paste(xx, collapse=' ')))
    }
}

## Cut trailing security type from character vector 
.br.cuttype=function(type){
    p=paste0(' +', get(".br.types", pos = "bloomr"), '$|', collapse='')
    p=sub('\\|$', '', p)
    sub(p, '', type, ignore.case=TRUE)
}


.br.jar=function(){
    jarpath=dbr.brmain("/blpapi/bin")
    Sys.glob(file.path(jarpath,  "blpapi-[0-9]*.jar"))
    }



.br.test.dates=function( # Test if start, end, holidays dates and have coherent formats and values.
                        ## Return list(start, end, holidays) converted to ISO if asChar=T.
                        ## See br.try.date() for valid formats
                        start, end, holidays=NULL, asChar=FALSE){

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


## Session variable. To be modified by .br.session$set()
.br.session=list()
.br.session$simulated=FALSE 
.br.session$usetest=FALSE 
.br.session$tokenasked=FALSE 
.br.session$tokensuccess=FALSE
.br.session$set=function(lab, val) assign(".br.session",
                                          `[[<-`(.br.session, lab, val), pos = "bloomr")

.br.developer=function(mode=TRUE) { # Set developer, modifying some function behaviour for debugging
    .br.session$set("usetest", mode)
    if(mode)
        warning("You are in developer mode.\nUse\n .br.developer(mode=FALSE)\n to go back to normal use.")
}

.br.is.dev=function() { # Test if in developer mode
    get(".br.session", pos = "bloomr")$usetest
}


## .br.raw, .br.raw_, .br.choose.testdata, 
## are under br.hist chunk

store(.br.is.con)
store(".br.types")
store(.br.check.type) 
store(.br.cuttype)
store(.br.jar)
store(".br.session")
store(.br.test.dates)
store(.br.developer)
store(.br.is.dev)


## ----connections, opts.label='brfuncs'-----------------------------------
br.open=function() {

    ## Simulation mode
    if(br.is.sim()) return(NULL)

    ## Developer mode
    if(.br.is.dev()) {
        out="devmode"
        attr(out, 'jclass') = "org/findata/blpwrapper/Connection"
        return(out)
    }
    
    .br.session$set("tokenasked", TRUE)
    out=blpConnect(blpapi.jar.file=.br.jar())
    .br.session$set("tokensuccess", TRUE)
    out
}

br.close=function(conn) {
    if(!is.null(conn)) blpDisconnect(conn)
    .br.session$set("tokenasked", FALSE)
    .br.session$set("tokensuccess", FALSE)
}

br.simulate=function(is=TRUE) {
    .br.session$set("simulated", is)
}

br.is.sim=function() {
    get(".br.session", pos = "bloomr")$simulated
}


store(br.open)
store(br.close)
store(br.simulate)
store(br.is.sim)


## ----miscfunc, opts.label='brfuncs'--------------------------------------

#Clean up
## Remove visible and invisible objects
rm.all=function() 
    rm(list=ls(all=TRUE, envir=parent.frame()), envir=parent.frame())

## Remove visible non-function objects
rm.var=function() 
    rm(list=setdiff(ls(envir=parent.frame()), lsf.str(envir=parent.frame())),  envir=parent.frame())

store(rm.all)
store(rm.var)

## ----betafun, opts.label='brfuncs'---------------------------------------

br.beta=function(){
    f=paste0(R.home("share"), "/bloomr/bloomr.beta.R")    
    if(file.exists(f)) source(f)  else message("No beta functionalities in this release")
}

store(br.beta)


## ----MISCFUNC, opts.label='brfuncs'--------------------------------------

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



## ----deprecated, opts.label='brfuncs'------------------------------------

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


## ----time, opts.label='brfuncs'------------------------------------------
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
store(`%+%.Date`)
store(`%-%.Date`)
store(last.day)
store(day.us)


## ----attach, opts.label='brfuncs'----------------------------------------
### Make visible br.* in bloomr env and base ns
attach(bloomr)
rm(store)

