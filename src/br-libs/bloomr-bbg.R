## ----store, opts.label='purlme'----------------------------------------------------------------------------------------------------
## Purl this first
## Store br.* objects in dedicated namespace
bloomr.bbg <- new.env(parent=asNamespace("stats"))

## func: store(func);  var: store("var")
store=function(sym){
    if(is.function(sym)){
        name=deparse(substitute(sym))
        val=sym
    } else {
        name=sym
        val=get(sym)
    }

    assign(name, val, envir=bloomr.bbg)
    rm(list=name, envir=parent.frame())
}


## ----br.bdh, opts.label='purlme'---------------------------------------------------------------------------------------------------
br.bdh=function(
    con, securities, fields="PX_LAST", start.date, end.date = NULL,
    option.names = NULL, option.values = NULL,
    always.display.tickers = FALSE, dates.as.row.names = (length(securities) == 1),
    include.non.trading.days = NULL
    ) {
    if(is.null(con)) stop("'con' is NULL, but br.bdh() does not support simulated mode.")
    bdh(con, securities, fields, start.date, end.date, override_fields  = NULL, override_values = NULL,
        option.names, option.values, always.display.tickers, dates.as.row.names,
        include.non.trading.days)
}
store(br.bdh)

## ----br.bulk.csv, opts.label='purlme'----------------------------------------------------------------------------------------------
br.bulk.csv=function(con, file, start=Sys.Date()-5, field="PX_LAST", cols=NULL,
    addtype=FALSE, showtype=FALSE, use.xts=TRUE, comma=TRUE,
    price=TRUE, nrow=5, same.dates=FALSE, no.na=FALSE, empty.sec=0
    )
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
        x=list(br.bulk.tiks(con, csv[[g]],
            start, field, addtype, showtype, use.xts,
            price=price, nrow=nrow, same.dates=FALSE, no.na=FALSE, empty.sec=empty.sec))
        names(x)=gnams[g]
        grps=c(grps, x)
    }
    if(length(grps)==1) grps=grps[[1]]
    grps
}
store(br.bulk.csv)

## ----br.bulk.desc, opts.label='purlme'---------------------------------------------------------------------------------------------
br.bulk.desc=function(con, tiks) {

    LL = lapply(tiks, function(tik){
        message('Reading ', tik)
        br.desc(con, tik)
    })
    names(LL)=tiks
    LL
}
store(br.bulk.desc)

## ----br.bulk.idx, opts.label='purlme'----------------------------------------------------------------------------------------------
br.bulk.idx=function(con, index, start=Sys.Date()-5, field="PX_LAST", showtype=FALSE,
    include.idx=TRUE, use.xts=TRUE,
    nsec=10, price=TRUE, nrow=5,
    same.dates=FALSE, no.na=FALSE, empty.sec=0, sec.names = NULL
    )
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
        if(length(sec.names)!=nsec) stop("'sec.names' length should be equal to the number of index constituents")
        tiks=sec.names
    }

    ## Include index?
    if(include.idx) tiks=c(tiks, index)

    ## Get data
    br.bulk.tiks(
        con=con,
        tiks=tiks,
        start=start,
        field=field,
        addtype=FALSE,
        showtype=showtype,
        use.xts=use.xts,
        price=price,
        nrow=nrow,
        same.dates=same.dates,
        no.na=no.na,
        empty.sec=empty.sec
        )
}
store(br.bulk.idx)

## ----br.bulk.tiks, opts.label='purlme'---------------------------------------------------------------------------------------------
br.bulk.tiks=function(
    con,
    tiks,
    start=Sys.Date()-5, field="PX_LAST",
    addtype=FALSE, showtype=FALSE, use.xts=TRUE,
    price=TRUE, nrow=5, same.dates=FALSE, no.na=FALSE, empty.sec=0
    )
{

    ## Check connection
    if(!is.null(con) && !.br.is.con(con)) stop('Invalid connection parameter')

    ## Check tickers (skip possible empty CSV cells)
    if(!is.character(tiks)) stop('Tickers should be in the form of a character vector')
    tiks=tiks[!is.na(tiks)]
    tiks=tiks[tiks!='']

    ## Check start date
    if(is.na(as.Date(start, format='%Y%m%d'))) stop(paste('Invalida date', start))

    ## Check security type to add/show
    .br.check.type(addtype)
    .br.check.type(showtype)
    if(addtype==TRUE)  addtype="Equity"
    if(addtype!=FALSE) tiks=paste(tiks, addtype)
    if(!showtype) tiks.show=.br.cuttype(tiks) else tiks.show=tiks

    ## Check xts library availability
    if(use.xts && !require("xts", quietly=TRUE, character.only=TRUE))  stop("Can't find library xts")

    ## Get data as an xts class
    if(use.xts){
        LL = lapply(tiks, function(tik){
            message('Loading ', tik)
            if(!is.null(con)) x=br.bdh(con, tik, field, start) else {
                x=br.sample(nrow, 1, price=price, start=start,
                    df=TRUE, same.dates=same.dates, no.na=no.na,
                    sec.names=c('date', field))
            }
            x=xts(x[-1], as.Date (x[[1]]))
            if(nrow(x)==0) x=NA else x
        })

        ## Randomly identify empty.sec
        if(is.null(con)){
            x=round(length(tiks.show) * empty.sec)
            empty=sample(length(tiks.show), x)
            LL[empty] = NA
        }

        ##print(LL ) ###################
        ## If there are only NAs, output an empty xts else merge ts and in simul. mode use no.na val
        if(all(is.na(LL))) LL=na.omit(xts(t(unlist(LL)), Sys.Date())) else {
          #  if(!is.null(con)) no.na=FALSE # in merge c(LL, all=!no.na)
            if(length(LL)>1) LL=do.call("merge.xts", LL) else LL=LL[[1]]
        }

        ## Set labels
        if(nrow(LL)>0) names(LL) = tiks.show  else  dimnames(LL)= list(NULL,tiks.show)
        LL

    } else {
        ## Get data in list format
        LL=lapply(tiks, function(tik){
            message('Loading ', tik)
            if(!is.null(con)) br.bdh(con, tik, field, start) else {
                br.sample(nrow, 1, price=price, empty.sec=empty.sec, start=start,
                           df=TRUE, sec.names=c('date', field))
            }
        })

        ## Randomly identify empty.sec
        if(is.null(con)){
            x=round(length(tiks.show) * empty.sec)
            empty=sample(length(tiks.show), x)
            LL[empty] = NA
        }

        setNames(LL, tiks.show)
    }
}
store(br.bulk.tiks)

## ----br.desc, opts.label='purlme'--------------------------------------------------------------------------------------------------
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
    if(!is.null(xx)) colnames(xx) = colnames(x)
    rnams=c(rownames(x), rownames(xx))
    x=rbind(x,xx)
    rownames(x)=rnams
    x
}
store(br.desc)

## ----br.sample, opts.label='purlme'------------------------------------------------------------------------------------------------
br.sample=function(nrow, nsec=1, price=TRUE, start=Sys.Date(), mean=ifelse(price, 10, 0.1), sd=1,
    jitter=0, same.dates=FALSE, no.na=FALSE, df=FALSE, empty.sec=0, sec.names=NULL)
{

    if(!require("xts", quietly=TRUE, character.only=TRUE))  stop("Can't find library xts")

    ## Start can be Date class or ISO string without sep
    if(is.na(as.Date(start, format='%Y%m%d'))) stop(paste('Invalida date', start)) else
        start=as.Date(start, '%Y%m%d')

    ## Check sec.names
    if(!is.null(sec.names)) {
        if(!is.character(sec.names)) stop("'sec.names' should be a character vector")
        if(!df && length(sec.names)!=nsec) stop("'sec.names' length should be equal to 'nsec'")
        if(df && length(sec.names)!=nsec+1) stop("'sec.names' length should be equal to 'nsec'+1")
    }

    ## Check empty.sec is a ratio
    if(empty.sec<0 || empty.sec>1) stop("'empty.sec' must be between 0 and 1")

    ## Randomly identify empty.sec
    x=round(nsec * empty.sec)
    empty=sample(nsec, x)

    ## Make xts matrix
    tss=lapply(1:nsec, function(col){

        ## Jitter
        mean.jit= mean + runif(1, -jitter, jitter)

        ## Generate TS with fixed or random dates
        if(same.dates) r=nrow else r=sample(1:nrow,1)

        ## Generate column if not among the empty ones
        if(col %in% empty){
            x=NA
        } else {
            x=xts(round(rnorm(r,mean.jit, sd),3), sort(sample(start+1:nrow-1,r)))
            if(price) coredata(x)=abs(coredata(x))   # Price always non-negative
        }

        x
    })

    ## If there are only NAs cells output an empty date/val xts else merge and fill empty cells with NAs
    if(all(is.na(tss))) tss=na.omit(xts(t(unlist(tss)), Sys.Date())) else {
        if(length(tss)>1) tss=do.call("merge.xts", tss) else
        tss=tss[[1]]
    }

    ## Remove NAs
    if(no.na) tss=na.omit(tss)

    ## Set labels
    x=paste0('sample', 1:ncol(tss))
    if(nrow(tss)>0)  names(tss) = x  else  dimnames(tss)= list(NULL,x)

    ## Convert to data frame
    if(df) tss=data.frame(date=time(tss), tss)

    if(is.null(sec.names)) tss else
       setNames(tss, sec.names)

}
store(br.sample)

## ----bbg-internal, opts.label='purlme'---------------------------------------------------------------------------------------------

## Check connection token
.br.is.con=function(con) identical(attr(con, 'jclass'), "org/findata/blpwrapper/Connection")

## Legal security types
.br.types=c('Govt', 'Corp', 'Mtge', 'M-Mkt', 'Muni', 'Pfd', 'Equity', 'Comdty', 'Index', 'Curncy')

## Check security type
.br.check.type=function(type) {
    if(is.character(type)){
	x=toupper(type)
	xx=toupper(.br.types)
	if(!any(xx %in% x)) stop(paste(x, 'not in', paste(xx, collapse=' ')))
    }
}

## Cut trailing security type from character vector
.br.cuttype=function(type){
    p=paste0(' +', .br.types, '$|', collapse='')
    p=sub('\\|$', '', p)
    sub(p, '', type, ignore.case=TRUE)
}


.br.jar=function(){
    jarpath=.br.home("/blpapi/bin")
    Sys.glob(file.path(jarpath,  "blpapi-[0-9]*.jar"))
    }


store(.br.is.con)
store(".br.types")
store(.br.check.type)
store(.br.cuttype)
store(.br.jar)

## ----connections, opts.label='purlme'----------------------------------------------------------------------------------------------
br.open=function() blpConnect(blpapi.jar.file=.br.jar())
br.close=function(conn) if(!is.null(conn)) blpDisconnect(conn)

store(br.open)
store(br.close)

## ----attach, opts.label='purlme'---------------------------------------------------------------------------------------------------
### Make visible br.* in bloomr env and base ns
attach(bloomr.bbg)
rm(store)
rm(bloomr.bbg)


