


## ----br.bulk.csv, include=FALSE------------------------------------------
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
        cat('Processing', gnams[g],  '...\n')
        x=list(br.bulk.tiks(con, csv[[g]],
            start, field, addtype, showtype, use.xts,
            price=price, nrow=nrow, same.dates=FALSE, no.na=FALSE, empty.sec=empty.sec))
        names(x)=gnams[g]
        grps=c(grps, x)        
    }
    if(length(grps)==1) grps=grps[[1]]
    grps
}




























## ----br.bulk.desc, include=FALSE-----------------------------------------
br.bulk.desc=function(con, tiks) {

    LL = lapply(tiks, function(tik){
        cat('Reading', tik,  '\n')
        br.desc(con, tik)             
    })
    names(LL)=tiks
    LL
}





## ----br.bulk.idx, include=FALSE------------------------------------------
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




## ----br.bulk.tiks, include=FALSE-----------------------------------------
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
            cat('Loading', tik,  '\n')
            if(!is.null(con)) x=bdh(con, tik, field, start) else {
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
        
        ## If there are only NAs cells output an empty xts else merge and fill empty cells with NAs 
        if(all(is.na(LL))) LL=na.omit(xts(t(unlist(LL)), Sys.Date())) else {
            if(length(LL)>1) LL=do.call("merge.xts", LL) else
            LL=LL[[1]]
        }
    
        ## Set labels
        if(nrow(LL)>0) names(LL) = tiks.show  else  dimnames(LL)= list(NULL,tiks.show)
        LL
        
    } else {
        ## Get data in list format
        LL=lapply(tiks, function(tik){
            cat('Loading', tik,  '\n')
            if(!is.null(con)) bdh(con, tik, field, start) else {
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


## ----br.desc, include=FALSE----------------------------------------------
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


## ----br.sample, include=FALSE--------------------------------------------
br.sample=function(nrow, nsec=1, price=TRUE, start=Sys.Date(), mean=ifelse(price, 10, 0.1), sd=1,
    jitter=0, same.dates=FALSE, no.na=FALSE, df=FALSE, empty.sec=0,sec.names=NULL)
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


## ----deprecated, include=FALSE-------------------------------------------
bbg.open=function() stop("Sorry 'bbg.open' is now deprecated. Please use br.open().")
bbg.close=function(con) stop("Sorry 'bbg.close' is now deprecated. Please use br.close().")



## ----bbg-internal, include=FALSE-----------------------------------------

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
	jarpath=paste0(R.home(), "/blpapi_java/bin")
        Sys.glob(file.path(jarpath,  "blpapi-[0-9]*.jar"))
    }





## ----connections, include=FALSE------------------------------------------
br.open=function() blpConnect(blpapi.jar.file=.br.jar())
br.close=function(conn) if(!is.null(conn)) blpDisconnect(conn)



## ----miscfunc, include=FALSE---------------------------------------------

#Clean up
## Remove visible and invisible objects
rm.all=function() 
    rm(list=ls(all=TRUE, envir=parent.frame()), envir=parent.frame())

## Remove visible non-function objects
rm.var=function() 
    rm(list=setdiff(ls(envir=parent.frame()), lsf.str(envir=parent.frame())),  envir=parent.frame())



## ----time, include=FALSE-------------------------------------------------
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
    x1=day.mod(d1,1);x2=day.mod(d2,1);
    x=seq(x1, x2, by="1 month")
    #last day of each month in seq
    x=sapply(x, last.day)
    #count 31d-months
    x=length(which(x>30))
    #substract 1 for each 31d-month
    as.numeric(d2-d1-x)
}







