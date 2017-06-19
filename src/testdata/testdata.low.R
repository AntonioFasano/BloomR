
br.raw_=function( # br.raw work horse
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
            
    optnams=c(optnams, "startDate", "endDate")
    optvals=c(optvals, startDate, endDate)

    if(alldays) {
        optnams=c(optnams, "nonTradingDayFillOption", "nonTradingDayFillMethod")
        optvals=c(optvals, "ALL_CALENDAR_DAYS", "NIL_VALUE")
    }

    ref=.jnew("org/findata/blpwrapper/Connection") 

    r=.jcall(ref, "Lorg/findata/blpwrapper/DataResult;", "blh",
             sec, .jarray(field), 
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

    list(symb=sec, vals=matrix.data, types=data.types)
    
}


histdeb=function(sec, field="PX_LAST", start="20150101", end,
                 alldays=FALSE,
                 optnams=NULL, optvals=NULL,
                 ovrnams=NULL, ovrvals=NULL){

    if(missing(sec)) sec=get("SEC", parent.frame())
    if(missing(end)) end=get("END", parent.frame())
    
    dt=br.raw_(security=sec, field=field, startDate=start, endDate=end, alldays=alldays,   
                optnams=optnams, optvals=optvals, ovrnams=ovrnams, ovrvals=ovrvals)

    list(symb=dt$symb, vals=dt$vals, types=dt$types,
         field=field, start=start, end=end,
         alldays=alldays,
         optnams=optnams, optvals=ovrvals,
         ovrnams=ovrnams, ovrvals=ovrvals)  
}


main=function(bbgjar=NULL){

## With blpConnect in deb.R  blpapi.jar.file can be a relative path
if(missing(bbgjar)) bbgjar="blpapi-3.8.8-2.jar"     
CONN=blpConnect(log.level="info",  blpapi.jar.file=bbgjar)

F=FALSE;T=TRUE

message("Daily")
END="20150201"

SEC=c("MSFT US Equity")
msft_daily=histdeb(alldays=F)
msft_daily_all=histdeb(alldays=T)

SEC=c("AMZN US Equity")
amzn_daily=histdeb(alldays=F)
amzn_daily_all=histdeb(alldays=T)

SEC=c("AGORASM RU Equity") # liquidated 
liq_daily=histdeb(alldays=F)
liq_daily_all=histdeb(alldays=T)


message("Monthly")
END="20151201"

SEC=c("MSFT US Equity")
msft_month=histdeb(alldays=F, optnams="periodicitySelection", optvals="MONTHLY")
msft_month_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="MONTHLY")

SEC=c("AMZN US Equity")
amzn_month=histdeb(alldays=F,  optnams="periodicitySelection", optvals="MONTHLY")
amzn_month_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="MONTHLY")

SEC=c("AGORASM RU Equity") # liquidated 
liq_month=histdeb(alldays=F,  optnams="periodicitySelection", optvals="MONTHLY")
liq_month_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="MONTHLY")

message("Weekly")
END="20150801"

SEC=c("MSFT US Equity")
msft_week=histdeb(alldays=F, optnams="periodicitySelection", optvals="WEEKLY")
msft_week_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="WEEKLY")

SEC=c("AMZN US Equity")
amzn_week=histdeb(alldays=F, optnams="periodicitySelection", optvals="WEEKLY")
amzn_week_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="WEEKLY")

SEC=c("AGORASM RU Equity") # liquidated 
liq_week=histdeb(alldays=F, optnams="periodicitySelection", optvals="WEEKLY")
liq_week_all=histdeb(alldays=T, optnams="periodicitySelection", optvals="WEEKLY")

message("Override Fields")
SEC=c("VGTSX US Equity")
END="20150201"
ovrcur_none_daily=histdeb(alldays=F) # hre like USD
ovrcur_eur_daily=histdeb(alldays=F, ovrnams = "EQY_FUND_CRNCY", ovrvals = "EUR")
ovrcur_usd_daily=histdeb(alldays=F, ovrnams = "EQY_FUND_CRNCY", ovrvals = "USD")

D=list(
msft_daily       =msft_daily        ,     
amzn_daily       =amzn_daily        , 
liq_daily        =liq_daily         , 
                                   
msft_daily_all   =msft_daily_all    , 
amzn_daily_all   =amzn_daily_all    , 
liq_daily_all    =liq_daily_all     , 
                                   
msft_week        =msft_week         ,    
amzn_week        =amzn_week         , 
liq_week         =liq_week          , 
                                    
msft_week_all    =msft_week_all     , 
amzn_week_all    =amzn_week_all     , 
liq_week_all     =liq_week_all      , 
                                   
msft_month       =msft_month        ,     
amzn_month       =amzn_month        , 
liq_month        =liq_month         , 
                                    
msft_month_all   =msft_month_all    , 
amzn_month_all   =amzn_month_all    , 
liq_month_all    =liq_month_all     ,
                  
ovrcur_none_daily=ovrcur_none_daily ,
ovrcur_eur_daily =ovrcur_eur_daily  ,
ovrcur_usd_daily =ovrcur_usd_daily  )

CN=list(
    class   = class(CONN),
    attribs = attributes(CONN),
    s4=isS4(CONN),
    is.jobjRef="jobjRef"==class(CONN),
    .jclass = .jclass(CONN)
)


save(D, CN, file="testdata.low.Rdata")
}

main(bbgjar=.br.jar()) 




