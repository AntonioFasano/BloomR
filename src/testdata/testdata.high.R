
main=function(){
br.beta()
con=br.open()

dc=function(secs, ...){
    usr=list(...)
    all=c(securities=list(secs), fields= "PX_LAST", start_date="20150101", end_date="20150201", usr)
    dt=do.call("bdh", c(CONN, all))
    list(all=all, secs=secs, usr=usr, dt=dt)
    
}
CONN=con; T=TRUE; F=FALSE

secs <- c("MSFT US Equity")
a=dc(secs, always.display.tickers=F, dates.as.row.names=F, include.non.trading.days=T)
b=dc(secs, always.display.tickers=T, dates.as.row.names=F, include.non.trading.days=T)
c=dc(secs, always.display.tickers=F, dates.as.row.names=T, include.non.trading.days=T)
d=dc(secs, always.display.tickers=T, dates.as.row.names=T, include.non.trading.days=T)

secs <- c("AMZN US Equity", "MSFT US Equity")
aa=dc(secs, always.display.tickers = F, dates.as.row.names=F, include.non.trading.days = T) # always.display.tickers ineffective
bb=dc(secs, always.display.tickers = T, dates.as.row.names=F, include.non.trading.days = T)
## cc=dc(secs, always.display.tickers = F, dates.as.row.names=T, include.non.trading.days = T) # error
## dd=dc(secs, always.display.tickers = T, dates.as.row.names=T, include.non.trading.days = T) # error
ee=dc(secs, include.non.trading.days = FALSE)

## Fund liquidated in 2013: AGORASM RU
secs <- c("AGORASM RU Equity")
la=dc(secs, always.display.tickers = F, dates.as.row.names=F, include.non.trading.days = T)
lb=dc(secs, always.display.tickers = T, dates.as.row.names=F, include.non.trading.days = T)
lc=dc(secs, always.display.tickers = F, dates.as.row.names=T, include.non.trading.days = T)
ld=dc(secs, always.display.tickers = T, dates.as.row.names=T, include.non.trading.days = T)

## Fund liquidated in 2013: AGORASM RU, only trading days
secs <- c("AGORASM RU Equity")
lat=dc(secs, always.display.tickers = F, dates.as.row.names=F, include.non.trading.days = F)
lbt=dc(secs, always.display.tickers = T, dates.as.row.names=F, include.non.trading.days = F)
lct=dc(secs, always.display.tickers = F, dates.as.row.names=T, include.non.trading.days = F)
ldt=dc(secs, always.display.tickers = T, dates.as.row.names=T, include.non.trading.days = F)

## Fund liquidated in 2013: AGORASM RU and active
secs <- c("MSFT US Equity", "AGORASM RU Equity")
laa=dc(secs, always.display.tickers = F, dates.as.row.names=F, include.non.trading.days = T)
lbb=dc(secs, always.display.tickers = T, dates.as.row.names=F, include.non.trading.days = T)
## lcc=dc(secs, always.display.tickers = F, dates.as.row.names=T, include.non.trading.days = T)
## ldd=dc(secs, always.display.tickers = T, dates.as.row.names=T, include.non.trading.days = T) # error
lee=dc(secs, include.non.trading.days = FALSE)


ds=br.desc(CONN, "MSFT US Equity")

save(a,b,c,d,
     aa,bb,ee,
     la,lb,lc,ld,
     laa,lbb,
     lat,lbt,lct,ldt,
     ds,
     file="testdata.high.Rdata")


}

main()
