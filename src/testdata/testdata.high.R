
main=function(){
br.beta()
con=br.open()

dc=function(secs, ...){
    usr=list(...)
    all=c(tiks=list(secs), field= "PX_LAST", start="20150101", end.date="20150201", usr)
    dt=do.call("br.hist", c(CONN, all))
    list(all=all, secs=secs, usr=usr, dt=dt)
    
}
CONN=con; T=TRUE; F=FALSE

secs <- c("MSFT US Equity")
a=dc(secs, use.xts=F, merge.xts=F, only.trading.days=T)
b=dc(secs, use.xts=T, merge.xts=F, only.trading.days=T)
c=dc(secs, use.xts=F, merge.xts=T, only.trading.days=T)
d=dc(secs, use.xts=T, merge.xts=T, only.trading.days=T)

## only trading days=F
secs <- c("MSFT US Equity")
at=dc(secs, use.xts=F, merge.xts=F, only.trading.days=F)
bt=dc(secs, use.xts=T, merge.xts=F, only.trading.days=F)
ct=dc(secs, use.xts=F, merge.xts=T, only.trading.days=F)
dt=dc(secs, use.xts=T, merge.xts=T, only.trading.days=F)

secs <- c("AMZN US Equity", "MSFT US Equity")
aa=dc(secs, use.xts = F, merge.xts=F, only.trading.days=T) 
bb=dc(secs, use.xts = T, merge.xts=F, only.trading.days=T)
cc=dc(secs, use.xts = F, merge.xts=T, only.trading.days=T) 
dd=dc(secs, use.xts = T, merge.xts=T, only.trading.days=T) 

## only trading days=F
secs <- c("AMZN US Equity", "MSFT US Equity")
aat=dc(secs, use.xts = F, merge.xts=F, only.trading.days = F) 
bbt=dc(secs, use.xts = T, merge.xts=F, only.trading.days = F)
cct=dc(secs, use.xts = F, merge.xts=T, only.trading.days = F) 
ddt=dc(secs, use.xts = T, merge.xts=T, only.trading.days = F) 

## Fund liquidated in 2013: AGORASM RU
secs <- c("AGORASM RU Equity")
la=dc(secs, use.xts = F, merge.xts=F, only.trading.days = T)
lb=dc(secs, use.xts = T, merge.xts=F, only.trading.days = T)
lc=dc(secs, use.xts = F, merge.xts=T, only.trading.days = T)
ld=dc(secs, use.xts = T, merge.xts=T, only.trading.days = T)

## Fund liquidated in 2013: AGORASM RU, only trading days=F
secs <- c("AGORASM RU Equity")
lat=dc(secs, use.xts = F, merge.xts=F, only.trading.days = F)
lbt=dc(secs, use.xts = T, merge.xts=F, only.trading.days = F)
lct=dc(secs, use.xts = F, merge.xts=T, only.trading.days = F)
ldt=dc(secs, use.xts = T, merge.xts=T, only.trading.days = F)

## Fund liquidated in 2013: AGORASM RU and active
secs <- c("MSFT US Equity", "AGORASM RU Equity")
laa=dc(secs, use.xts = F, merge.xts=F, only.trading.days = T)
lbb=dc(secs, use.xts = T, merge.xts=F, only.trading.days = T)
lcc=dc(secs, use.xts = F, merge.xts=T, only.trading.days = T)
ldd=dc(secs, use.xts = T, merge.xts=T, only.trading.days = T) 

## Fund liquidated in 2013: AGORASM RU and active, only trading days=F
secs <- c("MSFT US Equity", "AGORASM RU Equity")
laat=dc(secs, use.xts = F, merge.xts=F, only.trading.days = F)
lbbt=dc(secs, use.xts = T, merge.xts=F, only.trading.days = F)
lcct=dc(secs, use.xts = F, merge.xts=T, only.trading.days = F)
lddt=dc(secs, use.xts = T, merge.xts=T, only.trading.days = F) 
leet=dc(secs, only.trading.days = FALSE)


dsc=br.desc(CONN, "MSFT US Equity")
dscf=br.desc(CONN, "AGORASM RU Equity")

save(a,b,c,d,
     at,bt,ct,dt,
     aa,bb,cc,dd,
     aat,bbt,cct,ddt,
     la,lb,lc,ld,
     laa,lbb,lcc,ldd,
     lat,lbt,lct,ldt,
     laat,lbbt,lcct,lddt,leet, 
     dsc, dscf, 
     file="testdata.high.Rdata")
}

main()

