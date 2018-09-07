
### Download and print last 31 days for SPX Index from Bloomberg
## Make sure this PC is connected to Bloomberg terminal 
con <- blpConnect()
data <- bdh("SPX Index", c("PX_LAST", "VOLUME"), start.date=Sys.Date()-31)
print(data)
blpDisconnect(con)


### Download and print 2015 monthly prices for Microsoft from Reuters Eikon
## Make sure this PC is connected to Eikon terminal 
data <- get_timeseries("MSFT.O", c("TIMESTAMP", "CLOSE"), "2015-01-01T00:00:00", "2016-01-01T00:00:00", "monthly")


### Generate a report from the Rmd file my-first-report.Rmd in the examples directory
### Run this only in BloomR Studio
br.rmd2both("~/examples/my-first-report.Rmd")

## See the HTML and PDF files generated in the examples directory
