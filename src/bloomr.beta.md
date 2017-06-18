


# BloomR main functions

## TODO
Complete help for functions br.try.date, br.is.same.class, br.bdh
When in br.desc 'CIE_DES_BULK' is not availbale use NA done set it in log
allow br.desc/br.bulk.desc to use simulated
Find a better place for  br.beta(): here it does not make sense. In bloomr.Rmd prevents a replacement with the beta when it is ready for shipping. 
Use safer way to get internal variables `get(".br.types", pos = "bloomr")`
Fix XXXX paragraphs
For final tests (when beta is release candidate), do not use source('bloomr.R')

### Terminal tests  
always.display.tickers, dates.as.row.names 
only.trading.days









br.bdh{#br.bdh}
===============
*Historical data*

XXXXRemove always.display.ticker, dates.as.row.names?
XXXXCheck only.trading.days description
XXXXMove staff in auto args

Description
-----------
Download historical Bloomberg data 

Usage
-----
    br.bdh(con, security, fields="PX_LAST", start.date=Sys.Date() - 7, end.date=Sys.Date(), 
    override_fields= NULL, override_values=NULL, 
    option.names = NULL, option.values = NULL,
    only.trading.days = TRUE
    )     

Arguments
---------
con
:   the connection token returned from br.open()

securities
:   character vector of the tickers queried for data  

field
:   case insensitive character vector of the Bloomberg field queried. Defaults to "PX_LAST". 

start.date
:   Time series start date as a Date object or an ISO string without separators (YYYYMMDD). Time series will actually begin at `start.date` if there is data available; otherwise it will start at the first significant date. 

end.date
:   Time series end  date as a Date object or an ISO string without separators (YYYYMMDD). If NULL or missing, it defaults to the last available date. 

option.names, option.values
:   See details

always.display.tickers 
:   (removed) Displays tickers in first column even if a single one is requested. Defaults to FALSE

dates.as.row.names  
:   (removed) Displays dates _also_ as row names. Defaults to TRUE for single ticker query, FALSE otherwise. 

only.trading.days
:   If FALSE, rows are returned for all requested dates, even when no markets data is available.  
XXXXCheck! It defaults to FALSE for a single security or and TRUE for multiple securities. In the latter case, use `na.omit`  or `na.exclude` to remove these rows.


Details
-------
For multi-ticker queries you might consider to use `br.hist()` which features also a simulated mode.

`option.names` and `option.values` are options vectors affecting the returned data. Options are set pairwise, for example to set `opt1` and `opt2`  respectively to `val1` and `val2`, you would pass the arguments:

    option.names=c("opt1", "opt2"), option.values=c("val1", "val2")

Here is a list of options:



periodicityAdjustment
:   Determine the frequency and calendar type of the output. To be used in conjunction with `periodicitySelection`. If `ACTUAL`, it  reverts to the actual date from today (if the end date is left blank) 
or from the End Date. If `CALENDAR`, (for pricing fields), it  reverts to the last business day of the specified calendar period. Calendar Quarterly (CQ), Calendar Semi-Annually (CS), or Calendar Yearly (CY). If `FISCAL`, it reverts to the fiscal period end for the company - Fiscal Quarterly (FQ), Fiscal Semi-Annually (FS) and Fiscal Yearly (FY) only.

periodicitySelection
:   Determine the frequency of the output. To be used in conjunction with periodicityAdjustment.
if `DAILY`, `WEEKLY`, `MONTHLY`, `QUARTERLY`, `SEMI_ANNUALLY`, `YEARLY`.

currency
:   Amends the value from local to desired currency. The value is a 3 letter ISO code string, e.g. USD, GBP. View `WCV<GO>` on the Bloomberg terminal for the full list.

overrideOption
:   Indicates whether to use the average or the closing price in quote calculation. Values can be 
`OVERRIDE_OPTION_CLOSE` for using the closing price or `OVERRIDE_OPTION_GPA`  for the average price.

pricingOption
:   Sets quote to Price or Yield for a debt instrument whose default value is quoted in yield (depending on pricing source).
`PRICING_OPTION_PRICE` sets quote to price; `PRICING_OPTION_YIELD` sets quote to yield.

nonTradingDayFillOption 
:   Sets to include/exclude non trading days where no data was generated.
`NON_TRADING_WEEKDAYS` includes all weekdays (Monday to Friday); `ALL_CALENDAR_DAYS`  includes all days of the calendar; `ACTIVE_DAYS_ONLY`  includes only the days where the instrument and field pair 
were updated.


nonTradingDayFillMethod
:   If data is to be displayed for non trading days what is the data to be returned.
 `PREVIOUS_VALUE` searches back and retrieve the previous value available for this security field pair. The search back period is up to one month. `NIL_VALUE` returns blank for the "value" value 
within the data element for this field.

maxDataPoints
:   the maximum number of data points to return. If the original data set is larger, the response will be a subset, containing only the last `maxDataPoints` data points.

returnEids
:   returns the entitlement identifiers associated with security. If TRUE, populates data with an extra element containing a name and value for the EID date.

returnRelativeDate
:   returns data with a relative date. If TRUE, populates data with an extra element containing a name and value for the relative date. For example RELATIVE_DATE = 2002 Q2

adjustmentNormal
:   Adjust for "change on day". If TRUE, adjusts historical pricing to reflect: Regular Cash, Interim, 1st Interim, 2nd Interim, 3rd Interim, 4th Interim, 5th Interim, Income, Estimated, Partnership Distribution, Final, Interest on Capital, Distribution, Prorated.

adjustmentAbnormal 
:   Adjusts for Anormal Cash Dividends. If TRUE, adjusts historical pricing to reflect: Special Cash, Liquidation, Capital Gains, Long-Term Capital Gains, Short-Term Capital Gains, Memorial, Return of Capital, Rights Redemption, Miscellaneous, Return Premium, Preferred Rights Redemption, Proceeds/Rights, Proceeds/Shares, Proceeds/Warrants.


adjustmentSplit
:   Capital Changes Defaults. If TRUE, adjusts historical pricing and/or volume to reflect: Spin-Offs, Stock Splits/Consolidations, Stock Dividend/Bonus, Rights Offerings/Entitlement.

adjustmentFollowDPDF
:   If TRUE (defaults) Follow the Bloomberg function as from `DPDF<GO>`.

CalendarCodeOverride
:   Returns the data based on the calendar of the specified country, exchange, or 
religion. Value is  a two character calendar code as from `CDR<GO>`. This will cause the data to be aligned according to the calendar  and including calendar holidays. Only applies only to DAILY requests.

calendarOverridesInfo
:   (Experimental, not tested) Returns data based on the calendar code of multiple countries, exchanges, or religious calendars as from `CDR<GO>`. This will cause the data to be aligned according to the set calendar(s) including their calendar holidays and only applies to DAILY requests.
Requires `calendarOverrides`, which is a character vector of  two-character calendar codes as from `CDR<GO>`; `calendareOverridesOperation`, which can be  `CDR_AND`  returning  the intersection of trading days among multiple calendars or  `CDR_OR` returning the union of trading days. That is, a data point is returned if a date is a valid trading day for any of the calendar codes specified in the request.

Overrides
:   (Experimental, not tested) Append overrides to modify the calculation. `fieldID` specifies  a field mnemonic or alpha-numeric, such as `PR092` or `PRICING_SOURCE`. Review FLDS<GO> for list of possible overrides. `value` sets the desired override value



Value
-----
A data frame with historical data. If tickers are displayed, the first column shows tickers, the second one the time series dates and the following ones the values of the queried fields; otherwise the columns start with dates. Dates will also be shown as rows if `dates.as.row.names=TRUE`. If multiple tickers are queried, they are vertically stacked respecting the order in `securities` vector.




br.hist.csv{#br.hist.csv}
========================
*Historical data from grouped tickers in a CSV file*  
Reads a CSV file containing a group of tickers in each column and returns the historical data in xts or list format. The CSV file is assumed to have headers denoting group labels. 
It replaces `br.bulk.csv``

Usage
------
    br.hist.csv(con, file, field="PX_LAST", start=Sys.Date()-5, end.date=Sys.Date(),

                     cols=NULL, comma=TRUE,
                     addtype=FALSE, showtype=FALSE,  
                     use.xts=TRUE, merge.xts=TRUE,

                     option.names = NULL, option.values = NULL,
                     only.trading.days = TRUE,

                     price=TRUE,
                     mean=ifelse(price, 10, 0.1), sd=1, jitter=0,
                     same.dates=FALSE, empty.sec=0,
                     weekend=TRUE, holidays=NULL)


  
Arguments
----------

con
:   the connection token returned from br.open(). If `NULL` simulated values are generated.   

file
:   path to CSV file.

field
:   case insensitive string denoting the Bloomberg field queried. Defaults to "PX_LAST". If the field is wrong or not accessible, data will be empty, but no error will be raised.

start
:   start date. Can be a Date object or an ISO string without separators (YYYYMMDD). Defaults to 5 days before current date.  

end.date
:   end date. Same format as `start`. Defaults to current date.  

cols
:   Logical or integer vector for selecting CSV columns (ticker groups). Defaults to all columns.  

comma
:   to be set to FALSE for (non-English) CSV, using semicolon as separator.  

addtype
:   If a string, it denotes the security type and is added to all tickers; if TRUE "Equity", will be added; if FALSE (the default), nothing will be added.  

showtype
:   if TRUE, security types will be removed from names of list or xts output. It defaults to FALSE.  

use.xts
:   if TRUE (the default) time series are formatted as xts objects. else as a data frame.  

merge.xts
:   if TRUE (the default) xts objects in the same group are merged using all rows and using NAs for missing observations.

option.names 
:   list of Bloomberg options names. Require `option.values` too.

option.values 
:   list of Bloomberg options values related to `option.names`.

only.trading.days 
:   if TRUE (the default) only trading days are used, else non-trading days are added as NA values. 

price, mean, sd, jitter, same.dates, empty.sec, weekend, holidays
:   arguments passed to `br.sample()` if `con=NULL`.

Details
-------
Empty CSV cells or cells interpreted as NAs will be ignored.  
If `con=NULL`, values are simulated by means of `br.sample()`. This function is used with default values, except for `start, end.date, price, mean, sd, jitter, same.dates, empty.sec, weekend, holidays`, which can be explicitly passed as arguments, and `sec.names` depending on tickers found in the CSV file. These arguments are ignored if `con!=NULL`. See `br.sample()` help for more.

Value
-----
a list where each element is the historical data of a CSV group.  
If `use.xts=TRUE` and `merge.xts=FAlSE`, each group is a sub-list, whose elements are the the securoty time series as an xts object.
If `use.xts=TRUE` and `merge.xts=TRUE`, each group is the merged xts object, obtained merging historical data of all securities of that group.
If `use.xts=FALSE`, each group is a sub-list, where each element is the historical data of a security as a data frame.  
If there is only one group, the first (and unique) element of the list will be returned (XXXXto check).  




Demonstration
-------------


A sample CSV with Bloomberg tickers will look like follows:



```r
read.csv("mybloomr/tickers.csv")
## This file is part of BloomR and anyway available here:
## https://github.com/AntonioFasano/BloomR/blob/master/res/tickers.csv
```


```
##          Financial     Technology      Indices
## 1   3988 HK Equity QCOM US Equity    DJI Index
## 2      C US Equity CSCO US Equity DJUSFN Index
## 3 601288 CH Equity  700 HK Equity  W1TEC Index
## 4    BAC US Equity  IBM US Equity             
## 5   HSBA LN Equity INTC US Equity
```


Note: 

* CSV group headers are mandatory;
* Group headers need not to be the same length. 

We can now download data:


```r
con=NULL  #Simulated mode: replace with con=br.open() on terminal
```


```r
data=br.hist.csv(con, "mybloomr/tickers.csv") 
```


```
## Processing Financial ...
```

```
## Error in br.hist(con = con, tiks = csv[[g]], field = field, start = start, : Invalid connection parameter
```


Above you see some info about data being processed that we will not show anymore in the following.

If you want to have detailed ticker descriptions,  see [br.bulk.desc Example](#br.bulk.desc.exem). Downloaded data look like follows:


```r
data
```

```
## function (..., list = character(), package = NULL, lib.loc = NULL, 
##     verbose = getOption("verbose"), envir = .GlobalEnv) 
## {
##     fileExt <- function(x) {
##         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
##         ans <- sub(".*\\.", "", x)
##         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
##             x[db])
##         ans
##     }
##     names <- c(as.character(substitute(list(...))[-1L]), list)
##     if (!is.null(package)) {
##         if (!is.character(package)) 
##             stop("'package' must be a character string or NULL")
##         if (any(package %in% "base")) 
##             warning("datasets have been moved from package 'base' to package 'datasets'")
##         if (any(package %in% "stats")) 
##             warning("datasets have been moved from package 'stats' to package 'datasets'")
##         package[package %in% c("base", "stats")] <- "datasets"
##     }
##     paths <- find.package(package, lib.loc, verbose = verbose)
##     if (is.null(lib.loc)) 
##         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
##             paths)
##     paths <- unique(normalizePath(paths[file.exists(paths)]))
##     paths <- paths[dir.exists(file.path(paths, "data"))]
##     dataExts <- tools:::.make_file_exts("data")
##     if (length(names) == 0L) {
##         db <- matrix(character(), nrow = 0L, ncol = 4L)
##         for (path in paths) {
##             entries <- NULL
##             packageName <- if (file_test("-f", file.path(path, 
##                 "DESCRIPTION"))) 
##                 basename(path)
##             else "."
##             if (file_test("-f", INDEX <- file.path(path, "Meta", 
##                 "data.rds"))) {
##                 entries <- readRDS(INDEX)
##             }
##             else {
##                 dataDir <- file.path(path, "data")
##                 entries <- tools::list_files_with_type(dataDir, 
##                   "data")
##                 if (length(entries)) {
##                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
##                   entries <- cbind(entries, "")
##                 }
##             }
##             if (NROW(entries)) {
##                 if (is.matrix(entries) && ncol(entries) == 2L) 
##                   db <- rbind(db, cbind(packageName, dirname(path), 
##                     entries))
##                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
##                   sQuote(packageName)), domain = NA, call. = FALSE)
##             }
##         }
##         colnames(db) <- c("Package", "LibPath", "Item", "Title")
##         footer <- if (missing(package)) 
##             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
##                 "\n", "to list the data sets in all *available* packages.")
##         else NULL
##         y <- list(title = "Data sets", header = NULL, results = db, 
##             footer = footer)
##         class(y) <- "packageIQR"
##         return(y)
##     }
##     paths <- file.path(paths, "data")
##     for (name in names) {
##         found <- FALSE
##         for (p in paths) {
##             if (file_test("-f", file.path(p, "Rdata.rds"))) {
##                 rds <- readRDS(file.path(p, "Rdata.rds"))
##                 if (name %in% names(rds)) {
##                   found <- TRUE
##                   if (verbose) 
##                     message(sprintf("name=%s:\t found in Rdata.rds", 
##                       name), domain = NA)
##                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
##                   thispkg <- sub("_.*$", "", thispkg)
##                   thispkg <- paste0("package:", thispkg)
##                   objs <- rds[[name]]
##                   lazyLoad(file.path(p, "Rdata"), envir = envir, 
##                     filter = function(x) x %in% objs)
##                   break
##                 }
##                 else if (verbose) 
##                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
##                     name, paste(names(rds), collapse = ",")), 
##                     domain = NA)
##             }
##             if (file_test("-f", file.path(p, "Rdata.zip"))) {
##                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
##                   ".\nThat is defunct, so please re-install the package.", 
##                   domain = NA)
##                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
##                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
##                 else {
##                   warning(gettextf("file 'filelist' is missing for directory %s", 
##                     sQuote(p)), domain = NA)
##                   next
##                 }
##             }
##             else {
##                 files <- list.files(p, full.names = TRUE)
##             }
##             files <- files[grep(name, files, fixed = TRUE)]
##             if (length(files) > 1L) {
##                 o <- match(fileExt(files), dataExts, nomatch = 100L)
##                 paths0 <- dirname(files)
##                 paths0 <- factor(paths0, levels = unique(paths0))
##                 files <- files[order(paths0, o)]
##             }
##             if (length(files)) {
##                 for (file in files) {
##                   if (verbose) 
##                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
##                       basename(file), "::\t", appendLF = FALSE, 
##                       domain = NA)
##                   ext <- fileExt(file)
##                   if (basename(file) != paste0(name, ".", ext)) 
##                     found <- FALSE
##                   else {
##                     found <- TRUE
##                     zfile <- file
##                     zipname <- file.path(dirname(file), "Rdata.zip")
##                     if (file.exists(zipname)) {
##                       Rdatadir <- tempfile("Rdata")
##                       dir.create(Rdatadir, showWarnings = FALSE)
##                       topic <- basename(file)
##                       rc <- .External(C_unzip, zipname, topic, 
##                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
##                       if (rc == 0L) 
##                         zfile <- file.path(Rdatadir, topic)
##                     }
##                     if (zfile != file) 
##                       on.exit(unlink(zfile))
##                     switch(ext, R = , r = {
##                       library("utils")
##                       sys.source(zfile, chdir = TRUE, envir = envir)
##                     }, RData = , rdata = , rda = load(zfile, 
##                       envir = envir), TXT = , txt = , tab = , 
##                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
##                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
##                         header = TRUE, as.is = FALSE), envir = envir), 
##                       CSV = , csv = , csv.gz = , csv.bz2 = , 
##                       csv.xz = assign(name, read.table(zfile, 
##                         header = TRUE, sep = ";", as.is = FALSE), 
##                         envir = envir), found <- FALSE)
##                   }
##                   if (found) 
##                     break
##                 }
##                 if (verbose) 
##                   message(if (!found) 
##                     "*NOT* ", "found", domain = NA)
##             }
##             if (found) 
##                 break
##         }
##         if (!found) 
##             warning(gettextf("data set %s not found", sQuote(name)), 
##                 domain = NA)
##     }
##     invisible(names)
## }
## <bytecode: 0x000000001965a048>
## <environment: namespace:utils>
```

Note:

* The name of the securities tickers is stored without the security type: "Equity", "Index", etc.  
If this piece of info is significant for you, pass `showtype = TRUE`.   

* Time series start date defaults to 5 days before current date, unless you set `start` to: 
an R Date object (`start=as.Date("2014/9/30")`) or to a  more friendly ISO string (`start="20140930")`).     

Data are stored as a list of xts objects, each representing one group of tickers in the CSV file.



```r
length(data)
```

```
## [1] 1
```

```r
names(data)
```

```
## NULL
```

```r
class(data$Financial)
```

```
## Error in data$Financial: object of type 'closure' is not subsettable
```

If you prefer you may get time series as data frames, and precisely as a list representing the ticker groups, where each group is in turn a list containing a data frame for each security:


































































