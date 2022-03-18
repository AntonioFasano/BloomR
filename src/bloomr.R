## ----store, opts.label='purlme'----------------------------------------------------------------------------------------------------
## Purl this first
## Store br.* objects in bloomr env in base namespace
assign('bloomr.usr', new.env(parent=asNamespace("stats")))

## func: store(func);  var: store("var")
store=function(sym){
    if(is.function(sym)){
        name=deparse(substitute(sym)) 
        val=sym
    } else {
        name=sym
        val=get(sym)
    }
    
    assign(name, val, envir=bloomr.usr)
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

## ----rmd-internal, opts.label='purlme'---------------------------------------------------------------------------------------------
.br.addpaths <- function(pandonly = FALSE, quiet=TRUE){
### Add to Windows System Path the executable directories of LaTeX, Pandoc, and Perl with this search priority, and
### return invisibly the original path. If "pandonly" is true, add only Pandoc. If "quiet" is false, print the new path.

    ## Exit in case this snippet is not exectuted in Windows
    if(.Platform$OS.type != "windows")
        stop("Sorry, Bloomberg Terminal only exists for Windows and so BloomR functions.")
        
  ## Find executables
  panexe=.br.home("pandoc/bin/pandoc.exe")
  if(!file.exists(panexe))
    stop(paste("Unable to find:", panexe, '\nYour BloomR edition might not support it.'))
  pandir=normalizePath(dirname(panexe))
    
  latbin=.br.home("latex/texmfs/install/miktex/bin/x64/latex.exe")
  if(!file.exists(latbin) && !pandonly){
    stop(paste("Unable to find:", latbin, '\nYour BloomR edition might not support it.'))
  }
  latdir=normalizePath(dirname(latbin))

  perlexe <- .br.home("perl/bin/perl.exe")
  if(!file.exists(perlexe) && !pandonly){
    stop(paste("Unable to find:", perlexe, '\nYour BloomR edition might not support it.'))
  }
  perldir <- normalizePath(dirname(perlexe))
    
  ## Add executable dirs to system path
  old.path=Sys.getenv('Path')
  f <- function(dir) Sys.setenv(PATH=paste0(dir, ";", Sys.getenv("PATH")))

  ## Wanted final order is: LaTeX;Pandoc;Perl 
  if(!pandonly) f(perldir)
  f(pandir)
  if(!pandonly) f(latdir)
  
  ## Add Ghostscript environment variable   
  if(!pandonly)  Sys.setenv(GSC="mgs.exe")   

  if(!quiet) message(Sys.getenv("PATH"))

  invisible(old.path)
  
}



## ----br.md2pdf, opts.label='purlme'------------------------------------------------------------------------------------------------
br.md2pdf=function(md.file, pdf.file, quiet=TRUE){
### Make a markdown file into a PDF
### You need the proper BloomR version

    ## Test arguments
    if(missing(md.file)) stop("Argument 'md.file' missing.")
    if(missing(pdf.file)) pdf.file=paste0(tools:::file_path_sans_ext(md.file), '.pdf')
    
    ## Set executable paths and render
    old.path <- .br.addpaths(quiet=quiet)
    cmd <- paste("pandoc", .br.wpath(md.file), '-o', .br.wpath(pdf.file))
    tryCatch(
        out  <-  system(cmd, intern = TRUE, invisible = FALSE),
        finally = {
            ## Restore original system Path
            Sys.setenv(Path=old.path)
            ## Return errors if any
            if(!is.null(attr(out, 'status')))  message(paste(out, collapse="\n"))
        })  

    invisible(out)
}
store(br.md2pdf)

## ----br.rmd2html, opts.label='purlme'----------------------------------------------------------------------------------------------
br.rmd2html=function(rmd.file, html.file, quiet=TRUE){
### Make an R Markdown file into a HTML self-contained file
### You need the proper BloomR edition

    
    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(html.file)) html.file=paste0(tools:::file_path_sans_ext(rmd.file), '.html')

    library(knitr)
    library(rmarkdown)

    ## Set executable paths and render
    old.path <- .br.addpaths(pandonly = TRUE, quiet = TRUE)  
    tryCatch(
        ## Render Rmd to HTML
        out <- render(rmd.file,
                      output_format=html_document(theme="cerulean", highlight="tango",
                                                  md_extensions="-tex_math_single_backslash"),
                      output_file=html.file,
                      clean=quiet, quiet=quiet),
        finally = {
            ## Restore original system Path
            Sys.setenv(Path=old.path)
        })

    invisible(out)
}
store(br.rmd2html)

## ----br.rmd2slides, opts.label='purlme'--------------------------------------------------------------------------------------------
br.rmd2slides <- function(rmd.file, html.file, quiet=TRUE){
### Make an R Markdown file into a Google Slides self-contained HTML file
### You need proper BloomR edition

    
    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(html.file)) html.file=paste0(tools:::file_path_sans_ext(rmd.file), '.html')

    library(knitr)
    library(rmarkdown)

    ## Set executable paths and render
    old.path <- .br.addpaths(pandonly = TRUE, quiet = TRUE)  
    tryCatch(
        ## Render Rmd to HTML
        out <- render(rmd.file,
                      output_format = ioslides_presentation(
                          md_extensions="-tex_math_single_backslash"
                                                  ),
                      output_file=html.file,
                      clean=quiet, quiet=quiet),
        finally = {
            ## Restore original system Path
            Sys.setenv(Path=old.path)
        })

    invisible(out)
}
store(br.rmd2slides)    
    

## ----br.rmd2pdf, opts.label='purlme'-----------------------------------------------------------------------------------------------
br.rmd2pdf=function(rmd.file, pdf.file, quiet=TRUE){
### Make an R Markdown file into a PDF
### You need BloomR LaTeX addons or the proper BloomR version
    
    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(pdf.file)) pdf.file=paste0(tools:::file_path_sans_ext(rmd.file), '.pdf')

    library(knitr)
    library(rmarkdown)
    
    ## Set executable paths and render
    old.path <- .br.addpaths(quiet = TRUE)    
    tryCatch(
        ## Render Rmd to PDF
        out <- render(rmd.file,
                      output_format=pdf_document(highlight="tango",
                                                  md_extensions="-tex_math_single_backslash"),
                      output_file=pdf.file,
                      clean=quiet, quiet=quiet),
        finally = {
            ## Restore original system Path
            Sys.setenv(Path=old.path)
        })
    invisible(out)            
}
store(br.rmd2pdf)

## ----br.rmd2both, opts.label='purlme'----------------------------------------------------------------------------------------------
br.rmd2both=function(rmd.file, out.dir, quiet=TRUE){
### Make an R Markdown file into a PDF and an HTML self-contained file
### You need  BloomR LaTeX addons or the proper BloomR version

    
    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    
    pdf.file=paste0(tools:::file_path_sans_ext(rmd.file), '.pdf')
    html.file=paste0(tools:::file_path_sans_ext(rmd.file), '.html')
    if(!missing(out.dir)){
        out.dir <- paste0(sub("/$", "", out.dir), "/") 
        pdf.file <- paste0(out.dir, basename(pdf.file))
        html.file <- paste0(out.dir, basename(html.file))
    }
    
    library(knitr)
    library(rmarkdown)

    ## Set executable paths
    old.path <- .br.addpaths(quiet = TRUE)
    
    tryCatch(
        {
            ## Render Rmd to HTML
            out <- render(rmd.file,
                          output_format=html_document(theme="cerulean", highlight="tango",
                                                     md_extensions="-tex_math_single_backslash"),
                          output_file=html.file,
                          clean=quiet, quiet=quiet)

            ## Render Rmd to PDF
            out <- render(rmd.file,
                          output_format=pdf_document(highlight="tango",
                                                     md_extensions="-tex_math_single_backslash"),
                          output_file=pdf.file,
                          clean=quiet, quiet=quiet)

        },
        finally = {
            ## Restore original system Path
            Sys.setenv(Path=old.path)
        })
    
    invisible(dirname(out))    
}
store(br.rmd2both)


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

## ----deprecated, opts.label='purlme'-----------------------------------------------------------------------------------------------
bbg.open=function() stop("Sorry 'bbg.open' is now deprecated. Please use br.open().")
bbg.close=function(con) stop("Sorry 'bbg.close' is now deprecated. Please use br.close().")
store(bbg.open)
store(bbg.close)

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

## ----miscfunc, opts.label='purlme'-------------------------------------------------------------------------------------------------

#Clean up
## Remove visible and invisible objects
rm.all=function() 
    rm(list=ls(all=TRUE, envir=parent.frame()), envir=parent.frame())

## Remove visible non-function objects
rm.var=function() 
    rm(list=setdiff(ls(envir=parent.frame()), lsf.str(envir=parent.frame())),  envir=parent.frame())

store(rm.all)
store(rm.var)

## ----betafun, opts.label='purlme'--------------------------------------------------------------------------------------------------

br.beta=function(){
    f=paste0(R.home("share"), "/bloomr/bloomr.beta.R")    
    if(file.exists(f)) source(f)  else message("No beta functionalities in this release")
}

store(br.beta)


## ----time, opts.label='purlme'-----------------------------------------------------------------------------------------------------
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


## ----attach, opts.label='purlme'---------------------------------------------------------------------------------------------------
### Make visible br.* in bloomr env and base ns
attach(bloomr.usr)
rm(store)

