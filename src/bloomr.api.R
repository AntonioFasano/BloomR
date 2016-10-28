

## Log object
dbr.log=function(logfile){

    ## Check log
    islog=!is.null(logfile)
    if(islog) {
        if(!dir.exists(dirname(logfile))) stop("Invalid path:\n", logfile) else {
            if(file.exists(logfile)) {
                ans <- readline(sprintf("%s \nalready exists, overwrite?. (Y/n) ", logfile))
                if(tolower(ans)!="y" && ans!="") {message('Cancelled'); return()}
            } else file.create(logfile) 
        }
    }
        
    add=function(...){ # Log ... to logfile if not null
        if(is.null(logfile)) return()
        msg=paste('BR:', ...)
        cat(msg, sep='\n', append=TRUE, file=logfile)
        message(msg)
    }

    err=function(...){ # Stop and log ... to logfile if not null
        msg=paste('BR error:', ...)
        if(!is.null(logfile)) cat(msg, sep='\n', append=TRUE, file=logfile)
        stop(msg)
    }

    list(add=add, err=err)
    
}
    

dbr.jar=function(){ # Get bbg jar path 
    jarpath=dbr.brmain("/blpapi/bin")
    Sys.glob(file.path(jarpath,  "blpapi-[0-9]*.jar"))
}


## Get bbg connection token     
dbr.getToken=function(logfile=NULL, host=NULL,  port=NULL){
        
    ## Init JVM
    ret=.jinit()
    log=dbr.log(logfile)
    if (ret == 0) log$add("BloomR JVM initialized successfully.") else 
        log$err("Error in initialising BloomR JVM. Error code:", ret)    

    ## blpapi.jar to classpath
    ret=dbr.jar()
    .jaddClassPath(ret)
    log$add(ret, "added to Java classpath") 

    ## Set log level 
    java.logging.levels = J("java/util/logging/Level")
    log.level = 'finest'  # to be set by user  in the future
    
    java.log.level = switch(log.level,
        finest = java.logging.levels$FINEST, 
        fine = java.logging.levels$FINE, info = java.logging.levels$INFO, 
        warning = java.logging.levels$WARNING, off = java.logging.levels$OFF, 
        stop(paste("log level ", log.level, "not recognized")))
    
    if (is.null(host) || is.null(port)) {


        # todo con <- .jnew( 
    } else {

                # todo con <- .jnew( 

    }


##  to be continued
    
}


dbr.brmain=function(dir=""){
### Detect the path of the BloomR main directory and returns "path/to/bloomr-main/dir",
### where "path/to/bloomr-main" is BloomR main full path and "dir" is the argument and might not exist.
### BloomR main is intedented to host a subfodler for each BloomR app, e.g. R, Sumatra, BRemacs.
### Therefore it is the parent of R.home(). It does not include the user's personal folder.
### NOTE: currently BloomR main is R.home()!!!

    file.path(dirname(R.home()), dir) # will be this in the near future
    ##file.path(R.home(), dir)
    
}

    

    
