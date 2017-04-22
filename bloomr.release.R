
###  BloomR automatic Github releases

##  TODO
## 
##  Usage:
##  Source this file and run:
##  makeBloomR("path/to/workDir")
##  You will get the BloomR green executable in workDir
##
##  Requirements:
##  Rcurl package. 
##  R should be able to connect to the Internet.
##  Tested only with .Platform$OS.type == "windows"
##
##


#### Globals

## some future global
G.key="value"




# read
# https://developer.github.com/v3/repos/releases/
# https://developer.github.com/v3/
# https://github.com/blog/1645-releases-api-preview            
# https://developer.github.com/changes/2013-09-25-releases-api/
    
#curl --data '{"tag_name": "v1.0.0","target_commitish": "master","name": "v1.0.0","body": "Release of version 1.0.0","draft": false,"prerelease": false}' https://httpbin.org/post




deployRel=function(
              code,      # Code dir, with relase notes and current version num
              work,      # Work dir as from bloomr.build.R-> makeBloomR()
              draft=TRUE # create a draft and do not replace "curver.txt"   
              ){


    ## Custom
    bloomr.exe.name="BloomR_green_setup_.exe"

    ## debug
    # bloomr.exe.name="app.exe"

    ## Test arguments
    if(!nzchar(code) || !nzchar(work)) 
        stop("Missing arguments. Please set:\n",
             "    code -> Code dir, with relase notes and current version num.\n",
             "    work -> Work dir as from bloomr.build.R-> makeBloomR().")
    if(!dir.exists(code)) stop("Missing directory:\n    ", code)
    if(!dir.exists(work)) stop("Missing directory:\n    ", work)
    
    ## Test/load RCurl
    if (!"RCurl" %in% rownames(installed.packages()))
        stop("Please, install RCurl package")  
    library("RCurl")

    ## Test curl executable 
    if(!nzchar(Sys.which("curl"))) stop("Can't find curl executable.")  
    
    ## Get release files and versions
    relnote.f=normalizePath(file.path(code, "latest.txt"), mustWork=FALSE)
    curver.f=normalizePath(file.path(code, "curver.txt"), mustWork=FALSE)
    bloomr.exe=normalizePath(file.path(work, bloomr.exe.name), mustWork=FALSE)

    x=lapply(c(relnote.f, curver.f, bloomr.exe), function(f)
        if(!file.exists(f)) stop("Missing file:\n    ", f))

    ## Manage versions
    curver=trimws(scan(curver.f, quiet=TRUE, what=character()))
    newver=trimws(scan(relnote.f, quiet=TRUE, what=character())[1])
    seemsVnum(curver, curver.f)
    seemsVnum(newver, relnote.f)    
    if(!draft) compVer(newver, relnote.f, curver, curver.f)
    bloomr.exe.name=
        paste0(tools:::file_path_sans_ext(bloomr.exe.name), "v", newver, ".exe")
    
    #bloomr.exe.name="BloomR_green_setup_.exe"
    bin.type="application/octet-stream"
    bin.type="application/x-dosexec"
    
    apiurl="https://api.github.com/repos/AntonioFasano/BloomR/releases"
    uplurl= "https://uploads.github.com/repos/AntonioFasano/BloomR/releases"
    
    body=paste(readLines(relnote.f), collapse="\n")
    body= sub(".+?\n", "",  body) # remove header with version 
    ## escape the escapded
    body=gsub("\\\\", "\\\\\\\\", body) # should be the first to avoid escaping the others
    body=gsub("\n", "\\\\n", body)
    body=gsub("\"", "\\\\\"", body)

    
    release=addRelTag("", tag_name=paste0('v', newver))
    release=addRelTag(release,
                   target_commitish="master",
                   name= "Turandot",
                   body= body,
                   draft= draft, prerelease= FALSE)

    
    auhdr=makeAuhead()
    auhdr=c(auhdr, `User-Agent`="RCurl")


    succ=tryCatch(
        postForm(apiurl,
                 .opts=list(httpheader=auhdr, postfields=release)),
        error = function(x) cat("Post Error:", x$message, '\n'))

    if(is.null(succ)) {
                message("Failed to deploy ", newver)
                return()
    }

    ## Get upload ID from succ response
    ## "upload_url": "https://uploads.github.com/repos/:owner/:repo/releases/:ID/assets{?name,label}",
    x=regexpr(paste0(uplurl, "/[[:digit:]]+"), succ)    
    idurl=regmatches(succ, x)
    idurl=paste0(idurl, "/assets?name=", bloomr.exe.name)

    ## Upload release

    ##   RCurl seems unable to upload to git   
    ##   auhdr=makeAuhead()
    ##   auhdr=c(auhdr, `User-Agent`="RCurl")
    ##   auhdr=c(auhdr, `Content-Type`=bin.type)
    ##   
    ##   url="https://uploads.github.com/repos/AntonioFasano/BloomR/releases/4708097/assets?name=bloomr.exe"
    ##   succ2=tryCatch(
    ##       postForm(idurl,
    ##                "fileData" = fileUpload(filename = bloomr.exe, contentType=bin.type),
    ##                .opts=list(httpheader=auhdr)),
    ##        error = function(x) cat("Post Error:", x$message, '\n'))

    curl.cmd=sprintf("curl -H \"Authorization: token %s\" -H \"Content-Type: %s\" --data-binary @%s %s",
                     getToken(), bin.type, bloomr.exe, idurl)                
    succ=executeCurl(curl.cmd)

    ## Parse resuld and get download url   
    downurl=rev(succ)[1]
    downurl=regmatches(downurl, regexpr("browser_download_url.+", downurl))
    downurl=regmatches(downurl, regexpr("https[^\"]+", downurl))
    
    succ = length(downurl)>0
    if(succ) {
        if(!draft) writeLines(newver, curver.f)
        message("Release ", newver, " successfully deployed to:\n", downurl)
        if(draft) message("Release uploaded as draft and local version file not updated.")
    } else {
        message("Failed to deploy ", newver)
    }   
    
}




###== (json) API releases  ==

makeAuhead=function()       # Make the autorization header for Post request
    c(Authorization=paste("token", getToken()))


getToken=function(                       # Return Git API access token
                  source="./gitToken.R"  # Path to the script releasing the token 
                  ){
### If the env variable GITTOKEN is not empty is use as token, else
### If the file "~/.ssh/git-api-token.txt" exists its first line is used, else return a blank.
    
    source(source)$value  -> tok
    if(!nzchar(tok))(stop("Git token not found")) else tok
}


# addReqHdr=function(      # Add headers to the header vector for post requests
#                         req,  # current headers  as a named character vector or an empty string
#                         ...   # new headers as a named character vector
#                         ){
#  
#     
#  
# }

addRelTag=function(      # Add name and value as json element to json release string
                        rel,  # current json release string or empty string
                        ...   # list of name=value arguments to be added to rel
                        ){

    ## Format names for json
    op=options(useFancyQuotes = FALSE)
    names=sapply(names(list(...)), dQuote)
    options(op)

    ## Format values for json
    values=sapply(list(...), addRelTag.formatVal)

    ## Get vector of json elements and turn into a string
    elts=sapply(seq_along(names), function(i) sprintf("%s: %s", names[i], values[i]))
    elts=paste(elts, collapse=",")        

    ## Add element vector to existing or empty json string 
    rel=ifelse(nzchar(rel), sub("}$", ",", rel), "{") # comma for non empty rel
    paste0(rel, elts, "}")        
}

addRelTag.formatVal=function(value) # Format value for json
### character  -> dquoted
### logical as character ans lower case
### numeric as character 

{
    op=options(useFancyQuotes = FALSE)
    value=switch(typeof(value),
           logical=tolower(as.character(value)),
           character=dQuote(value),
           double=as.character(value))
    options(op)
    value
}

seemsVnum=function(num, file){ # num in file seems a version number

    yes=grep("\\.", num) & grepl("^[0-9]+$",  gsub("\\.", "", num))
    if(!yes) stop("In:\n    ", file, "\n", num, " does not seem a version number")
}
    
compVer=function(newv, newv.f, oldv, oldv.f) { # Make sure new version is really new

    ##newv="1.2.3"
    ##oldv="1.2.2"
    newa=strsplit(newv, "\\.")[[1]] 
    olda=strsplit(oldv, "\\.")[[1]] 
    newa=newa[1:length(olda)]
    
    if(all(sapply(seq_along(newa), function(i)
        as.integer(newa[i]) >= as.integer(olda[i]))))
        stop("Version ", newv, " from ", newv.f,
             "\nis not newer than\n",
             "Version ", oldv, " from ", oldv.f)    
}


executeCurl=function(curl.cmd){ # As by the name

    shell <- Sys.getenv("R_SHELL")
    if (!nzchar(shell)) shell <- Sys.getenv("COMSPEC")
    flag=ifelse(nzchar(Sys.getenv("COMSPEC")), "/c","-c")
    cmd <- paste(shell, flag, curl.cmd)
  
    
    ##In RTerm shows output anyway, RGui TO BE TESTED should open a separate shell console and wait
    system.pars=list(wait=TRUE, show.output.on.console=FALSE, intern=TRUE,  invisible=FALSE)
    do.call(system,  c(cmd, system.pars))
      
###cmd="cmd.exe /k curl http://mirror.internode.on.net/pub/archlinux/iso/2017.02.01/archlinux-bootstrap-2017.02.01-i686.tar.gz --output buttacurl "

    

}
