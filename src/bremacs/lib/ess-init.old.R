
## Get original args without ending NULL
temp=rev(rev(deparse(args(install.packages)))[-1]) 
temp=paste(paste(temp, collapse="\n"),
           ## Add code to load tcltk
           "{",
           "    wasloaded= 'package:tcltk' %in% search()",
           "    require(tcltk)",
           ## Add orginal body without braces
           paste(rev(rev(deparse(body(install.packages))[-1])[-1]), collapse="\n"),
           ## Unload tcltk if it was not loaded before by user
           "    if(!wasloaded) detach('package:tcltk', unload=TRUE)",
           "}\n",
           sep="\n")
           # cat(temp) # see new patched function as text

## Eval patched function 
temp=eval(parse(text=temp))
                           # temp # see as expression

## Override original function
unlockBinding("install.packages", as.environment("package:utils"))
assign("install.packages", temp,  envir=as.environment("package:utils"))
unlockBinding("install.packages",  asNamespace("utils"))
assign("install.packages", temp,  envir=asNamespace("utils"))
#assignInNamespace("install.packages", temp, asNamespace("utils"))
rm(temp)

message ("")



