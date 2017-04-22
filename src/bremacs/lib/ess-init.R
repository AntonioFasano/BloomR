
untrace(install.packages)
trace(install.packages, 
    tracer=quote({wasloaded='package:tcltk' %in% search(); require(tcltk)}) ,
    exit=quote(if(!wasloaded) detach('package:tcltk', unload=TRUE)),
    print=FALSE
)

message ("")



