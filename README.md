BloomR
======

Portable R for Bloomberg

Build instructions
------------------

Download [bloomr.build.R](https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.r) and under R for Windows issue:

	source("path\to\bloomr.build.R")
	makeBloomR("path\to\workDir")


You will find your portable BloomR directory  inside your `workDir`.


Requirements
------------

XML and Rcurl packages. If missing, it will tray to download and install them.  
R should be able to connect to the Internet.  
`.Platform$OS.type == "windows"`


