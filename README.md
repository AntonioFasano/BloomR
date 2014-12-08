BloomR
======

*Portable R for Bloomberg*


What's new
----------

### BloomR-0.3-dev

- Only 64 bit
- Updated to R-3.1.2
- New functions for bulk download from CSV files tickers, from index constituents or from strings.
- Simulated download to prepare script at home and test on Bloomberg terminal later.
- Bulk and non-bulk download of many description fields.
- `bbg.*` suffix is deprecated now use `br.*`
- A formal documentation
- Demo and examples.


Manual
------

TODO
Find in your BloomR directory the subdirectory `help` intended for learning and reference material.  
As this is development version the help resources are not yet finalised!

Build instructions
------------------

### For the impatient

Download [bloomr.build.R](https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R) (make sure your browser does not change its extension to ".html" or ".txt") and under R for Windows issue:

	source("path\to\bloomr.build.R")
	makeBloomR("path\to\workDir")


You will find your portable BloomR directory  inside your `workDir`.

### More options and requirements

A decent R version is needed 3+ (perhaps older version will do).  
`.Platform$OS.type == "windows"`. `XML` and `Rcurl` packages are needed, but if missing, the build script will try to download and install them.  
R should be able to connect to the Internet.

If you want to find  `BloomR.zip` too in your work directory use:

	makeBloomR("path\to\workDir", zip = TRUE)

<!--  LocalWords:  BloomR Bloomberg CSV
 -->
