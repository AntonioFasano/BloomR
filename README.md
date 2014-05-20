BloomR
======

*Portable R for Bloomberg*


What's new
----------

### BloomR-0.2.1 Beta

- Only 64 bit
- Updated to R-3.1.0
- New functions for bulk download from CSV files tickers, from index constituents or from strings.
- Sample download to prepare script at home and test on terminal later.
- Bulk and non-bulk download of many description fields.

Manual
------

The new manual is not yet ready, please use the old manual (inluded) plus new functions addendum XXXX

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


