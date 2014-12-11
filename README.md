BloomR
======

*Portable R for Bloomberg*


What's new
----------

### BloomR-1.0-dev

Improvements over debut version 0.1

- BloomR development is now open source on Github.com.
- Only 64 bit (it means less space)
- Updated to R-3.1.2
- New functions for bulk download using tickers from CSV files, from index constituents or from ticker (character) vectors.
- Simulated download to prepare script at home and test on Bloomberg terminal later.
- Bulk and non-bulk download of many description fields.
- `bbg.*` suffix is deprecated now use `br.*`
- A formal documentation (see bloomr.html, or bloomr.pdf)
- Demo and examples.
- BloomR can be downloaded as a binary or generated from an R script. So you can stay yourself updated with the latest version of R (and related packages).



Plus read.xlx joins the family. With this function you can  can read Excel xlsx workbook sheets into R data frames.  Some features are:

* It can import all, one, or a selection of sheets, where specific sheets are requested by means of their name.
* Instead of importing all the sheets' cells, it can import only those comprised in a named range.
* It can distinguish between cells formatted as numbers, percent, text and dates,
* Date cells are recognised  whatever the language locale. 
* Blank (visual) lines are detected and automatically removed from the data frame, unless you want to keep them.  
* The filter is not based on any external engine and does not requires Excel to be installed at all. It's pure R code, so you can read xlsx files on Linux systems. 



See xlx.html, or xlx.pdf user manual for more.


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


    
<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->

<!--  LocalWords:  BloomR Bloomberg CSV
 -->
