BloomR
======

*Portable R for Bloomberg*


What's new
----------

### BloomR Turandot (1.0-dev)

Improvements over Début version:

- BloomR development is now open source on Github.com.
- Only 64 bit (it means less space).
- Updated to R-3.1.2.
- New functions for bulk download using tickers from CSV files, from index constituents or from ticker vectors.
- Simulated download allows to prepare script at home and test on Bloomberg terminal later.
- Bulk and non-bulk download of many description fields.
- More consistent naming scheme: `bbg.*` suffix is deprecated now, use `br.*`
- A formal documentation (see bloomr.html or [bloomr.pdf](https://github.com/AntonioFasano/BloomR/blob/master/bloomr.pdf?raw=true))
- Demo and examples.
- BloomR can be downloaded as a binary or generated from an R script. So you can stay always updated with the latest version of R (and related packages).


Plus read.xlx joins the family. With this function you can read Excel xlsx workbook sheets into R data frames.  Some features are:

* It can import all, one, or a selection of sheets, where specific sheets are requested by means of their name.
* Instead of importing all the sheets' cells, it can import only those comprised in a named range.
* It can distinguish between cells formatted as numbers, percent, text and dates,
* Date cells are recognised  whatever the language locale. 
* Blank (visual) lines are detected and automatically removed from the data frame, unless you want to keep them.
* The filter is not based on any external engine and does not requires Excel to be installed at all. It's pure R code, so you can read xlsx files on Linux systems. 

See xlx.help.html, or [xlx.help.pdf](https://github.com/AntonioFasano/BloomR/blob/master/xlx.help.pdf?raw=true) user manual for more.

The new version of the BloomR manual for lecturers (in slide format) is coming soon. 
 
 
Documentation
-------------

Find in your BloomR directory the subdirectory `help` intended for learning and reference material.  
As this is a development version the help resources are not yet finalised!

Start to get results in BloomR immediately 
----------------------------------------

1. Download [BloomR Turandot](https://www.sugarsync.com/pf/D9604848_697_6715279040").
2. Extract the zip file on your USB drive (or wherever).
3. Run "BloomR.exe" in the main folder. 
4. Log to Bloomberg service on the same PC (if you are not logged already).
5. To get some data, type in the BloomR console:

        con=br.open()                                            # Connect BloomR to Bloomberg
        br.bulk.tiks(con, c("MSFT US", "AMZN US"), addtype=TRUE) # Last 5 days for price of these tickers 
        br.bulk.idx(con, "SX5E Index", field="PX_LAST")          # Last 5 days for price of these indices
        br.close(con)                                            # Disconnect BloomR from Bloomberg



Build instructions
------------------

### For the impatient

Download [bloomr.build.R](https://raw.githubusercontent.com/AntonioFasano/BloomR/master/bloomr.build.R) (as a text file, *not as a HTML file*, and make sure your browser does not change ".R" extension to ".html" or ".txt"). In the console of R for Windows issue:

	source("path\to\bloomr.build.R")
	makeBloomR("path\to\workDir")


You will find your portable BloomR directory  inside `workDir`.

**Browser oddities**

Downloading a file _as is_  from you browser can be tangled. Anyway, choosing Text or All-Files in the save-as option from the browser Save dialog should do the trick. Then, double quoting the file name should give the proper extension, without extra suffix. Anyway, in as far as the format is text (not HTML), even if you get a distorted name, like "bloomr_build_R.txt", you can always adjust the `source()`-path accordingly.   
Yes, bowsers are strange sometimes. 


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
