## ----store, opts.label='purlme'----------------------------------------------------------------------------------------------------
## Purl this first
## Store br.* objects in dedicated namespace
bloomr.rmd <- new.env(parent=asNamespace("stats"))

## func: store(func);  var: store("var")
store=function(sym){
    if(is.function(sym)){
        name=deparse(substitute(sym))
        val=sym
    } else {
        name=sym
        val=get(sym)
    }

    assign(name, val, envir=bloomr.rmd)
    rm(list=name, envir=parent.frame())
}


## ----rmd-internal, opts.label='purlme'---------------------------------------------------------------------------------------------
.br.addpaths <- function(pandonly = FALSE, quiet = TRUE){
### Add to Windows System Path the executable directories of LaTeX, Pandoc, and Perl with this search priority, and
### return invisibly the original path. If "pandonly" is true, add only Pandoc. If "quiet" is false, print the new path.

    ## Exit in case this snippet is not exectuted in Windows
    if(.Platform$OS.type != "windows")
        stop("Sorry, Bloomberg Terminal only exists for Windows and so BloomR functions.")

    ## Find executables
    xfind <- function(exepath){
        exe <- .br.home(exepath)
        if(!file.exists(exe))
            stop(paste("Unable to find:", exe, '\nYour BloomR edition might not support it.'))
        normalizePath(dirname(exe))
    }

    pandir <- xfind("pandoc/bin/pandoc.exe")
    if(!pandonly){
        latdir <- xfind("tinytex/bin/windows/latex.exe")
        perldir <- xfind("tinytex/tlpkg/tlperl/bin/perl.exe")
        .br.inittex()
    }

    ## Add executable dirs to system path
    old.path <- Sys.getenv('Path')
    ap <- function(dir) Sys.setenv(PATH=paste0(dir, ";", Sys.getenv("PATH")))

    ## Wanted final order is: LaTeX;Pandoc;Perl
    if(!pandonly) ap(perldir)
    ap(pandir)
    if(!pandonly) ap(latdir)

    ## Add Ghostscript environment variable
    ## if(!pandonly)  Sys.setenv(GSC="mgs.exe") # only miktex?

    if(!quiet) message(Sys.getenv("PATH"))

    invisible(old.path)

}
store(.br.addpaths)

.br.pathexe <- function(cmdexpr, pandonly = FALSE, quiet = TRUE){
### Change system path with .br.addpaths(), evaluate the expression cmdexpr in parent env, restotre path.
### See .br.addpaths() for pandonly and quiet arguments

    old.path <- Sys.getenv("PATH")
    tryCatch(
        {
            .br.addpaths(quiet=quiet)
            eval(cmdexpr, parent.frame())
        },

        error = function(cond) {
            message(cond)
            return(NA)
        },

        warning = function(cond) {
            message(cond)
            return(NULL)
        },

        finally = {
            Sys.setenv(PATH = old.path)
        })
}
store(.br.pathexe)

.br.inittex <- function() {
    library('tinytex')
    options(tinytex.tlmgr.path = file.path(.br.home(), "tinytex/bin/windows/tlmgr.bat"))
}
store(.br.inittex)


## ----br.md2pdf, opts.label='purlme'------------------------------------------------------------------------------------------------
br.md2pdf <- function(md.file, pdf.file, quiet=TRUE){
### Make a markdown file into a PDF
### You need the proper BloomR version

    ## Test arguments
    if(missing(md.file)) stop("Argument 'md.file' missing.")
    if(missing(pdf.file)) pdf.file=paste0(tools:::file_path_sans_ext(md.file), '.pdf')

    ## Set executable paths and render
    cmd <- paste("pandoc", .br.wpath(md.file), '-o', .br.wpath(pdf.file))
    cmdexpr <- quote(system(cmd, intern = TRUE, invisible = FALSE))
    out <- .br.pathexe(cmdexpr, quiet = quiet)
    invisible(out)
}
store(br.md2pdf)

## ----br.rmd2html, opts.label='purlme'----------------------------------------------------------------------------------------------
br.rmd2html <- function(rmd.file, html.file, quiet=TRUE){
### Make an R Markdown file into a HTML self-contained file
### You need the proper BloomR edition

    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(html.file)) html.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.html')
    if(html.file != basename(html.file)) stop("Sorry, currently you can only specify a name without directory as an output doc.")

    library(knitr)
    #library(rmarkdown)

    ## Render Rmd to HTML
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format = rmarkdown::html_document(theme="cerulean", highlight="tango",
                                           md_extensions="-tex_math_single_backslash"),
               output_file = html.file,
               clean = quiet, quiet = quiet)
    )
    out <- .br.pathexe(cmdexpr, pandonly = TRUE, quiet = quiet)
    invisible(out)
}
store(br.rmd2html)

## ----br.rmd2slides, opts.label='purlme'--------------------------------------------------------------------------------------------
br.rmd2slides.html <- function(rmd.file, html.file, quiet=TRUE){
### Make an R Markdown file into a Google Slides self-contained HTML file
### You need proper BloomR edition

    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(html.file)) html.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.html')
    if(html.file != basename(html.file)) stop("Sorry, currently you can only specify a name without directory as an output doc.")

    library(knitr)
    #library(rmarkdown)

    ## Render Rmd to HTML
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format = rmarkdown::ioslides_presentation(
                   md_extensions="-tex_math_single_backslash"
               ),
               output_file=html.file,
               clean=quiet, quiet=quiet)
    )
    out <- .br.pathexe(cmdexpr, pandonly = TRUE, quiet = quiet)
    invisible(out)
}
store(br.rmd2slides.html)

br.rmd2slides.pdf <- function(rmd.file, pdf.file, theme = "AnnArbor", highlight = "tango", quiet = TRUE) {
### Make an R Markdown file into a Beamer PDF
### You need the proper BloomR edition

    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(pdf.file)) pdf.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.pdf')
    if(pdf.file != basename(pdf.file)) stop("Sorry, currently you can only specify a name without directory as an output doc.")

    library(knitr)
    #library(rmarkdown)

    ## Render Rmd to PDF
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format = rmarkdown::beamer_presentation(theme = theme, highlight = highlight,
                                          md_extensions="-tex_math_single_backslash",
                                          keep_tex = !quiet, keep_md = !quiet),
               output_file = pdf.file,
               clean = quiet, quiet = quiet)
    )

    out <- .br.pathexe(cmdexpr, quiet = quiet)
    invisible(out)

}
store(br.rmd2slides.pdf)

br.rmd2slides <- function(rmd.file,  theme = "AnnArbor", highlight = "tango", quiet = TRUE) {
### Make an R Markdown file into a Beamer PDF and and a Google Slides self-contained HTML file
### You need the proper BloomR edition

    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")

    html.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.html')
    pdf.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.pdf')

    br.rmd2slides.html(rmd.file, html.file, quiet = quiet)
    br.rmd2slides.pdf (rmd.file, pdf.file,  theme = theme, highlight = highlight, quiet = quiet)

}
store(br.rmd2slides)

highlight.styles <- function() {# List Pandoc syntax highlight styles

    panexe <- .br.home("pandoc/bin/pandoc.exe")
    if(!file.exists(panexe)) stop("This function is not available in this BloomR edition")
    system2(panexe, "--list-highlight-styles")
}
store(highlight.styles)



## ----br.rmd2pdf, opts.label='purlme'-----------------------------------------------------------------------------------------------
br.rmd2pdf <- function(rmd.file, pdf.file, quiet=TRUE){
### Make an R Markdown file into a PDF
### You need BloomR LaTeX addons or the proper BloomR version

    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")
    if(missing(pdf.file)) pdf.file <- paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.pdf')
    if(pdf.file != basename(pdf.file)) stop("Sorry, currently you can only specify a name without directory as an output doc.")

    library(knitr)
    #library(rmarkdown)

    ## Render Rmd to PDF
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format=rmarkdown::pdf_document(highlight="tango",
                                          md_extensions="-tex_math_single_backslash"),
               output_file = pdf.file,
               clean = quiet, quiet = quiet)
    )
    out <- .br.pathexe(cmdexpr, quiet = quiet)
    invisible(out)

}
store(br.rmd2pdf)

## ----br.rmd2both, opts.label='purlme'----------------------------------------------------------------------------------------------
br.rmd2both <- function(rmd.file, quiet=TRUE){
### Make an R Markdown file into a PDF and an HTML self-contained file
### You need  BloomR LaTeX addons or the proper BloomR version


    ## Test arguments
    if(missing(rmd.file)) stop("Argument 'rmd.file' missing.")

    pdf.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.pdf')
    html.file=paste0(tools:::file_path_sans_ext(basename(rmd.file)), '.html')

    ## render does not support directory, perhaps we can move docs at the end.
    ##if(!missing(out.dir)){
    ##    out.dir <- paste0(sub("/$", "", out.dir), "/")
    ##    pdf.file <- paste0(out.dir, basename(pdf.file))
    ##    html.file <- paste0(out.dir, basename(html.file))
    ##}

    library(knitr)
    #library(rmarkdown)

    ## Render Rmd to HTML
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format=rmarkdown::html_document(theme="cerulean", highlight="tango",
                                           md_extensions="-tex_math_single_backslash"),
               output_file = html.file,
               clean = quiet, quiet = quiet)
    )
    out <- .br.pathexe(cmdexpr, pandonly = TRUE, quiet = quiet)

    if(is.na(out)) stop("HTML generation failed.")

    ## Render Rmd to PDF
    cmdexpr <- quote(
        rmarkdown::render(rmd.file,
               output_format = rmarkdown::pdf_document(highlight="tango",
                                            md_extensions = "-tex_math_single_backslash"),
               output_file = pdf.file,
               clean = quiet, quiet = quiet)
    )

    out <- .br.pathexe(cmdexpr, quiet = quiet)
    invisible(out)
}
store(br.rmd2both)


## ----miscfunc, opts.label='purlme'-------------------------------------------------------------------------------------------------

#Clean up
## Remove visible and invisible objects
rm.all <- function()
    rm(list=ls(all=TRUE, envir=parent.frame()), envir=parent.frame())

## Remove visible non-function objects
rm.var <- function()
    rm(list=setdiff(ls(envir=parent.frame()), lsf.str(envir=parent.frame())),  envir=parent.frame())

store(rm.all)
store(rm.var)

## ----betafun, opts.label='purlme'--------------------------------------------------------------------------------------------------

br.beta <- function(){
    f=paste0(R.home("share"), "/bloomr/bloomr.beta.R")
    if(file.exists(f)) source(f)  else message("No beta functionalities in this release")
}

store(br.beta)


## ----attach, opts.label='purlme'---------------------------------------------------------------------------------------------------
### Make visible br.* in bloomr env and base ns
attach(bloomr.rmd)
rm(store)
rm(bloomr.rmd)


