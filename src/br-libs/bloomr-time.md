---
title: "BloomR Time Functions"
author: "Antonio Fasano"
date: "Jan 29, 2024"
---

R topics documented:
-----------
[Beta time functions](#beta.misc.functions)   
[Time extension functions](#time.functions)   









# BloomR time functions


Beta time functions{#beta.misc.functions}
=========================================

Description
------------

Miscellaneous functions dealing with dates. 


Usage
-----
    br.try.date(d)
    br.is.same.class(...)


Arguments
---------
d
:   a POSIXlt, POSIXct, Date, "%Y/%m/%d", or "%Y-%m-%d" vector

Details
-------

`br.try.date` converts a vector to a date vector if possible or return `NULL`. Any vector element should be POSIXlt, POSIXct, Date, "%Y/%m/%d", or "%Y-%m-%d"

`br.is.same.class` check if all supplied argumets have the same class. It is mostly intended to check if dates are homogeneous. 





Time extension functions{#time.functions}
=========================================

Description
------------
Functions to get, set dates.

Usage
-----
    day(d)
    month(d)
    year(d)
    day(d, n)
    month(d, n)
    year(d, n)
    day(d)=x
    month(d)=x
    year(d)=x
    d %+% n
    d %-% n
    last.day(d)
    day.us(d1, d2)

Arguments
---------
d, d1, d2
:   objects of class date  

x
:   an integer representing the day/month/year  

n
:   an integer representing the months to add/subtract


Details
-------
If `component` is `day`, `month` or `year`: `component(d)` returns the *component* of the date `d` as an integer; `component(d, n)` returns the date `d` with the *component* set to the integer `n`; `component(d)= n` sets to the *component* of the date `d` to the integer `n`.  
`%+%` and `%-%` add and subtract months to a date.  
`last.day` returns last day of the month as an integer. `day.us` calculates date differences with the US convention.  









 

    
<!-- Local Variables: -->
<!-- mode: rmd -->
<!-- End: -->

<!--  LocalWords:  BloomR
 -->
