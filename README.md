RJafroc-master
========

[![Build Status](https://travis-ci.org/dpc10ster/rjafroc-master.svg?branch=master)](https://travis-ci.org/dpc10ster/rjafroc-master)
[![codecov](https://codecov.io/gh/dpc10ster/rjafroc-master/branch/master/graph/badge.svg)](https://codecov.io/gh/dpc10ster/rjafroc-master)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjafroc)](https://cran.r-project.org/package=rjafroc)

### What is this repository for? ###

* Modeling, Analysis, Validation and Visualization of ROC/FROC studies
* Extends and replaces Windown version of software (JAFROC: https://www.devchakraborty.com)

### RJafroc code development history ###
See NEWS.md

### How do I get set up? ###

* Navigate to https://github.com/dpc10ster/rjafroc-master
* Clone this repository to Desktop.
* Install current versions of R and RStudio.
* Open RJafroc.Rproj.
* Find RJafroc in Packages and click on it - this will open the help files.
* Click on any function and run the example(s) by higlighting, copying and pasting
     into the Console window.
* For example, click on PlotBinormalFit, find the example, copy it and paste it
     into the Console window:
     binormalPlot <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
     print(binormalPlot)
* You should see two binormal model ROC curves, one with a decided hook.
* Study the RJafroc.pdf file (https://cran.r-project.org/web/packages/RJafroc/RJafroc.pdf)
* Dependencies
Imports:
    openxlsx,
    ggplot2,
    stringr,
    tools,
    utils,
    stats,
    bbmle,
    binom,
    mvtnorm,
    numDeriv,
    Rcpp
Suggests:
    testthat,
    knitr,
    rmarkdown
* Database configuration
  NA
* How to run tests
  TBA
* Deployment instructions
  TBA

### Contribution guidelines ###

* Writing tests
  TBA
* Code review
  TBA
* Other guidelines
  TBA

### Who do I talk to? ###

dpc10ster@gmail.com
https://www.devchakraborty.com
