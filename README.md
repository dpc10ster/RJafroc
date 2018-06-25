### What is this repository for? ###

* Modeling, Analysis, Validation and Visualization of ROC/FROC studies
* Extends and replaces Windown version of JAFROC, https://www.devchakraborty.com, 
   which is no longer unsupported

### RJafroc code development history ###
See NEWS.md

### How do I get set up? ###

* Navigate to https://github.com/dpc10ster/rjafroc
* Clone this repository to Desktop (click on Clone button).
* Install current versions of R and RStudio.
* Open RJafroc.Rproj.
* Find RJafroc in Packages and click on it - this will open the help files.
* Click on the 'show in a new window' icon, just below packages to get the help
     files in a separate window that you can resize or print.
* Click on any function and run the example(s) by higlightin, copying and pasting
     into the Console window, followed by <enter>.
* For example, click on PlotBinormalFit, and find the example, copy it and paste it
     into the Console window:
     binormalPlot <- PlotBinormalFit(c(1, 2), c(0.5, 0.5))
     print(binormalPlot)
* You should see two binormal model ROC curves, one with a decided hook.
* Study the RJafroc.pdf file (this can be downloaded from CRAN)
* Dependencies
    openxlsx
    tools,
    ggplot2,
    stringr,
    utils,
    stats,
    bbmle,
    binom,
    caTools,
    mvtnorm,
    numDeriv,
    Rcpp
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
