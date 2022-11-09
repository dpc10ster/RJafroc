--- 
title: "RJafroc software"
author: "Dev P. Chakraborty, PhD"
date: "`r Sys.Date()`"
---

RJafroc
=======

[![R build status](https://github.com/dpc10ster/RJafroc/workflows/R-CMD-check/badge.svg)](https://github.com/dpc10ster/RJafroc/actions)
[![codecov](https://codecov.io/gh/dpc10ster/rjafroc/branch/master/graph/badge.svg)](https://codecov.io/gh/dpc10ster/rjafroc)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/RJafroc)](https://cran.r-project.org/package=RJafroc)

# Purpose

* This website contains four repositories describing **observer performance and artificial intelligence systems modeling, analysis and validation**. 
* They are:
    + [`RJafroc`](https://github.com/dpc10ster/RJafroc) is the software package that provides the common thread on which the other three online books depend; 
    + [`RJafrocQuickStart`](https://github.com/dpc10ster/RJafrocQuickStart) is an online book for those already somewhat familiar running Windows JAFROC. The obsolete Windows program has been replaced by `RJafroc`. This book dives into how to use `RJafroc` to analyze ROC or FROC datasets.
    + [`RJafrocRocBook`](https://github.com/dpc10ster/RJafrocRocBook) is an online book providing background on the ROC paradigm, modeling and analysis.  
    + [`RJafrocFrocBook`](https://dpc10ster.github.io/RJafrocFrocBook/) is an online book providing a detailed exposition of the FROC paradigm, modeling and analysis.

The first CRAN-posted version of `RJafroc` was used to support the R-code examples in the book: Chakraborty DP: Observer Performance Methods for Diagnostic Imaging - Foundations, Modeling, and Applications with R-Based Examples, Taylor-Francis LLC, 2017. 

Since its publication in 2017 `RJafroc`, on which the `R` code examples in the print book depend, has evolved considerably, causing many of the examples to "break" if one uses the most current version of `RJafroc`. The code will still run if one uses [`RJafroc` 0.0.1](https://cran.r-project.org/src/contrib/Archive/RJafroc/) but this is inconvenient and misses out on many of the software improvements made since the print book appeared.

This, and other considerations, led me to conclude that an update to the book is needed. This website contains the updated software and three online books that use the software and extend the 2017-book.


# Documentation

* HTML documentation for `RJafroc` (functions, vignettes and update history) is [available here](https://dpc10ster.github.io/RJafroc/). The software is fairly stable and undergoing occasional updates. 

* The HTML online book `RJafrocQuickStart` is available [here](https://dpc10ster.github.io/RJafrocQuickStart/). A link is provided there to download the pdf book. Both are being continually updated (as of December 2021).  

* The HTML online book `RJafrocRocBook` is available [here](https://dpc10ster.github.io/RJafrocRocBook/). A link is provided there to download the pdf book. Both of them are being continually updated.  

* The HTML online book `RJafrocFrocBook` is available [here](https://dpc10ster.github.io/RJafrocFrocBook/). A link is provided there to download the pdf book. Both are being continually updated.  


# Applications

While most of the applications in this package are geared toward analyzing radiologist performance in search tasks such as finding lesions in medical images, the software applies to any task involving detection and localization of targets in images. For example, the functions in `RJafroc` can be used to analyze the performance of artificial intelligence (AI) algorithms. Two applications to AI are [here](https://dpc10ster.github.io/RJafrocFrocBook/), specifically:

* Measuring AI performance.
* Optimizing the reporting threshold of an AI algorithm.

The radiological search model (RSM), described [here](https://dpc10ster.github.io/RJafrocFrocBook/) is implemented in `RJafroc`. A fitting function `RJafroc::FitRsmRoc` estimates RSM parameters from ROC data These parameters are related to search and classification performances: 

* Search performance refers to finding lesions while simultaneously minimizing finding non-lesion locations 
* Classification performance measures ability to distinguish between lesion and non-lesion locations. 

Knowing the individual performances allows principled optimization of reader or AI algorithm performance.

# Relation to Windows software

* `RJafroc` extends Windows `JAFROC` software and runs on multiple platforms.
* Originally uploaded in 2004, the Windows software is many generations behind the software available on this website. However, many users find it to be easy to use and useful. Vignettes in `RJafroc` and the online book available [here](https://dpc10ster.github.io/RJafrocQuickStart/) should allow one to quickly transition to `RJafroc`.
* If you still need Windows `JAFROC` software it is still available [here](https://github.com/dpc10ster/WindowsJafroc).

# Update History

* [See here](https://dpc10ster.github.io/RJafroc/news/index.html) for details.
* Current `GitHub` version is 2.1.2.9000 on the `master` branch.
* Current `CRAN` version is 2.1.2.

# `RJafroc` branch designations

* The most current tested version of the software and documentation is on the `master` branch.
* The `developer` branch is for development/experimental work.

# Installation: those already familiar with installing R packages from GitHub can ignore the following directions  

# How do I get set up?

Three options are given below in increasing order of complexity. 
    + The first method downloads the package from CRAN.
    + The second downloads the package from the `master` branch on `GitHub`. 
    + The third downloads all source files from the `master` branch on `GitHub` and then installs the software. I recommend the second method as the CRAN package is behind the `master` branch.

## 1. Install from CRAN
* Install `R` and `RStudio`.
* Create an empty directory, e.g., `myProject`. In my computer it is `/Users/Dev/Downloads/myProject`.
* Open `RStudio` by clicking on the `myProject.Rproj` file.
* Starting from `RStudio` > `Packages` > `Install` > `RJafroc`.
* `library(RJafroc)`.
* This loads the CRAN package and all functions on the `cran2` branch become available.
* For documentation of functions and vignettes, [see](https://dpc10ster.github.io/RJafroc/). It may be helpful to have both windows open (`RStudio` and the above website) simultaneously.
* The CRAN version will not include more recent updates on the `master` branch. To access them use the next suggested method, below.
* Test the installation:

```
cbmPlot <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
print(cbmPlot)
```
* You should see two ROC plots in the `Plots` window.

## 2. Install from GitHub using package `devtools`
* Install `R` and `RStudio`.
* Create an empty directory, e.g., `myProject`. In my computer it is `/Users/Dev/Downloads/myProject`.
* Open `RStudio` by clicking on the `myProject.Rproj` file.
* Starting from `RStudio` > `File` > `New Project` > `Existing Directory` > Select `myProject` > `Create Project`.
* Oila! You should see `myProject.RProj` in the Files menu.
* Install the `devtools` package as shown below:
* Starting from `RStudio` > `Packages` > `Install` > `devtools`.
* Load `devtools` as shown below:
```
library(devtools)
```
* Install `RJafroc` directly from `GitHub` (this is where `devtools` is used):
```
install_github("dpc10ster/RJafroc")
```
* Hit Enter on any prompts...
* Lots of activity and compilation of C++ code ....
* Load `RJafroc` as shown below:
```
library(RJafroc)
```
* Test the installation:
```
cbmPlot <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
print(cbmPlot)
```
* You should see two ROC plots in the `Plots` window.
* Documentation (vignettes) is [available](https://dpc10ster.github.io/RJafroc/).
* Be sure to study these examples and make full use of the online documentation.
* Put your data and other files, if any, in `myProject`.

## 3. Download the `RJafroc` source files and install from the downloaded files
* Clone this repository to a directory anywhere on your computer. On my computer it is in `/Users/Dev/Downloads/RJafroc`. Rename the folder if necessary to match my example.
* I find the GitHub desktop app useful in mananging my downloads/uploads from `Git`.
* Install `R` and `RStudio`.
* Navigate to the `RJafroc` directory.
* Open `RJafroc.Rproj`. This will open `RStudio`.
* Navigate to `File` menu (lower-right window) and click on DESCRIPTION file.
* Install all packages listed under Imports, e.g.,    
    openxlsx,
    ggplot2,
    stringr,
    tools,
    utils,
    stats,
    bbmle,
    binom,
    mvtnorm,
    dplyr,
    numDeriv,
    Rcpp
* For example, to install the first two above-listed packages, use the following command at the Console prompt:
```
install.packages(c("openxlsx", "ggplot2"))
```
* Click on Build > Install and Restart (upper right panel). If errors result from missing packages, install those packages.
* A successful Install and Restart will result in the following line in the Console window:
```
library(RJafroc)
```
* Thats it! `RJafroc` has been installed to your computer and is visible to any other `R` project in any directory.
* You will not need to access the `RJafroc` folder again (unless you reinstall a new version of the software).
* Do not write any of your files to the `RJafroc` directory!
* All necessary files of the installation are in a hidden directory that you do not normally need to worry about.
* Create an empty directory, e.g., `myProject`, outside of the `RJafroc` folder. In my computer it is `/Users/Dev/Downloads/myProject`.
* Starting from `RStudio` > `File` > `New Project` > `Existing Directory` > `myProject` > `Create Project`.
* Oila! You should see `myProject.RProj` in the Files menu.
* Click on `Packages` and scroll down to find `RJafroc`, and check the box next to it. This results in `RJafroc` being loaded to the current workspace. The following line appears in the Console window (this is the hidden directory referrred to above).
```
  library("RJafroc", lib.loc="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
```  
* Click on `RJafroc` in the packages window. A help window opens up. I find it convenient to put this in its own window by clicking the "out" arrow button (hover message: Show in new window). You can access all documentation from here.
* Test the installation:
```
cbmPlot <- PlotCbmFit(c(1, 2), c(0.5, 0.5))
print(cbmPlot)
```
* You should see two ROC plots in the `Plots` window.
* Preliminary documentation (vignettes) is [available](https://dpc10ster.github.io/RJafroc/).
* Put your data and other files, if any, in myProject.
* TBA

### Contibutor guidelines (adapted from `devtools` `GitHub` page.)

* As contributors and maintainers of this project, we pledge to respect all people who contribute through reporting issues, posting feature requests, updating documentation, submitting pull requests or patches, and other activities.

* We are committed to making participation in this project a harassment-free experience for everyone, regardless of level of experience, gender, gender identity and expression, sexual orientation, disability, personal appearance, body size, race, ethnicity, age, or religion.

* Examples of unacceptable behavior by participants include the use of sexual language or imagery, derogatory comments or personal attacks, trolling, public or private harassment, insults, or other unprofessional conduct.

* Project maintainers have the right and responsibility to remove, edit, or reject comments, commits, code, wiki edits, issues, and other contributions that are not aligned to this Code of Conduct. Project maintainers who do not follow the Code of Conduct may be removed from the project team.

* Instances of abusive, harassing, or otherwise unacceptable behavior may be reported by opening an issue or contacting one or more of the project maintainers.

### Who do I talk to?

dpc10ster@gmail.com
