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

# Summary

See [this link](https://dpc10ster.github.io/ai-froc-research/) for a summary of my AI/FROC research. 

# RJafroc Update History

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
