RJafroc
========

[![Build Status](https://travis-ci.org/dpc10ster/rjafroc.svg?branch=master)](https://travis-ci.org/dpc10ster/rjafroc)
[![codecov](https://codecov.io/gh/dpc10ster/rjafroc/branch/master/graph/badge.svg)](https://codecov.io/gh/dpc10ster/rjafroc)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjafroc)](https://cran.r-project.org/package=rjafroc)

# The `VarCompDiscrepancy` branch #
This branch is for a collaboration (with Dr. Jason Cai) who noted the discrepancy when comparing jackknife vs bootstrap for `includedFrocData.xlsx` using `HrAuc` FOM. The bootstrap values are quite stable w.r.t. `nBoots` (200, 1000), while the jackknife values are very different. His independent SAS implementation agrees with my bootstrap implementation, and with his jackknife implementation. Something is wrong with my jackknife implementation. This branch is a diversion from the split plot branch to fix this issue. When done, it should be merged with split plot. 

# The `OldCode` branch #
This branch compares the significance testing functions with the original/old Xuetong Zhai code, in the first posted version of RJafroc (0.0.1) - this version does not have an C++-Code. Functions from here were copied over and unit test (`testthat`) functions written to compare:

## New/current code vs. old code for DBMH/ORH methods,
```
test_that("Compare current to original code: DBMH", {
...
}
```
test_that("Compare current to original code: ORH", {
...
}
```
## DBMH and ORH variance components vs. those obtained from Windows version of JAFROC
```
test_that("Ensure that DBM varComp values match those from Windows JAFROC", {
...
}

test_that("Ensure that OR varComp values match those from Windows JAFROC", {
...
}
```
## DBMH vs ORH using current code for an ROC and an FROC dataset
```
test_that("Compare DBMH to ORH for dataset02, ROC", {
...
}

test_that("Compare DBMH to ORH for dataset05, FROC, HrAuc", {
...
}
```


```
StSignificanceTesting(dataset, FOM, FPFValue = 0.2, alpha = 0.05,
  method = "DBMH", covEstMethod = "Jackknife", nBoots = 200,
  option = "ALL", tempOrgCode = FALSE)
```
