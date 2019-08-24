RJafroc
========

[![Build Status](https://travis-ci.org/dpc10ster/rjafroc.svg?branch=master)](https://travis-ci.org/dpc10ster/rjafroc)
[![codecov](https://codecov.io/gh/dpc10ster/rjafroc/branch/master/graph/badge.svg)](https://codecov.io/gh/dpc10ster/rjafroc)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/rjafroc)](https://cran.r-project.org/package=rjafroc)

# The `VarCompDiscrepancy` branch #
This branch is for a collaboration (with Dr. Jason Cai) who noted the discrepancy when comparing jackknife vs bootstrap for `includedFrocData.xlsx` using `HrAuc` FOM. The bootstrap values are quite stable w.r.t. `nBoots` (200, 1000), while the jackknife values are very different. His independent SAS implementation agrees with my bootstrap implementation, and with his jackknife implementation. Something is wrong with my jackknife implementation. This branch is a diversion from the split plot branch to fix this issue. When done, it should be merged with split plot. 


