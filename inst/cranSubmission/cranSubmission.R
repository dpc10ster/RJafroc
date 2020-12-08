library(devtools)
library(rhub)
library(RJafroc)
paths <- rhub::platforms()
# check "ubuntu-gcc-devel"
rhub::check(platform = paths[[1]][15])