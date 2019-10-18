# careful here
# uncomment and run to clean out goodValues361 and generate new ones
# 
# fns <- list.dirs("tests/testthat/goodValues361")
# for (i in 2:length(fns)) cat(paste0(fns[i], "/*"),"\n")
# for (i in 2:length(fns)) unlink(paste0(fns[i], "/*")) # starting from 2 as dont want to delete the goodValues directory
# devtools::test()
