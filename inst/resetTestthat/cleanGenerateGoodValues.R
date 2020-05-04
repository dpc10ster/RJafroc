# careful here ...!!!...
# uncomment and run to clean out goodValues361 and generate new ones
# 
fns <- list.dirs("tests/testthat/goodValues361")
for (i in 2:length(fns)) cat(paste0(fns[i], "/*"),"\n")
for (i in 2:length(fns)) {
  # starting from 2 as we dont want to delete the goodValues directory!!
  if (grepl("CORCBM", fns[i], fixed = TRUE)) {
    # skip the CORCBM folder as this takes very long and is not changing
    cat("skipping ", fns[i], "\n")
    next
  }
  cat("Doing ", fns[i], "\n")
  # careful here ...!!!...
  # uncomment following line and run to clean out goodValues361 and generate new ones
  # unlink(paste0(fns[i], "/*")) # this deletes all files in directory but not the directory
}
# devtools::test()
