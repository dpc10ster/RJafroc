library(devtools)
library(rhub)
library(RJafroc)
paths <- rhub::platforms()

# > paths[[1]]
# "macos-highsierra-release"      
# "macos-highsierra-release-cran" 
# "macos-m1-bigsur-release"       
# "solaris-x86-patched"          
# "solaris-x86-patched-ods" 
# update DESCRIPTION ...NOT DONE
# update cran-comments ...NOT DONE

#start <- 1; end <- 5; for (i in start:end) rhub::check(platform = paths[[1]][i])
#devtools::check_win_devel() # OK
#devtools::check_win_release()
#devtools::check_win_oldrelease()

#devtools::revdep()

#indx -> c(5, 16, 17, 18, 20);for (i in indx) rhub::check(platform = paths[[1]][i])