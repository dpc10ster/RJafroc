library(devtools)
library(rhub)
library(RJafroc)
paths <- rhub::platforms()

# > paths[[1]]
# [1] "debian-clang-devel"            "debian-gcc-devel"              "debian-gcc-devel-nold"        
# [4] "debian-gcc-patched"            "debian-gcc-release"            "fedora-clang-devel"           
# [7] "fedora-gcc-devel"              "linux-x86_64-centos6-epel"     "linux-x86_64-centos6-epel-rdt"
# [10] "linux-x86_64-rocker-gcc-san"   "macos-highsierra-release"      "macos-highsierra-release-cran"
# [13] "solaris-x86-patched"           "solaris-x86-patched-ods"       "ubuntu-gcc-devel"             
# [16] "ubuntu-gcc-release"            "ubuntu-rchk"                   "windows-x86_64-devel"         
# [19] "windows-x86_64-oldrel"         "windows-x86_64-patched"        "windows-x86_64-release"  

# update DESCRIPTION ...DONE
# update cran-comments ...DONE

start <- 1; end <- 21; for (i in start:end) rhub::check(platform = paths[[1]][i])
devtools::check_win_devel()
devtools::check_win_release()
devtools::check_win_oldrelease()

devtools::revdep()

indx -> c(5, 16, 17, 18, 20);for (i in indx) rhub::check(platform = paths[[1]][i])