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

# check "ubuntu-gcc-devel"
rhub::check(platform = paths[[1]][15])

# update DESCRIPTION ...DONE
# update cran-comments

start <- 8; end <- 12; for (i in start:end) {rhub::check(platform = paths[[1]][i])}