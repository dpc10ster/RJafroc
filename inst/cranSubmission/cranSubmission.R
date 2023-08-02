#################
## This runs more reliably on iMac, not M2
#################


library(rhub)


# CRAN check flavors
# https://cran.r-project.org/web/checks/check_flavors.html

#
# platf <- rhub::platforms()
# 
# indx_packages_cran <- c(1,2,6,7,12,4,5,15,13)
# 
# packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.3.tar.gz"
# if (!file.exists(packagePath))
#   packagePath <- devtools::build()
# 
##
## RUN in Terminal window
## caffeinate -d
## 

rhub::validate_email()
rhub::check() # then select platform from menu

# 1: Debian Linux, R-devel, clang, ISO-8859-15 locale (debian-clang-devel)
# 2: Debian Linux, R-devel, GCC (debian-gcc-devel)
# 3: Debian Linux, R-devel, GCC, no long double (debian-gcc-devel-nold)
# 4: Debian Linux, R-patched, GCC (debian-gcc-patched)
# 5: Debian Linux, R-release, GCC (debian-gcc-release)
# 6: Fedora Linux, R-devel, clang, gfortran (fedora-clang-devel)
# 7: Fedora Linux, R-devel, GCC (fedora-gcc-devel)
# 8: Debian Linux, R-devel, GCC ASAN/UBSAN (linux-x86_64-rocker-gcc-san)
# 9: Ubuntu Linux 20.04.1 LTS, R-devel, GCC (ubuntu-gcc-devel)
# 10: Ubuntu Linux 20.04.1 LTS, R-release, GCC (ubuntu-gcc-release)
# 11: Ubuntu Linux 20.04.1 LTS, R-devel with rchk (ubuntu-rchk)
# 12: Windows Server 2022, R-devel, 64 bit (windows-x86_64-devel)
# 13: Windows Server 2022, R-oldrel, 32/64 bit (windows-x86_64-oldrel)
# 14: Windows Server 2022, R-patched, 32/64 bit (windows-x86_64-patched)
# 15: Windows Server 2022, R-release, 32/64 bit (windows-x86_64-release)
