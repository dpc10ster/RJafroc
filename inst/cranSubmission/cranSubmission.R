library(devtools)
library(rhub)
library(RJafroc)

platforms <- rhub::platforms()
print(platforms)
# debian-clang-devel:
#   Debian Linux, R-devel, clang, ISO-8859-15 locale
# debian-gcc-devel:
#   Debian Linux, R-devel, GCC
# debian-gcc-devel-nold:
#   Debian Linux, R-devel, GCC, no long double
# debian-gcc-patched:
#   Debian Linux, R-patched, GCC
# debian-gcc-release:
#   Debian Linux, R-release, GCC
# fedora-clang-devel:
#   Fedora Linux, R-devel, clang, gfortran
# fedora-gcc-devel:
#   Fedora Linux, R-devel, GCC
# linux-x86_64-rocker-gcc-san:
#   Debian Linux, R-devel, GCC ASAN/UBSAN
# macos-highsierra-release:
#   macOS 10.13.6 High Sierra, R-release, brew
# macos-highsierra-release-cran:
#   macOS 10.13.6 High Sierra, R-release, CRAN's setup
# macos-m1-bigsur-release:
#   Apple Silicon (M1), macOS 11.6 Big Sur, R-release
# solaris-x86-patched:
#   Oracle Solaris 10, x86, 32 bit, R-release
# solaris-x86-patched-ods:
#   Oracle Solaris 10, x86, 32 bit, R release, Oracle Developer Studio 12.6
# ubuntu-gcc-devel:
#   Ubuntu Linux 20.04.1 LTS, R-devel, GCC
# ubuntu-gcc-release:
#   Ubuntu Linux 20.04.1 LTS, R-release, GCC
# windows-x86_64-devel:
#   Windows Server 2022, R-devel, 64 bit
# windows-x86_64-oldrel:
#   Windows Server 2022, R-oldrel, 32/64 bit
# windows-x86_64-patched:
#   Windows Server 2022, R-patched, 32/64 bit
# windows-x86_64-release:
#   Windows Server 2022, R-release, 32/64 bit

packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.0.tar.gz"
if (!file.exists(packagePath))
  packagePath <- devtools::build()

# for (indx in 1:8) {
#   if (indx == 12) next
#   if (indx == 13) next
#   print(cat(platforms[[1]][indx]))
#   chk1 <- rhub::check(packagePath, platform = platforms[[1]][indx]) # OK
# }

# Next three need to be run individually in Console
# devtools::check_win_devel() #OK
# devtools::check_win_release() #OK
# devtools::check_win_oldrelease() #OK

# rhub::check_for_cran() # OK detritus file lastMiKTeXException?

# devtools::revdep() # OK

# update DESCRIPTION ...NOT DONE
# update cran-comments ...NOT DONE


