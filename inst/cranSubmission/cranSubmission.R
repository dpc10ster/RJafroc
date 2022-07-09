library(devtools)
library(rhub)
library(RJafroc)

platforms <- rhub::platforms()
# > platforms[[1]]
# "macos-highsierra-release"      
# "macos-highsierra-release-cran" 
# "macos-m1-bigsur-release"       
# "solaris-x86-patched"          
# "solaris-x86-patched-ods" 

packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.0.tar.gz"
if (!file.exists(packagePath))
 packagePath <- devtools::build()

# devtools::check_win_devel(packagePath) #OK
# devtools::check_win_release(packagePath) #OK
# devtools::check_win_oldrelease(packagePath) #OK
# devtools::revdep() # NONE

# chk1 <- rhub::check(packagePath, platform = platforms[[1]][1]) # OK
# chk2 <- rhub::check(packagePath, platform = platforms[[1]][2]) # OK

chk3 <- rhub::check(packagePath, platform = platforms[[1]][3]) # fails error in SsFrocNhRsmModel.R line 55
# chk4 <- rhub::check(packagePath, platform = platforms[[1]][4]) # failed to download dependencies readxl, testthat kableExtra
# chk5 <- rhub::check(packagePath, platform = platforms[[1]][5]) # failed to download dependencies readxl, testthat kableExtra

# rhub::check_for_cran(packagePath) # Error in match_platform(platform)


# update DESCRIPTION ...NOT DONE
# update cran-comments ...NOT DONE


