library(devtools)
library(rhub)
library(RJafroc)

# CRAN check flavors
# https://cran.r-project.org/web/checks/check_flavors.html

platforms <- rhub::platforms()

indx_packages_cran <- c(1,2,6,7,12,8,15,13)  
  
packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.3.tar.gz"
if (!file.exists(packagePath))
  packagePath <- devtools::build()

for (indx in 7:length(indx_packages_cran)) { 
  cat(platforms[[1]][indx_packages_cran[indx]],"\n")
  chk1 <- rhub::check(packagePath, platforms = platforms[[1]][indx])
  next
}

# devtools::check_win_devel()
# devtools::check_win_release()
# devtools::check_win_oldrelease()

# rhub::check_for_cran() 

# devtools::revdep()

