#################
## This runs more reliably on iMac, not M2
#################


library(devtools)
library(rhub)
library(RJafroc)

# CRAN check flavors
# https://cran.r-project.org/web/checks/check_flavors.html

platf <- rhub::platforms()

indx_packages_cran <- c(1,2,6,7,12,8,15,13)

packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.3.tar.gz"
if (!file.exists(packagePath))
  packagePath <- devtools::build()

##
## RUN in Terminal window
## caffeinate -d
## 

for (indx in 1:length(indx_packages_cran)) {
  indx1 <- platf[[1]][indx_packages_cran[indx]]
  cat(indx1,"\n")
  chk1 <- rhub::check(packagePath, platf = indx1, email = "dpc10ster@gmail.com")
}

