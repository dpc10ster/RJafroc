#################
## This runs more reliably on iMac, not M2
#################


library(rhub)

# CRAN check flavors
# https://cran.r-project.org/web/checks/check_flavors.html

platf <- rhub::platforms()

indx_packages_cran <- c(1,2,6,7,12,4,5,15,13)

packagePath <- "/Users/Dev/GitHub/RJafroc_2.1.3.tar.gz"
if (!file.exists(packagePath))
  packagePath <- devtools::build()

##
## RUN in Terminal window on iMac
## caffeinate -d
## 

#for (i in 1:length(indx_packages_cran)) {
for (i in 1:1) {
  descr <- platf$description[indx_packages_cran[i]]
  name <- platf$name[indx_packages_cran[i]]
  cat(descr,"\n")
  chk1 <- rhub::check(path = packagePath, platform = name)
}

