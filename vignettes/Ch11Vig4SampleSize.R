## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## ------------------------------------------------------------------------
power <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", 6, 251, method = "ORH", option = "RRRC")

## ------------------------------------------------------------------------
str(power)

## ------------------------------------------------------------------------
powTab <- SsPowerTable(dataset02, FOM = "Wilcoxon", method = "ORH", option = "RRRC")

## ------------------------------------------------------------------------
powTab

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "ORH", option = "RRRC")

## ------------------------------------------------------------------------
str(ncases)

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "ORH", option = "FRRC")

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "ORH", option = "RRFC")

