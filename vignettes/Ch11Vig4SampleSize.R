## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)

## -----------------------------------------------------------------------------
power <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 251, method = "OR", analysisOption = "RRRC")

## -----------------------------------------------------------------------------
str(power)

## -----------------------------------------------------------------------------
powTab <- SsPowerTable(dataset02, FOM = "Wilcoxon", analysisOption = "RRRC")

## -----------------------------------------------------------------------------
powTab

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "OR", analysisOption = "RRRC")

## -----------------------------------------------------------------------------
str(ncases)

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "OR", analysisOption = "FRRC")

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "OR", analysisOption = "RRFC")

