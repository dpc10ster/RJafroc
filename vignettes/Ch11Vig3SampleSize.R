## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## -----------------------------------------------------------------------------
power <- SsPowerGivenJK(dataset02, FOM = "Wilcoxon", J = 6, K = 112, analysisOption = "RRRC")

## -----------------------------------------------------------------------------
str(power)

## -----------------------------------------------------------------------------
powTab <- SsPowerTable(dataset02, FOM = "Wilcoxon", method = "DBMH", analysisOption = "RRRC")

## -----------------------------------------------------------------------------
powTab

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "DBMH", analysisOption =  "RRRC")
str(ncases)

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "DBMH", analysisOption = "RRRC", desiredPower = 0.9)
str(ncases)

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "DBMH", analysisOption = "FRRC")
str(ncases)

## -----------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, FOM = "Wilcoxon", J = 10, method = "DBMH", analysisOption = "RRFC")
str(ncases)

