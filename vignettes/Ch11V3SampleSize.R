## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(RJafroc)
library(ggplot2)

## ------------------------------------------------------------------------
power <- SsPowerGivenJK(dataset02, J = 6, K = 112, option = "RRRC")

## ------------------------------------------------------------------------
str(power)

## ------------------------------------------------------------------------
powTab <- SsPowerTable(dataset02, method = "DBMH", option = "RRRC")

## ------------------------------------------------------------------------
powTab

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, J = 10, method = "DBMH", option = "RRRC")
str(ncases)

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, J = 10, method = "DBMH", option = "RRRC", desiredPower = 0.9)
str(ncases)

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, J = 10, method = "DBMH", option = "FRRC")
str(ncases)

## ------------------------------------------------------------------------
ncases <- SsSampleSizeKGivenJ(dataset02, J = 10, method = "DBMH", option = "RRFC")
str(ncases)

