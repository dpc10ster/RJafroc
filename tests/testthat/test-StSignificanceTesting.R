context("StSignificanceTesting tests")
library(RJafroc)


test_that("StSignificanceTesting should error on unknown FOM", {
  expect_error(StSignificanceTesting(dataset02,FOM = "UNKNOWN", method = "DBMH"),"Must use Wilcoxon figure of merit with ROC data." )
})

test_that("StSignificanceTesting should error on unknown method", {
  expect_error(StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "UNKNOWN"),"is not a valid analysis method." )
})

test_that("StSignificanceTesting: no warnings/errors using dataset02", {
  expect_silent(StSignificanceTesting(dataset02,FOM = "Wilcoxon", method = "DBMH"))
})

test_that("StSignificanceTesting: should error on incorrect FOM with FROC or ROI data", {
  expect_error(StSignificanceTesting(dataset05,FOM = "Wilcoxon", method = "DBMH"),"Cannot use `Wilcoxon` FOM with `FROC` or `ROI` data.")
})