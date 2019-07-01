context("Sample Size ROC")

test_that("SsPowerGivenJK:DBMH: expected values are obtained for dataset02", 
          expect_equivalent(SsPowerGivenJK(dataset02, J = 6, K = 111, option = "RRRC")$powerRRRC, 
                            0.5526116, tolerance = 5e-8))

test_that("SsPowerGivenJK:ORH: expected values are obtained for dataset02", 
          expect_equivalent(SsPowerGivenJK(dataset02, J = 6, K = 111, option = "RRRC", method = "ORH")$powerRRRC, 
                            0.5526116, tolerance = 5e-8))


test_that("SsSampleSizeKGivenJ:DBMH: expected values are obtained for dataset02", 
          expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, option = "RRRC")$KRRRC, 
                            251))

test_that("SsSampleSizeKGivenJ:ORH: expected values are obtained for dataset02", 
          expect_equal(SsSampleSizeKGivenJ(dataset02, J = 6, option = "RRRC", method = "ORH")$KRRRC, 
                       251))

x <- SsPowerTable(dataset02)
y <- x$powerTableRRRC
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numReaders[4]),6))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numCases[4]),251))
test_that("SsPowerTable:DBMH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$power[4]),0.801))

x <- SsPowerTable(dataset02, method = "ORH")
y <- x$powerTableRRRC
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numReaders[4]),6))
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$numCases[4]),251))
test_that("SsPowerTable:ORH: expected values are obtained for dataset02",
          expect_equal(as.numeric(y$power[4]),0.801))
