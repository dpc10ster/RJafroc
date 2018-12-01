context("ROC sample size routines")
tmp <- tempfile()
expect_known_output(
  SsPowerGivenJK(dataset02, 6, 251, method = "DBMH"), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
expect_known_output(
  SsPowerGivenJK(dataset02, 6, 251, method = "ORH"), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
expect_known_output(
  SsSampleSizeKGivenJ(dataset02, J = 6, method = "DBMH"), 
  tmp, print = TRUE, update = TRUE)

tmp <- tempfile()
expect_known_output(
  SsPowerTable(dataset02, method = "DBMH"), 
  tmp, print = TRUE, update = TRUE)


