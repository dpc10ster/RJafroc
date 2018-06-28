source("real_roots.R")

test_that("Distinct roots", {

  roots <- real.roots(1, 7, 12)

  expect_that( roots, is_a("numeric") )
  expect_that( length(roots), equals(2) )
  expect_that( roots[1] < roots[2], is_true() )
})

test_that("Repeated root", {

  roots <- real.roots(1, 6000, 9000000)

  expect_that( length(roots), equals(1) )

  expect_that( roots, equals(-3000) )

  # Test whether ABSOLUTE error is within 0.1
  expect_that( roots, equals(-3000.01, tolerance  = 0.1) )

  # Test whether RELATIVE error is within 0.1
  # To test relative error, set 'scale' equal to expected value.
  # See base R function all.equal for optional argument documentation.
  expect_equal( roots, -3001, tolerance  = 0.1, scale=-3001)
})

test_that("Polynomial must be quadratic", {

  # Test for ANY error
  expect_that( real.roots(0, 2, 3), throws_error() )

  # Test specifically for an error string containing "zero"
  expect_that( real.roots(0, 2, 3), throws_error("zero") )

  # Test specifically for an error string containing "zero" or "Zero" using regular expression
  expect_that( real.roots(0, 2, 3), throws_error("[zZ]ero") )
})



# test_that("Bogus tests", {
#   
#   x <- c(1, 2, 3)
#   
#   expect_that( length(x), equals(2.7) )
#   expect_that( x, is_a("data.frame") )
# })

