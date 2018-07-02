#source("real_roots.R")

test_that("Distinct roots", {

  roots <- real.roots(1, 7, 12)

  expect_true( is.numeric(roots) )
  expect_length( roots, 2 )
  expect_lt( roots[1] , roots[2])
})

test_that("Repeated root", {

  roots <- real.roots(1, 6000, 9000000)

  expect_equal( length(roots), 1)

  expect_equal( roots, -3000)

  # Test whether ABSOLUTE error is within 0.1
  expect_equal( roots, -3100.01, tolerance  = 0.1)

  # Test whether RELATIVE error is within 0.1
  # To test relative error, set 'scale' equal to expected value.
  # See base R function all.equal for optional argument documentation.
  expect_equal( roots, -3901, tolerance  = 0.1, scale=-3000)
})

test_that("Polynomial must be quadratic", {

  # Test for ANY error
  expect_error( real.roots(0, 2, 3))

  # Test specifically for an error string containing "zero"
  expect_error( real.roots(0, 2, 3), "zero" )

  # Test specifically for an error string containing "zero" or "Zero" using regular expression
  expect_error( real.roots(0, 2, 3), "[zZ]ero" )
})



# test_that("Bogus tests", {
# 
#   x <- c(1, 2, 3)
# 
#   expect_that( length(x), equals(2.7) )
#   expect_that( x, is_a("data.frame") )
# })

