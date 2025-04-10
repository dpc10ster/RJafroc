# This checks for possible change in R random number generator
context("Check Random Number Generator Stability")
test_that("RNG", {
  
  seeds <- seq(1,1000, 10)
  for (i in seeds) {
    set.seed(i)
    fn <- paste0(test_path(), paste0("/goodValues361/RNG/", "seed", i), ".rds")
    if (!file.exists(fn)) {
      warning(paste0("File not found - generating new ",fn))
      x <- rnorm(10)
      saveRDS(x, file = fn)
    }
    x <- readRDS(fn)
    set.seed(i)
    y <- rnorm(10)
    expect_equal(x, y)
  }

})  