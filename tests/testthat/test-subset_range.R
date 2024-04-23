testthat::context("subset_range in Rcpp as in R")

# BE CAREFUL: indexes start at 0

testthat::test_that("Same output than base R solution", {
  
  testthat::expect_equal(
    subset_range(seq_len(100L), start = 0, end = 10),
    seq_len(100L)[1:11]
  )
  
  testthat::expect_equal(
    subset_range(seq_len(100L), start = 20, end = 80),
    seq_len(100L)[21:81]
  )
  
})

