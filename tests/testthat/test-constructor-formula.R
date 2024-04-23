testthat::context("Abstraction level for parameters works as expected")

data <- data.table::data.table(
  'x' = rnorm(10),
  'y' = sample(1:100, size = 10, replace = TRUE),
  'z' = 10L
)

# ERRORS ---------

testthat::test_that(
  "Errors when formula without parameters",
  {
    testthat::expect_error(
      constructor_interest(r = "r ~ x", data = data),
      "When r is a formula, r.parameters vector should be provided"
    )
    testthat::expect_error(
      constructor_gamma(gamma = "gamma ~ x", data = data),
      "When gamma is a formula, gamma.parameters vector should be provided"
    )
    testthat::expect_error(
      constructor_beta(r = "r ~ x", data = data),
      "When beta is a formula, beta.parameters vector should be provided"
    )
  }
)


# FORMULA ----------

testthat::test_that(
  "[r] Formula is passed",{
    testthat::expect_equal(
      constructor_interest(r = "r ~ x", data = data, r.parameters = c(1,1.5))[['r']],
      1 + 1.5*data$x
    )
    testthat::expect_equal(
      constructor_interest(r = "r ~ 0 + x + y", data = data, r.parameters = c(1,1.5))[['r']],
      data$x + 1.5*data$y
    )
    testthat::expect_identical(
      constructor_interest(r = as.formula("r ~ x"), data = data, r.parameters = c(1,1.5)),
      constructor_interest(r = "r ~ x", data = data, r.parameters = c(1,1.5))
    )
  }
)

testthat::test_that(
  "[gamma] Formula is passed",{
    testthat::expect_equal(
      constructor_gamma(gamma = "gamma ~ x", data = data, gamma.parameters = c(1,1.5))[['gamma']],
      1 + 1.5*data$x
    )
    testthat::expect_equal(
      constructor_gamma(gamma = "gamma ~ 0 + x + y", data = data, gamma.parameters = c(1,1.5))[['gamma']],
      data$x + 1.5*data$y
    )
    testthat::expect_identical(
      constructor_gamma(gamma = as.formula("gamma ~ x"), data = data, gamma.parameters = c(1,1.5)),
      constructor_gamma(gamma = "gamma ~ x", data = data, gamma.parameters = c(1,1.5))
    )
  }
)

testthat::test_that(
  "[beta] Formula is passed",{
    testthat::expect_equal(
      constructor_beta(beta = "beta ~ x", data = data, beta.parameters = c(1,1.5))[['beta']],
      1 + 1.5*data$x
    )
    testthat::expect_equal(
      constructor_beta(beta = "beta ~ 0 + x + y", data = data, beta.parameters = c(1,1.5))[['beta']],
      data$x + 1.5*data$y
    )
    testthat::expect_identical(
      constructor_beta(beta = as.formula("beta ~ x"), data = data, beta.parameters = c(1,1.5)),
      constructor_beta(beta = "beta ~ x", data = data, beta.parameters = c(1,1.5))
    )
  }
)


# parameter

testthat::test_that(
  "when giving parameter, returns a vector",
  {
    testthat::expect_equal(
      constructor_interest(r = 0.02, data = data)[['r']],
      rep(0.02, times = nrow(data))
    )
    testthat::expect_equal(
      constructor_interest(r = 0.05, data = data)[['r']],
      rep(0.05, times = nrow(data))
    )
    testthat::expect_equal(
      constructor_gamma(gamma = 0.02, data = data)[['gamma']],
      rep(0.02, times = nrow(data))
    )
    testthat::expect_equal(
      constructor_gamma(gamma = 0.05, data = data)[['gamma']],
      rep(0.05, times = nrow(data))
    )
    testthat::expect_equal(
      constructor_beta(beta = 0.02, data = data)[['beta']],
      rep(0.02, times = nrow(data))
    )
    testthat::expect_equal(
      constructor_beta(beta = 0.05, data = data)[['beta']],
      rep(0.05, times = nrow(data))
    )
  }
)
