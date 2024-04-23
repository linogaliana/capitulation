testthat::context("Inheritance model correctly applied")


# PREREQUISITE 1: ESTIMATE A MODEL ----------------

set.seed(242)

n <- 250

x1 <- sample(c(0, 1), n,
             replace = TRUE,
             prob = c(0.75, 0.25))

x2 <- vector("numeric", n)
x2[x1 == 0] <- sample(c(0, 1),
                      n - sum(x1 == 1),
                      replace = TRUE,
                      prob = c(2/3, 1/3))

z <- rnorm(n, 0.5)
# create latent outcome variable
latenty <- -0.5 + 1.5 * x1 - 0.5 * x2 + 0.5 * z + rnorm(n, sd = exp(0.5 *
                                                                      x1 - 0.5 * x2))
# observed y has four possible values: -1,0,1,2
# threshold values are: -0.5, 0.5, 1.5.
y <- vector("numeric", n)
y[latenty < 0.5] <- 1
y[latenty >= 0.5 & latenty < 1.5] <- 1e3
y[latenty >= 1.5 & latenty < 2.5] <- 1e4
y[latenty >= 2.5] <- 1e5
dataset <- data.frame(y, x1, x2)


ordered_logit <- REtage::ordered_model_threshold(
  dataset,
  formula = "y ~ x1+x2",
  link = "logit",
  delta = 0,
  constantMEAN = FALSE,
  constantSD = FALSE,
  thresholds = NULL
)


# PREREQUISITE 2: LONGITUDINAL DATA


r <- .02
gamma <- .5
beta <- .99

df <- data.table::data.table(
  Id = 1L,
  x1 = sample(c(0, 1), 8011,
         replace = TRUE,
         prob = c(0.75, 0.25)),
  K2009 = 100L,
  timeIndex = c(rep(0,10), 1, rep(0,8000)),
  UC = 1L
)
df$x2 <- as.numeric(df$x1 == 0)

df[,'id_death' := 2L]
df[,'share' := 1]
df[,'annee' := seq_len(8011)]


# APPLY MODEL ---------------------


testthat::test_that("H_received consistent with value predicted in model", {
  testthat::expect_error(
    apply_inheritance_model(df, inheritance_model = ordered_logit),
    NA
  )
})

