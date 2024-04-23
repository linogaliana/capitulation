context("Life cycle model implementation with C++ should be consistent with hypotheses")

# life_cycle_model_old should be used if you want t=0...T sequence
# T being date of death

r <- .02
gamma <- .5
beta <- .99


# TOY EXAMPLE -------


# df <- data.table::data.table(
#   Id = 1L,
#   w = rep(10, 20),
#   K2009 = 100L,
#   timeIndex = c(rep(0,9), 1, rep(0,10)),
#   UC = 1L
# )
# 
# df[,'H' := 0L]
# df[,'r' := r]
# 
# df[, 'wealth' := capitulation::life_cycle_model_cpp(income = get("w"),
#                                                     observed_wealth = get("K2009"),
#                                                     timeIndex = get("timeIndex"),
#                                                     inheritanceReceived = get('H'),
#                                                     inheritanceGiven = get('H'),
#                                                     r = get('r'),
#                                                     risk_aversion = gamma,
#                                                     discount_factor = beta)]

# CHECK CONSISTENCY OF Kt SEQUENCE WITH ASUMPTIONS -----------------------------

df <- data.table::data.table(
  Id = 1L,
  w = abs(rnorm(8011)),
  K2009 = 100L,
  timeIndex = c(rep(0,10), 1, rep(0,8000)),
  UC = 1L
)
df[,'H' := 0L]
df[,'r' := r]

df[, 'wealth' := capitulation::life_cycle_model_cpp(income = get("w"),
                                                     observed_wealth = get("K2009"),
                                                     timeIndex = get("timeIndex"),
                                                     inheritanceReceived = get('H'),
                                                     inheritanceGiven = get('H'),
                                                     r = get('r'),
                                                    risk_aversion = gamma,
                                                    discount_factor = beta)]



K0_expected <- capitulation::fit_K0(income = df$w,
                                    K2009vector = df$K2009,
                                    timeIndex = df$timeIndex,
                                    UC = df$UC,
                                    inheritanceReceived = df$H,
                                    inheritanceGiven = df$H,
                                    r = r,
                                    gamma = gamma,
                                    beta = beta)$K0

testthat::test_that(
  "K0 corresponds to the value expected",
  testthat::expect_equal(df[1]$wealth, K0_expected)
)

testthat::test_that(
  "K_t corresponds to the value observed when t=2009",
  testthat::expect_equal(df[which.max(timeIndex)]$wealth, df[which.max(timeIndex)]$K2009)
)



# CHECK CONSISTENCY OF Kt SEQUENCE WITH MORE COMPLETE IMPLEMENTATION ---------------------------

interest <- r

df[, 'wealth2' := capitulation::life_cycle_model_cpp2_old(income = get("w"),
                                                      K2009vector = get("K2009"),
                                                      timeIndex = get("timeIndex"),
                                                      UC = get('UC'),
                                                      inheritanceReceived = get('H'),
                                                      inheritanceGiven = get('H'),
                                                      r = interest,
                                                      gamma = gamma,
                                                      beta = beta)]


df[, 'wealth3' := capitulation::life_cycle_model_cpp(income = get("w"),
                                                     observed_wealth = get("K2009"),
                                                     timeIndex = get("timeIndex"),
                                                     inheritanceReceived = get('H'),
                                                     inheritanceGiven = get('H'),
                                                     r = interest,
                                                     risk_aversion = gamma,
                                                     discount_factor = beta)]

testthat::test_that(
  "Implementation is consistent btw life_cycle_model_cpp and life_cycle_model_cpp2",
  testthat::expect_equal(df$wealth, df$wealth2)
)

testthat::test_that(
  "Implementation is consistent btw life_cycle_model_cpp and life_cycle_model_cpp",
  testthat::expect_equal(df$wealth, df$wealth3)
)



# TEST OPTION LOG -----------------------


df[, 'lwealth' := capitulation::life_cycle_model_cpp(income = get("w"),
                                                    observed_wealth = get("K2009"),
                                                    timeIndex = get("timeIndex"),
                                                    inheritanceReceived = get('H'),
                                                    inheritanceGiven = get('H'),
                                                    r = get('r'),
                                                    risk_aversion = gamma,
                                                    discount_factor = beta,
                                                    scale_model = "log")]

summary(df$lwealth)


# CHECK FASTER IMPLEMENTATION THAN life_cycle_model_cpp2 -------------------------

# microbenchmark::microbenchmark(
#   df[, 'wealth1' := capitulation::life_cycle_model_cpp2_old(income = get("w"),
#                                                         K2009vector = get("K2009"),
#                                                         timeIndex = get("timeIndex"),
#                                                         UC = get("UC"),
#                                                         r = r,
#                                                         gamma = gamma,
#                                                         beta = beta, returnLast = FALSE)],
#   df[, 'wealth2' := life_cycle_model_cpp(income = get("w"),
#                                           K2009vector = get("K2009"),
#                                           timeIndex = get("timeIndex"),
#                                           r = r,
#                                           gamma = gamma,
#                                           beta = beta)],
#   times = 50L
# )




