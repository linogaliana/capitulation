context('life_cycle_model implements Modigliani-Friedman hypotheses as desired')


# -----------------------------------
# SCENARIO
# -----------------------------------

# 1 individual in our database:
#   [0-19]: does not work
#   [20-59]: work
#   [60-79]: does not work
#   79: dies


# ---------------------------------
# TRAJECTOIRE INDIVIDUELLE
# ---------------------------------

z <- data.table::data.table(
  Id = rep(1,60),
  annee = 2000:2059,
  age = 20:79,
  findet = 20
)

z[,'salaire_tot' := lest::case_when(
  get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
  TRUE ~ 0
)]


z[,'K2009' := 100L]
#z[get('annee') == 2009, 'K2009' := 100]

z[,'sexe' := 1]

z[,'UC' := 1]
z[,'ageliq' := 60]
z[,'tau' := seq_len(nrow(z))-1]
z[,'tt' := 0]
z[get('annee') == 2009, 'tt' := 1]

household_table <- data.table::copy(z)



r <- .02
gamma <- .5
beta <- 0.98

household_table[,c("Hg","Hr") := 0L]

household_table[, `:=`('Kt' = as.numeric(unlist(life_cycle_model_cpp(
  income = get("salaire_tot"),
  observed_wealth = get("K2009"),
  timeIndex = get("tt"),
  inheritanceGiven = get("Hg"),
  inheritanceReceived = get("Hr"),
  r = r,
  risk_aversion = gamma,
  discount_factor = beta)))
), by = 'Id']


# household_table[, .('Kt' = fit_K0(income = get("salaire_tot"),
#                                  findetVector = get("findet"),
#                                  K2009vector = get("K2009"),
#                                  timeIndex = get("tt"),
#                                  UC = get("UC"),
#                                  r = r,
#                                  gamma = gamma,
#                                  beta = beta)
# ), by = 'Id']


household_table2 <- household_table[, .('listval' = list(life_cycle_model_cpp_old(
  income = get("salaire_tot"),
  K2009vector = get("K2009"),
  timeIndex = get("tt"),
  gamma = gamma,
  UC = get("UC"),
  beta = beta, returnLast = TRUE))), by = 'Id']

household_table2[, `:=`('K0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["K0"]))),
                        'C0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["C0"]))))]

household_table2[,'listval' := NULL]
household_table <- merge(household_table,household_table2, by = 'Id')




# tempdf_RA_beta <- tempdf_RA_beta[get('annee')>=2009]


# PART I - CONSISTENCY WITH OTHER FUNCTIONS
# ------------------------------------------------


household_table2_old <- estimate_K0(data = z,
                                    r = r,
                                    gamma = gamma,
                                    beta = beta,
                                    wealthvar_survey = "K2009")



household_table2_old[, 'Kt' := simulate_wealth_structural_beta(K0 = get('K0'),
                                                               consumption0 = get('C0'),
                                                               income = get('salaire_tot'),
                                                               UC = get('UC'),
                                                               gamma = .5,
                                                               r = .02, beta = 0.98), by = c('Id')]



test_that("K0: Consistent with separate functions", {
  expect_equal(
    unique(household_table[['K0']]),
    unique(household_table2_old[['K0']])
  )
})

test_that("C0: Consistent with separate functions", {
  expect_equal(
    unique(household_table[['C0']]),
    unique(household_table2_old[['C0']])
  )
})

test_that("Kt: Consistent with separate functions", {
  expect_equal(
    household_table[['Kt']],
    household_table2_old[['Kt']]
  )
})



# PART II - CONSISTENCY WITH MODEL HYPOTHESES
# ------------------------------------------------


household_table[, `:=`('Kt' = as.numeric(unlist(life_cycle_model_cpp(income = get("salaire_tot"),
                                                                     observed_wealth = get("K2009"),
                                                                     timeIndex = get("tt"),
                                                                     inheritanceGiven = get("Hg"),
                                                                     inheritanceReceived = get("Hr"),
                                                                     r = r,
                                                                     risk_aversion  = gamma,
                                                                     discount_factor = beta)))
), by = 'Id']

household_table[, `:=`('KT' = as.numeric(unlist(life_cycle_model_cpp_old(income = get("salaire_tot"),
                                                                         K2009vector = get("K2009"),
                                                                         timeIndex = get("tt"),
                                                                         UC = get("UC"),
                                                                         r = r,
                                                                         gamma = gamma,
                                                                         beta = beta, returnLast = TRUE)['Kt']))[-1]
), by = 'Id']



test_that("Estimated value consistent with observed value for survey year", {
  expect_equal(
    household_table[annee==2009][['Kt']],
    as.numeric(
      unique(household_table[annee==2009][['K2009']]))
  )
})


test_that("Death with 0 wealth", {
  expect_equal(
    round(household_table[annee==max(annee)][['KT']]),
    0
  )
})


# -----------------------------------------------------------------------
# BEHAVIOR CONSISTENT WITH PEOPLE THAT START LIFE CYCLE AFTER 2009
# -----------------------------------------------------------------------

z <- data.table::data.table(
  Id = rep(1,60),
  annee = 2010:2069,
  age = 20:79,
  findet = 20
)

z[,'salaire_tot' := lest::case_when(
  get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
  TRUE ~ 0
)]


z[,'K2009_miss' := NA_real_]
z[,'K2009' := 0L]
#z[get('annee') == 2009, 'K2009' := 100]

z[,'sexe' := 1]

z[,'UC' := 1]
z[,'ageliq' := 60]
z[,'tau' := seq_len(nrow(z))-1]
z[,'tt' := 0]

household_table <- data.table::copy(z)

household_table[,c("Hg","Hr") := 0L]


r <- .02
gamma <- .5
beta <- 0.98

testthat::test_that("NA values in observed_wealth produce NA vector", {
  testthat::expect_equal(sum(
    is.na(household_table[, `:=`('Kt' = life_cycle_model_cpp(income = get("salaire_tot"),
                                                             observed_wealth = get("K2009_miss"),
                                                             timeIndex = get("tt"),
                                                             inheritanceGiven = get("Hg"),
                                                             inheritanceReceived = get("Hr"),
                                                             r = r,
                                                             risk_aversion = gamma,
                                                             discount_factor = beta)),
                          by = 'Id'][['Kt']])), nrow(household_table) 
  )
})


household_table[, `:=`('Kt' = life_cycle_model_cpp(income = get("salaire_tot"),
                                                   observed_wealth = get("K2009"),
                                                   timeIndex = get("tt"),
                                                   inheritanceGiven = get("Hg"),
                                                   inheritanceReceived = get("Hr"),
                                                   r = r,
                                                   risk_aversion = gamma,
                                                   discount_factor = beta)),
                by = 'Id']

household_table2 <- household_table[, .('listval' = list(life_cycle_model_cpp_old(income = get("salaire_tot"),
                                                                                  K2009vector = get("K2009"),
                                                                                  timeIndex = get("tt"),
                                                                                  UC = get("UC"),
                                                                                  r = r,
                                                                                  gamma = gamma,
                                                                                  beta = beta, returnLast = TRUE))),
                                    by = 'Id']

household_table2[, `:=`('K0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["K0"]))),
                        'C0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["C0"]))))]

household_table2[,'listval' := NULL]
household_table <- merge(household_table,household_table2, by = 'Id')



household_table[, 'Kt_old' := simulate_wealth_structural_beta(K0 = get('K0'),
                                                              consumption0 = get('C0'),
                                                              income = get('salaire_tot'),
                                                              UC = get('UC'),
                                                              gamma = .5,
                                                              r = .02, beta = 0.98), by = c('Id')]



test_that("K0: Consistent with separate functions", {
  expect_equal(
    household_table[['K0']],
    rep(0, length(household_table[['K0']]))
  )
})

test_that("Kt: Consistent with separate functions", {
  expect_equal(
    household_table[['Kt']],
    household_table[['Kt_old']]
  )
})


household_table[, `:=`('KT' = as.numeric(unlist(life_cycle_model_cpp_old(income = get("salaire_tot"),
                                                                         K2009vector = get("K2009"),
                                                                         timeIndex = get("tt"),
                                                                         UC = get("UC"),
                                                                         r = r,
                                                                         gamma = gamma,
                                                                         beta = beta, returnLast = TRUE)['Kt']))[-1]
), by = 'Id']


test_that("KT = 0", {
  expect_equal(
    round(household_table[get('annee')==max(get('annee'))][['KT']]),
    0
  )
})



household_table[, `:=`('KT_new' = capitulation:::life_cycle_model_cpp_bis(income = get("salaire_tot"),
                                                   observed_wealth = get("K2009"),
                                                   timeIndex = get("tt"),
                                                   inheritanceGiven = get("Hg"),
                                                   inheritanceReceived = get("Hr"),
                                                   return_last = TRUE,
                                                   r = r,
                                                   risk_aversion = gamma,
                                                   discount_factor = beta)[-1]),
                by = 'Id']


test_that("KT_new = 0", {
  expect_equal(
    round(household_table[get('annee')==max(get('annee'))][['KT_new']]),
    0
  )
})

# Model in log: log(eps) when K<0 ----



household_table[, `:=`('KT_new' = capitulation:::life_cycle_model_cpp(income = get("salaire_tot"),
                                                                          observed_wealth = get("K2009"),
                                                                          timeIndex = get("tt"),
                                                                          inheritanceGiven = get("Hg"),
                                                                          inheritanceReceived = get("Hr"),
                                                                          r = r,
                                                                          risk_aversion = gamma,
                                                                          discount_factor = beta)),
                by = 'Id']


household_table[, `:=`('KT_new_log' = capitulation:::life_cycle_model_cpp(income = get("salaire_tot"),
                                                                          observed_wealth = get("K2009"),
                                                                          timeIndex = get("tt"),
                                                                          inheritanceGiven = get("Hg"),
                                                                          inheritanceReceived = get("Hr"),
                                                                          scale = "log",
                                                                          r = r,
                                                                          risk_aversion = gamma,
                                                                          discount_factor = beta)),
                by = 'Id']


test_that("KT_new = 0", {
  expect_equal(
    household_table[get('KT_new') < 0][['KT_new_log']],
    rep(log(.Machine$double.eps), sum(household_table$KT_new<0))
  )
})



household_table[, `:=`('KT_new_T' = capitulation:::life_cycle_model_cpp_bis(income = get("salaire_tot"),
                                                                          observed_wealth = get("K2009"),
                                                                          timeIndex = get("tt"),
                                                                          inheritanceGiven = get("Hg"),
                                                                          inheritanceReceived = get("Hr"),
                                                                          return_last = TRUE,
                                                                          r = r,
                                                                          risk_aversion = gamma,
                                                                          discount_factor = beta)[-1]),
                by = 'Id']
household_table[, `:=`('KT_new_T_log' = capitulation:::life_cycle_model_cpp_bis(income = get("salaire_tot"),
                                                                            observed_wealth = get("K2009"),
                                                                            timeIndex = get("tt"),
                                                                            inheritanceGiven = get("Hg"),
                                                                            inheritanceReceived = get("Hr"),
                                                                            return_last = TRUE,
                                                                            scale = "log",
                                                                            r = r,
                                                                            risk_aversion = gamma,
                                                                            discount_factor = beta)[-1]),
                by = 'Id']


test_that("KT_new = 0", {
  expect_equal(
    round(household_table[get('annee')==max(get('annee'))][['KT_new_T']]),
    0
  )
})

test_that("KT_new_T_log = 0", {
  expect_equal(
    household_table[get('annee')==max(get('annee'))][['KT_new_T_log']],
    .Machine$double.eps,
    tolerance = 1e-1
  )
})

# avec heritage ------

household_table <- data.table::copy(z)

household_table[,`:=`(Hg = 100L, Hr = 0L)]

household_table[, `:=`('KT_new_T' = capitulation:::life_cycle_model_cpp_bis(income = get("salaire_tot"),
                                                                            observed_wealth = get("K2009"),
                                                                            timeIndex = get("tt"),
                                                                            inheritanceGiven = get("Hg"),
                                                                            inheritanceReceived = get("Hr"),
                                                                            return_last = TRUE,
                                                                            r = r,
                                                                            risk_aversion = gamma,
                                                                            discount_factor = beta)[-1]),
                by = 'Id']

test_that("KT_new_T = H", {
  expect_equal(
    household_table[get('annee')==max(get('annee'))][['KT_new_T']],
    household_table[get('annee')==max(get('annee'))][['Hg']],
    tolerance = 1e-2
  )
})


household_table[, `:=`('KT_new_T_log' = capitulation:::life_cycle_model_cpp_bis(income = get("salaire_tot"),
                                                                            observed_wealth = get("K2009"),
                                                                            timeIndex = get("tt"),
                                                                            inheritanceGiven = get("Hg"),
                                                                            inheritanceReceived = get("Hr"),
                                                                            return_last = TRUE,
                                                                            scale = "log",
                                                                            r = r,
                                                                            risk_aversion = gamma,
                                                                            discount_factor = beta)[-1]),
                by = 'Id']


test_that("KT_new_T = H", {
  expect_equal(
    household_table[get('annee')==max(get('annee'))][['KT_new_T_log']],
    log(household_table[get('annee')==max(get('annee'))][['Hg']]),
    tolerance = 1e-2
  )
})
