context("fit_K0 internal produces expected same output than fit_K0")




# ---------------------------------
# TRAJECTOIRE INDIVIDUELLE
# ---------------------------------

z1 <- data.table::data.table(
  Id = 1L,
  annee = 2000:2059,
  age = 20:79,
  findet = 20
)

z1[,'salaire_tot' := lest::case_when(
  get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
  TRUE ~ 0
)]


z1[,'K2009' := 100L]
#z[get('annee') == 2009, 'K2009' := 100]

z1[,'sexe' := 1]

z1[,'UC' := 1]
z1[,'ageliq' := 60]
z1[,'tau' := seq_len(nrow(z1))-1]
z1[,'tt' := 0]
z1[get('annee') == 2009, 'tt' := 1]




z2 <- data.table::data.table(
  Id = 2L,
  annee = 2010:2069,
  age = 20:79,
  findet = 20
)

z2[,'salaire_tot' := lest::case_when(
  get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
  TRUE ~ 0
)]


z2[,'K2009' := NA_real_]
#z[get('annee') == 2009, 'K2009' := 100]

z2[,'sexe' := 1]

z2[,'UC' := 1]
z2[,'ageliq' := 60]
z2[,'tau' := seq_len(nrow(z2))-1]
z2[,'tt' := 0]

household_table <- data.table::copy(
  data.table::rbindlist(
    list(z1,z2),
    use.names = TRUE)
)

r <- .02
gamma <- .5
beta <- 0.98


# EQUIVALENT WITH OLD APPROACH ? --------------------------

household_table[,'H' := 0L]

output_old <- household_table[, fit_K0_old(
  income = get("salaire_tot"),
  K2009vector = get("K2009"),
  get("tau"),
  get("UC"),
  r = r,
  gamma = gamma,
  beta = beta), by = "Id"]



output_new <- household_table[, fit_K0(
  income = get("salaire_tot"),
  K2009vector = get("K2009"),
  get("tau"),
  get("UC"),
  inheritanceReceived = get('H'),
  inheritanceGiven = get('H'),
  r = r,
  gamma = gamma,
  beta = beta), by = "Id"]


data.table::setnames(output_old, 2:ncol(output_old), paste0(colnames(output_old)[2:ncol(output_old)],"_old"))
data.table::setnames(output_new, 2:ncol(output_new), paste0(colnames(output_new)[2:ncol(output_new)],"_new"))


output <- merge(output_old,output_new, by = "Id")

output[, 'K_0' := get('K0_new') -  get('K0_old')]
output[, 'sum_rY' := get('sum_rY_new') -  get('sum_rY_old')]
output[, 'sum_alpha' := get('sum_alpha_new') -  get('sum_alpha_old')]
output[, 'capitalizedBefore2009' := get('capitalizedBefore2009_new') -  get('capitalizedBefore2009_old')]
output[, 'capitalizedAfter' := get('capitalizedAfter_new') -  get('capitalizedAfter_old')]
output[, 'Rt' := get('Rt_new') -  get('Rt_old')]

output <- output[, .SD, .SDcols = c("Id","K_0", "sum_rY", "sum_alpha",
                                    "capitalizedBefore2009","capitalizedAfter",
                                    "Rt")]




test_that("K_0 equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['K_0']],
    c(0,NA)
  )
})

test_that("sum_rY equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['sum_rY']],
    c(0,0)
  )
})

test_that("sum_alpha equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['sum_alpha']],
    c(0,0)
  )
})

test_that("capitalizedBefore2009 equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['capitalizedBefore2009']],
    c(0,0)
  )
})

test_that("capitalizedAfter equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['capitalizedAfter']],
    c(0,0)
  )
})

test_that("Rt equal btw fit_K0 and fit_K0_old", {
  expect_equal(
    output[['Rt']],
    c(0,0)
  )
})


# NO NaN WITH SIGMA = 1 ? -----------------------


sigma_1 <- household_table[, .(fit_K0(
  income = get("salaire_tot"),
  K2009vector = get("K2009"),
  get("tau"),
  get("UC"),
  inheritanceReceived = 0L,
  inheritanceGiven = 0L,
  r = r,
  gamma = 1,
  beta = beta)), by = "Id"]

sigma_1 <- sigma_1[get('Id')==1]


test_that("Only real values returned for people observed before observation_year", {
  expect_equal(
    length(is.finite(as.numeric(sigma_1[Id == 1][['V1']]))),
    nrow(sigma_1)
  )
})




