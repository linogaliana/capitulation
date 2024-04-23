testthat::context("Master function is well implemented")


# POPULATION ----------

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

z2[,'salaire_tot' :=  get('age')-20 + exp(rnorm(length(get('age')<60)))]


z2[,'K2009' := 0]
#z[get('annee') == 2009, 'K2009' := 100]

z2[,'sexe' := 1]

z2[,'UC' := 1]
z2[,'ageliq' := 60]
z2[,'tau' := seq_len(nrow(z2))-1]
z2[,'tt' := 0]

z2[get('annee') == 2030, 'tt' := 1]


household_table <- data.table::copy(
  data.table::rbindlist(
    list(z1,z2),
    use.names = TRUE)
)


r <- .02
gamma <- .5
beta <- 0.98


# MODEL 1: NO INHERITANCE ------




wealth_data <- life_cycle_model(data = data.table::copy(household_table),
                                r = r,
                                beta = beta,
                                gamma = gamma,
                                wealthvar_survey = "K2009",
                                observation_year = 2009,
                                income_var = "salaire_tot",
                                weight_var = NULL)

# wealth_data2 <- life_cycle_model_old(data = data.table::copy(household_table),
#                                 r = r,
#                                 beta = beta,
#                                 gamma = gamma,
#                                 wealthvar_survey = "K2009",
#                                 observation_year = 2009,
#                                 income_var = "salaire_tot",
#                                 weight_var = NULL)



test_that("Kt(2009) consistent with observed value", {
  expect_equal(
    wealth_data[get("tt") == 1L][['wealth']],
    wealth_data[get("tt") == 1L][['K2009']]
  )
})

# test_that("life_cycle_model and life_cycle_model_old yield same value", {
#   expect_equal(
#     wealth_data[['wealth']],
#     wealth_data2[['wealth']]
#   )
# })

wealth_data3 <- life_cycle_model(data = data.table::copy(household_table),
                                          r = r,
                                          beta = beta,
                                          gamma = gamma,
                                          wealthvar_survey = "K2009",
                                          observation_year = 2009,
                                          income_var = "salaire_tot",
                                          weight_var = NULL,
                                          return_last = TRUE)
wealth_data3[,'last_year' := (get('annee')==max(get('annee'))),
             by = 'Id']

test_that("With one more year simulated, people would end up with 0 wealth", {
  expect_equal(
    round(wealth_data3[(last_year)][['wealth']]),
    rep(0,2)
  )
})



# MODEL 2: INHERITANCE GIVEN BUT NOT RECEIVED ------

household_table2 <- data.table::copy(household_table)

household_table2[,"H_given" := get('Id')*1000] #Id_1 gives 1000 ; Id_2 gives 2000


wealth_data <- life_cycle_model(data = data.table::copy(household_table2),
                                r = r,
                                beta = beta,
                                gamma = gamma,
                                wealthvar_survey = "K2009",
                                observation_year = 2009,
                                income_var = "salaire_tot",
                                weight_var = NULL,
                                Hgiven_var = "H_given"
                                )



test_that("Kt(2009) consistent with observed value", {
  expect_equal(
    wealth_data[get("tt") == 1L][['wealth']],
    wealth_data[get("tt") == 1L][['K2009']]
  )
})



wealth_data3 <- life_cycle_model(data = data.table::copy(household_table2),
                                 r = r,
                                 beta = beta,
                                 gamma = gamma,
                                 wealthvar_survey = "K2009",
                                 observation_year = 2009,
                                 income_var = "salaire_tot",
                                 weight_var = NULL,
                                 Hgiven_var = "H_given",
                                 return_last = TRUE)

wealth_data3[,'last_year' := (get('annee')==max(get('annee'))),
             by = 'Id']


# TO DO: test K_T = H


test_that("Terminal wealth is equal to H_given", {
  expect_equal(
    round(wealth_data3[(last_year)][['wealth']]),
    round(wealth_data3[(last_year)][['Id']]*1000)
    )
})


# MODEL 3: INHERITANCE RECEIVED BUT NOT GIVEN ------


household_table2 <- data.table::copy(household_table)

household_table2[,"H_received" := (get('annee') == 2020)*1000] #Id_1 gives 1000 ; Id_2 gives 2000


wealth_data <- life_cycle_model(data = data.table::copy(household_table2),
                                r = r,
                                beta = beta,
                                gamma = gamma,
                                wealthvar_survey = "K2009",
                                observation_year = 2009,
                                income_var = "salaire_tot",
                                weight_var = NULL,
                                Hreceived_var = "H_received"
)

wealth_data3 <- life_cycle_model(data = data.table::copy(household_table2),
                                r = r,
                                beta = beta,
                                gamma = gamma,
                                wealthvar_survey = "K2009",
                                observation_year = 2009,
                                income_var = "salaire_tot",
                                weight_var = NULL,
                                Hreceived_var = "H_received",
                                return_last = TRUE
)
wealth_data3[,'last_year' := (get('annee')==max(get('annee'))),
             by = 'Id']


test_that("Kt(2009) consistent with observed value", {
  expect_equal(
    wealth_data[get("tt") == 1L][['wealth']],
    wealth_data[get("tt") == 1L][['K2009']]
  )
})


test_that("With one more year simulated, people would end up with 0 wealth", {
  expect_equal(
    round(wealth_data3[(last_year)][['wealth']]),
    rep(0,2)
  )
})


# MODEL 4: INHERITANCE RECEIVED AND GIVEN ------


household_table2 <- data.table::copy(household_table)

household_table2[,"H_given" := get('Id')*1000] #Id_1 gives 1000 ; Id_2 gives 2000
household_table2[,"H_received" := (get('annee') == 2020)*1000] #Id_1 gives 1000 ; Id_2 gives 2000



wealth_data3 <- life_cycle_model(data = data.table::copy(household_table2),
                                 r = r,
                                 beta = beta,
                                 gamma = gamma,
                                 wealthvar_survey = "K2009",
                                 observation_year = 2009,
                                 income_var = "salaire_tot",
                                 weight_var = NULL,
                                 Hreceived_var = "H_received",
                                 Hgiven_var = "H_given",
                                 return_last = TRUE
)

wealth_data3[,'last_year' := (get('annee')==max(get('annee'))),
             by = 'Id']


test_that("Terminal wealth is equal to H_given", {
  expect_equal(
    round(wealth_data3[(last_year)][['wealth']]),
    round(wealth_data3[(last_year)][['Id']]*1000)
  )
})


# LOG SCALE --------------------

wealth_data3_log <- life_cycle_model(data = data.table::copy(household_table2),
                                 r = r,
                                 beta = beta,
                                 gamma = gamma,
                                 wealthvar_survey = "K2009",
                                 observation_year = 2009,
                                 income_var = "salaire_tot",
                                 weight_var = NULL,
                                 Hreceived_var = "H_received",
                                 Hgiven_var = "H_given",
                                 scale_model = "log"
)

wealth_data3 <- life_cycle_model(data = data.table::copy(household_table2),
                                 r = r,
                                 beta = beta,
                                 gamma = gamma,
                                 wealthvar_survey = "K2009",
                                 observation_year = 2009,
                                 income_var = "salaire_tot",
                                 weight_var = NULL,
                                 Hreceived_var = "H_received",
                                 Hgiven_var = "H_given"
)

# series will start to diverge when w_simulated < 0 in level
first_zero = min(which(wealth_data3$wealth<0))


test_that("log scale", {
  expect_equal(
    exp(wealth_data3_log[1:(first_zero-1)]$wealth),
    wealth_data3[1:(first_zero-1)]$wealth
  )
})
