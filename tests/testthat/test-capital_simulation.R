context('wealth_accumulation consistently gathers microsimulation steps')

# CMD check
data_path <- "./../../inst/dataINSEE/Destinie"

# Local
# data_path = "./inst/dataINSEE/Destinie"


#     PART I: SIMPLE DYNAMICS
# -----------------------------------------


#     SCENARIO
# =================================

z1a <- data.frame(
  Id = 1L,
  sexe = 1L,
  annee = 2000:2025,
  conjoint = 2L,
  matri = 2L,
  findet = 20L,
  age = 20:(20 + 20 + 5),
  salaire = exp(rnorm(26)),
  pere = 0,
  referent = 1L,
  ageMaxPere = 0,
  mere = 0,
  ageMaxMere = 0,
  neFrance = 1,
  ageliq = 35
)

z1b <- data.frame(
  Id = 2L,
  sexe = 2L,
  annee = 2000:2025,
  conjoint = 1L,
  matri = 2L,
  findet = 20L,
  age = 20:(20 + 20 + 5),
  salaire = exp(rnorm(26L)),
  referent = 1L,
  pere = 0,
  ageMaxPere = 0,
  mere = 0,
  ageMaxMere = 0,
  neFrance = 1,
  ageliq = 35
)

z1 <- data.table::data.table(rbind(z1a,z1b))
z1 <- capitulation::income_household(data.table::data.table(z1))

z1[,'UC' := 1.5]
z1[,'Nspouses' := 2L]
z1[,'K2009' := 100L]

z1[,'y_real' := get('salaire_tot')/runif(2)]


# PART I: CONSISTENCY WITH LIFE-CYCLE HYPOTHESES
# ----------------------------------------------------


# simulated_survey <- data.table::data.table(
#   data.frame(
#     'Id' = c(1,2),
#     'wealth' = 100L,
#     'annee' = 2009
#   )
# )
# 
# data.table::fwrite(simulated_survey, file = "fake.csv")

z1_output  <- capital_simulation(z1,
                               start_year = 2009,
                               estimation_year = 2009,
                               final_year = 2070,
                               id_var = "Id",
                               income_var = "salaire",
                               age_var = "age",
                               ageliq_var = "ageliq",
                               matrimonial_var = "matri",
                               findet_var = "findet",
                               wealthvar_survey = "K2009",
                               deflate = TRUE,
                               r = 0.03,
                               gamma = 0.5,
                               beta = 0.98,
                               verbose = TRUE,
                               weight_var = "Nspouses")



test_that("Wealth observed at household level consistent btw simulated and observed",
          expect_equal(
            sum(z1_output[get('annee')==2009][['wealth']]),
            unique(z1[['K2009']])
          )
)


z1_output  <- capital_simulation(z1,
                                 start_year = 2009,
                                 estimation_year = 2009,
                                 final_year = 2070,
                                 id_var = "Id",
                                 income_var = "salaire",
                                 age_var = "age",
                                 ageliq_var = "ageliq",
                                 matrimonial_var = "matri",
                                 findet_var = "findet",
                                 wealthvar_survey = "K2009",
                                 deflate = TRUE,
                                 r = 0.03,
                                 gamma = 0.5,
                                 beta = 0.98,
                                 verbose = TRUE,
                                 weight_var = "Nspouses",
                                 return_last = TRUE)



test_that("Wealth observed at household level consistent btw simulated and observed",
          expect_equal(
            z1_output[get('annee')==max(get('annee'))][['wealth']],
            rep(0,2)
          )
)



# PART II: CONSISTENCY WHEN USING CONSUMPTION UNITS
# ----------------------------------------------------

z1_output  <- capital_simulation(z1,
                                 start_year = 2009,
                                 estimation_year = 2009,
                                 final_year = 2070,
                                 id_var = "Id",
                                 income_var = "salaire",
                                 age_var = "age",
                                 ageliq_var = "ageliq",
                                 matrimonial_var = "matri",
                                 findet_var = "findet",
                                 wealthvar_survey = "K2009",
                                 deflate = TRUE,
                                 r = 0.03,
                                 gamma = 0.5,
                                 beta = 0.98,
                                 verbose = TRUE,
                                 weight_var = "UC")



test_that("Wealth observed at household level consistent btw simulated and observed",
          expect_equal(
            sum(z1_output[get('annee')==2009][['wealth']]),
            2/1.5*unique(z1[['K2009']])
          )
)


z1_output  <- capital_simulation(z1,
                                 start_year = 2009,
                                 estimation_year = 2009,
                                 final_year = 2070,
                                 id_var = "Id",
                                 income_var = "salaire",
                                 age_var = "age",
                                 ageliq_var = "ageliq",
                                 matrimonial_var = "matri",
                                 findet_var = "findet",
                                 wealthvar_survey = "K2009",
                                 deflate = TRUE,
                                 r = 0.03,
                                 gamma = 0.5,
                                 beta = 0.98,
                                 verbose = TRUE,
                                 weight_var = "UC",
                                 return_last = TRUE)



test_that("Wealth observed at household level consistent btw simulated and observed",
          expect_equal(
            z1_output[get('annee')==max(get('annee'))][['wealth']],
            rep(0,2)
          )
)
