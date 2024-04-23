context('wealth_accumulation consistently gathers microsimulation steps')

# CMD check
data_path <- "./../../inst/dataINSEE/Destinie"

# Local
# data_path = "./inst/dataINSEE/Destinie"


#     PART I: SIMPLE DYNAMICS
# -----------------------------------------


#     SCENARIO
# =================================

# 1 individual in our database:
#   [0-19]: does not work
#   [20-59]: work
#   [60-79]: does not work
#   79: dies


# TRAJECTOIRE INDIVIDUELLE
# =================================

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


z[,'K2009' := lest::case_when(
  get('annee') == 2009 ~ 100,
  TRUE ~ NA_real_
)]

z[,'sexe' := 1]

z[,'UC' := 1]
z[,'ageliq' := 60]

household_table <- data.table::copy(z)

household_table[,'dernier_changement' := 2009L]

household_table[, c('id_conjoint','matri') := 2]


# ESTIMATE K0
# =================================

r <- .02
gamma <- .5
beta <- .98

# CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
tempdf_RA_beta <- estimate_K0(data = household_table,
                              r = r,
                              gamma = gamma,
                              beta = beta,
                              wealthvar_survey = "K2009")


tempdf_RA_beta[,c('wealth','patrimoine') := get('K0')]


tempdf_RA_beta2 <- data.table::copy(tempdf_RA_beta)
tempdf_RA_beta2[,'tau' := get('age') - get('findet') ]


# TEST 1
# =================================

# simulate_wealth_structural_beta and wealth_accumulation produce same dynamics
# in a simple trajectory


tempdf_RA_beta2[, `:=` ('K' = simulate_wealth_structural_beta(K0 = get('K0'),
                                                              consumption0 = get('C0'),
                                                              income = get('salaire_tot'),
                                                              UC = get('UC'),
                                                              gamma = gamma,
                                                              r = r,
                                                              beta = beta,
                                                              returnLast = FALSE),
                        'K2' = simulate_wealth_structural_beta2(K0 = get('K0'),
                                                                consumption0 = get('C0'),
                                                                income = get('salaire_tot'),
                                                                UC = get('UC'),
                                                                tau = get('tau'),
                                                                gamma = gamma,
                                                                r = r,
                                                                beta = beta,
                                                                returnLast = FALSE)
), by = c('Id')]

wealth_dynamic_full <- wealth_accumulation(
  household_table = tempdf_RA_beta2,
  years = 2009:max(tempdf_RA_beta2$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009"
)


test_that(
  paste0("wealth_accumulation produces sequence consistent",
         "with both simulate_wealth_structural_beta",
         "and simulate_wealth_structural_beta2 in simple cases"),
  {
    expect_equal(
      tempdf_RA_beta2[get('annee')>=2009][['K']],
      tempdf_RA_beta2[get('annee')>=2009][['K2']]
    )
    expect_equal(
      tempdf_RA_beta2[get('annee')>=2009][['K']],
      wealth_dynamic_full[get('annee')>=2009][['wealth']]
    )
    expect_equal(
      tempdf_RA_beta2[get('annee')>=2009][['K2']],
      wealth_dynamic_full[get('annee')>=2009][['wealth']]
    )
  }
)


test_that(
  paste0("C0 after wealth_accumulation",
         " is the same as estimate_K0 output"),
  {
    expect_equal(
      unique(tempdf_RA_beta2[['C0']]),
      unique(wealth_dynamic_full[['C0']])
    )
  }
)

test_that(
  paste0("K0 after wealth_accumulation",
         " is the same as estimate_K0 output"),
  {
    expect_equal(
      unique(tempdf_RA_beta2[['K0']]),
      unique(wealth_dynamic_full[['K0']])
    )
  }
)



# TEST 2
# =================================
# wealth_accumulation gives same result for different years


tempdf_RA_beta3 <- data.table::copy(tempdf_RA_beta)

wealth_dynamic_more <- wealth_accumulation(
  tempdf_RA_beta3,
  years = min(tempdf_RA_beta$annee):max(tempdf_RA_beta$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009"
)



test_that("A sequence of years longer than necessary does not change output for common years", {
  expect_equal(
    wealth_dynamic_full[get('annee')>=2009][['wealth']],
    wealth_dynamic_more[get('annee')>=2009][['wealth']]
  )
})


tempdf_RA_beta4 <- data.table::copy(tempdf_RA_beta)

wealth_dynamic_restricted <- wealth_accumulation(
  tempdf_RA_beta4,
  years = 2009:2012,
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009"
)

test_that("A shorter sequence of years does not change output for common years", {
  expect_equal(
    wealth_dynamic_full[get('annee') <= 2012][['wealth']],
    wealth_dynamic_restricted[['wealth']]
  )
})

# TEST 3
# =================================

# Ends with zero wealth

KT <- wealth_dynamic_full[get('annee') == max(get('annee'))]
KT[,'KT' := (1+r)*(get('wealth')-get('C0')*(beta*(1+r))^((1/gamma)*(get('tau')+1)))]


test_that("Simple dynamics: individual die without wealth", {
  expect_equal(
    round(KT$KT),
    0
  )
})


#     PART II: MORE COMPLEX DYNAMICS
#       DIVORCE
# -----------------------------------------



#     SCENARIO
# =================================

# Individual 1 in our database:
#   [0-19]: does not work
#   [20-59]: work
#   [60-79]: does not work
#   79: dies

# Individual 2 in our database:
#   [0-19]: does not work
#   [2000-2019]: married with 3
#   [2020-2024]: alone
#   2025: dies

# Individual 3 in our database:
#   [*-2019]: married with 3
#   [2020-2030]: alone
#   2025: dies


# TRAJECTOIRE INDIVIDUELLE
# =================================

# indiv1: same as before
# indiv2 and 3: together up to 2020, id2 dies in 2025, id3 dies in 2030

z1a <- data.frame(
  Id = 2,
  sexe = 1,
  annee = 2000:2025,
  conjoint = c(rep(3,20L),rep(NA,6L)),
  matri = c(rep(2,20L),rep(4,6L)),
  referent = 2,
  findet = 20,
  age = 20:(20 + 20 + 5),
  salaire = exp(rnorm(26)),
  pere = 0,
  ageMaxPere = 0,
  mere = 0,
  ageMaxMere = 0,
  neFrance = 1,
  ageliq = 35
)

z1b <- data.frame(
  Id = 3,
  sexe = 2,
  annee = 2000:2030,
  conjoint = c(rep(2,20L),rep(NA,11L)),
  matri = c(rep(2,20L),rep(4,11L)),
  referent = c(rep(2,20L),rep(3,11L)),
  findet = 20,
  age = 20:(20 + 20 + 10),
  salaire = exp(rnorm(31L)),
  pere = 0,
  ageMaxPere = 0,
  mere = 0,
  ageMaxMere = 0,
  neFrance = 1,
  ageliq = 35
)

z1 <- data.table::data.table(rbind(z1a,z1b))


z1_household <- capitulation::income_household(table_indiv = data.table::data.table(z1))


# A few checks to ensure household dynamics are consistent

test_that("Individual trajectories are consistently transformed into household trajectory", {
  expect_equal(z1_household[['Id']], z1[['Id']])
})


test_that("Incomes are grouped together when people are in couple", {
  expect_equal(
    z1_household[get('annee')<2020 & Id==2][['salaire_tot']],z1_household[get('annee')<2020 & Id==3][['salaire_tot']]
  )
  expect_equal(
    z1_household[get('annee')<2020 & Id==2][['salaire']],z1[get('annee')<2020 & Id==2][['salaire']]
  )
  expect_equal(
    z1_household[get('annee')<2020 & Id==2][['salaire_conjoint']],z1[get('annee')<2020 & Id==3][['salaire']]
  )
  expect_equal(
    z1_household[get('annee')<2020 & Id==3][['salaire']],z1[get('annee')<2020 & Id==3][['salaire']]
  )
  expect_equal(
    z1_household[get('annee')<2020 & Id==3][['salaire_conjoint']],z1[get('annee')<2020 & Id==2][['salaire']]
  )
})


test_that("Incomes are individual level after divorce", {
  expect_equal(
    z1_household[get('annee')>=2020 & Id==2][['salaire_tot']],z1[get('annee')>=2020 & Id==2][['salaire']]
  )
  expect_equal(
    z1_household[get('annee')>=2020 & Id==3][['salaire_tot']],z1[get('annee')>=2020 & Id==3][['salaire']]
  )
  expect_equal(
    z1_household[get('annee')>=2020 & Id==2][['salaire_conjoint']],
    rep(0L, nrow(z1_household[get('annee')>=2020 & Id==2]))
  )
  expect_equal(
    z1_household[get('annee')>=2020 & Id==3][['salaire_conjoint']],
    rep(0L, nrow(z1_household[get('annee')>=2020 & Id==3]))
  )
})



# ESTIMATE K0
# =========================

# Assuming observed wealth is 100 euros
z1_household[,'K2009' := 100]
z1_household[,'UC' := 1]
menages <- data.table::copy(z1_household)


macro <- data.table::data.table(
  data.frame("annee" = 1900:2100,
             "Prix" = seq(0.5,1.5, length.out = length(1900:2100)))
)





# CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
tempdf_RA_beta <- estimate_K0(data = menages,
                              r = r,
                              gamma = gamma,
                              beta = beta,
                              wealthvar_survey = "K2009")



tempdf_RA_beta[,c('wealth','patrimoine') := get('K0')]

tempdf_RA_beta[,'tau' := get('age') - get('findet') ]
tempdf_RA_beta2 <- data.table::copy(tempdf_RA_beta)


# TEST 1
# =================================

# simulate_wealth_structural_beta and wealth_accumulation produce same dynamics
# in this more complex trajectory


tempdf_RA_beta2[, `:=` ('K' = simulate_wealth_structural_beta(K0 = get('K0'),
                                                              consumption0 = get('C0'),
                                                              income = get('salaire_tot'),
                                                              UC = get('UC'),
                                                              gamma = gamma,
                                                              r = r,
                                                              beta = beta,
                                                              returnLast = TRUE)[-1],
                        'K2' = simulate_wealth_structural_beta2(K0 = get('K0'),
                                                                consumption0 = get('C0'),
                                                                income = get('salaire_tot'),
                                                                UC = get('UC'),
                                                                tau = get('tau'),
                                                                gamma = gamma,
                                                                r = r,
                                                                beta = beta,
                                                                returnLast = TRUE)[-1]
), by = c('Id')]



test_that(
  paste0("simulate_wealth_structural_beta",
         "and simulate_wealth_structural_beta2 produce almost",
         "equal wealth series [enable difference from numeric errors compositions]"),
  {
    expect_lt(
      abs(
        min(tempdf_RA_beta2[get('annee')>=2009][['K']] - tempdf_RA_beta2[get('annee')>=2009][['K2']])
      ),
      10L
    )
    expect_lt(
      abs(
        max(tempdf_RA_beta2[get('annee')>=2009][['K']] - tempdf_RA_beta2[get('annee')>=2009][['K2']])
      ),
      10L
    )
  }
)


# TEST 2
# =================================

tempdf_RA_beta[,'tau' := get('age') - get('findet') ]
tempdf_RA_beta2 <- data.table::copy(tempdf_RA_beta)


# 2/ WEALTH DYNAMICS IS CONSISTENT WITH MODEL HYPOTHESES (0 WEALTH IN THE END)

wealth_dynamic_full <- wealth_accumulation(
  household_table = tempdf_RA_beta2,
  years = 2009:max(tempdf_RA_beta2$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  return_last = TRUE
)

KT <- wealth_dynamic_full[wealth_dynamic_full[, .I[get('annee') == max(get('annee'))], by='Id']$V1]


# test_that("life ends with 0 wealth",
#           expect_equal(
#             round(
#               KT$KT
#               ),
#             rep(0,2)
#           )
# )



#     PART III: PEOPLE THAT COME AFTER 2009
# -----------------------------------------------

#     SCENARIO
# =================================


# Individual 1 in our database:
#   [0-19]: does not work
#   [20-59]: work
#   [60-79]: does not work
#   79: dies

# Individual 2 in our database:
#   [0-19]: does not work
#   [2000-2019]: married with 3
#   [2020-2024]: alone
#   2025: dies

# Individual 3 in our database:
#   [*-2019]: married with 3
#   [2020-2030]: alone
#   2025: dies

# Individual 4 in our database:
#   [*-2014]: children of 2 and 3
#   [2015-2040]: alone
#   2040: dies

z4 <- data.frame(
  Id = 4,
  sexe = 1,
  annee = 2015:2040,
  conjoint = NA,
  matri = 1,
  referent = rep(4,26L),
  findet = 20,
  age = 20:(20 + 25),
  salaire = exp(rnorm(26L)),
  pere = 0,
  ageMaxPere = 0,
  mere = 0,
  ageMaxMere = 0,
  neFrance = 1,
  ageliq = 35
)

z1 <- data.table::data.table(rbind(z1a,z1b, z4))


z1_household <- capitulation::income_household(table_indiv = data.table::data.table(z1))


# Assuming observed wealth is 100 euros
z1_household[,'K2009' := 100]
z1_household[get('Id')==4, 'K2009' := NA_real_]

z1_household[,'UC' := 1]



menages <- data.table::copy(z1_household)


tempdf_RA_beta <- estimate_K0(data = menages,
                              r = r,
                              gamma = gamma,
                              beta = beta,
                              wealthvar_survey = "K2009",
                              weight_var = NULL)



tempdf_RA_beta[,c('wealth','patrimoine') := get('K0')]

tempdf_RA_beta[,'tau' := get('age') - get('findet') ]
tempdf_RA_beta2 <- data.table::copy(tempdf_RA_beta)


test_that("people not observed in 2009 start with 0 wealth",
          expect_equal(
            tempdf_RA_beta2[get('Id')==4][['wealth']],
            rep(0, length(tempdf_RA_beta2[get('Id')==4][['wealth']]))
          )
)





wealth_dynamic_full <- wealth_accumulation(
  household_table = tempdf_RA_beta2,
  years = 2009:max(tempdf_RA_beta2$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  return_split = FALSE
)


data.table::setkey(wealth_dynamic_full,NULL)
data.table::setkey(menages,NULL)

test_that("we do not lose any observation from household trajectory",
          expect_equal(
            menages[get('annee')>=2009 & Id==2, .SD, .SDcols = c('Id','annee')][order(get('Id'),get('annee'))],
            wealth_dynamic_full[Id==2, .SD, .SDcols = c('Id','annee')][order(get('Id'),get('annee'))]
          )
)



#     PART IV: NEW FUNCTION CONSISTENT WITH OLD ONE
# ------------------------------------------------------
# Check consistency with the old version in simple dynamics
# Husband and wife start together to work and die together

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

z1_new <- capitulation::income_household(data.table::data.table(z1))
z1_old <- capitulation::transform_household(data.table::data.table(z1))

# Assuming observed wealth is 100 euros
z1_new[,'K2009' := 100L]
z1_new[,'Nspouses' := 2L]
z1_old[,'K2009' := 100L]




z1_new <- estimate_K0(data = z1_new,
                      r = r,
                      gamma = gamma,
                      beta = beta,
                      wealthvar_survey = "K2009",
                      weight_var = "Nspouses")

z1_old <- estimate_K0(data = z1_old,
                      r = r,
                      gamma = gamma,
                      beta = beta,
                      wealthvar_survey = "K2009")

z1_new[,'tau' := get('age') - get('findet') ]
z1_old[,'tau' := get('age') - get('findet') ]

wealth_dynamic_new <- wealth_accumulation(
  household_table = z1_new,
  years = 2009:max(z1_new$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  weight_var = "Nspouses"
)


wealth_dynamic_old <- wealth_accumulation(
  household_table = z1_old,
  years = 2009:max(z1_new$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009"
)



test_that("K0 consistent at household level in both versions",
          expect_equal(
            sum(z1_new[annee == 2000L][["K0"]]),
            z1_old[annee == 2000L][["K0"]]
          )
)

test_that("C0 consistent at household level in both versions",
          expect_equal(
            sum(z1_new[annee == 2000L][["C0"]]),
            z1_old[annee == 2000L][["C0"]]
          )
)

test_that("Kt consistent at household level in both versions",
          expect_equal(
            wealth_dynamic_new[,.('wealth' = sum(get('wealth'))),
                               by = 'annee'][,.SD,
                                             .SDcols = c('annee','wealth')],
            wealth_dynamic_old[,.SD,
                               .SDcols = c('annee','wealth')]
          )
)




#     PART V: INDIVIDUAL LEVEL IS CONSISTENT GIVEN DIFFERENT WEIGHTS VECTOR
# ---------------------------------------------------------------------------

# There should be a difference of scale in UC and nbpers




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


# TEST 1: CHECK CONSISTENCY AT INDIVIDUAL LEVEL
# ==================================================

z1_indiv <- estimate_K0(data = z1,
                        r = r,
                        gamma = gamma,
                        beta = beta,
                        wealthvar_survey = "K2009",
                        weight_var = "Nspouses")

z1_UC <- estimate_K0(data = z1,
                     r = r,
                     gamma = gamma,
                     beta = beta,
                     wealthvar_survey = "K2009",
                     weight_var = "UC")


test_that("K0 consistent at individual level in both versions [up to normalizing factor]",
          expect_equal(
            z1_indiv[['K0']],
            1.5/2*z1_UC[['K0']]
          )
)

test_that("C0 consistent at individual level in both versions [up to normalizing factor]",
          expect_equal(
            z1_indiv[['C0']],
            1.5/2*z1_UC[['C0']]
          )
)


wealth_dynamic_indiv <- wealth_accumulation(
  household_table = z1_indiv,
  years = 2009:max(z1_indiv$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  weight_var = "Nspouses")

wealth_dynamic_UC <- wealth_accumulation(
  household_table = z1_UC,
  years = 2009:max(z1_UC$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  weight_var = "UC"
)

test_that("Kt consistent at individual level in both versions [up to normalizing factor]",
          expect_equal(
            wealth_dynamic_indiv[['wealth']],
            1.5/2*wealth_dynamic_UC[['wealth']]
          )
)



# TEST 2: CHECK CONSISTENCY WITH OBSERVATION
# ==================================================

test_that("wealth simulated at household level consistent with observation",
          {
            expect_equal(
              sum(wealth_dynamic_indiv[get('annee')==2009][['wealth']]),
              unique(z1[['K2009']])
            )
            expect_equal(
              sum(wealth_dynamic_UC[get('annee')==2009][['wealth']]),
              2/1.5*unique(z1[['K2009']])
            )
          }
)


# TEST 3: PEOPLE DIE WITH ZERO WEALTH
# ==================================================


wealth_dynamic_indiv <- wealth_accumulation(
  household_table = z1_indiv,
  years = 2009:max(z1_indiv$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  weight_var = "Nspouses",
  return_last = TRUE
)

wealth_dynamic_UC <- wealth_accumulation(
  household_table = z1_UC,
  years = 2009:max(z1_UC$annee),
  start_year = 2009,
  r = r,
  gamma = gamma,
  beta = beta,
  verbose = FALSE,
  age_var = "age",
  findet_var = "findet",
  income_var = "salaire_tot",
  wealthvar_survey = "K2009",
  weight_var = "UC",
  return_last = TRUE
)

test_that("People end life with 0 wealth",
          {
            expect_equal(
              round(wealth_dynamic_indiv[get('annee')==max(get('annee'))][['wealth']]),
              rep(0,2)
            )
            expect_equal(
              round(wealth_dynamic_UC[get('annee')==max(get('annee'))][['wealth']]),
              rep(0,2)
            )
          }
)
