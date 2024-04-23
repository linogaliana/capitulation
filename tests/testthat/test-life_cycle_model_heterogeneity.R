# context('life_cycle_model implements Modigliani-Friedman in a heterogeneous context')
# 
# 
# # -----------------------------------
# # SCENARIO
# # -----------------------------------
# 
# # 1 individual in our database:
# #   [0-19]: does not work
# #   [20-59]: work
# #   [60-79]: does not work
# #   79: dies
# 
# 
# # ---------------------------------
# # TRAJECTOIRE INDIVIDUELLE
# # ---------------------------------
# 
# z1 <- data.table::data.table(
#   Id = 1L,
#   annee = 2000:2059,
#   age = 20:79,
#   findet = 20
# )
# 
# z1[,'salaire_tot' := lest::case_when(
#   get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
#   TRUE ~ 0
# )]
# 
# 
# z1[,'K2009' := 100L]
# #z[get('annee') == 2009, 'K2009' := 100]
# 
# z1[,'sexe' := 1]
# 
# z1[,'UC' := 1]
# z1[,'ageliq' := 60]
# z1[,'tau' := seq_len(nrow(z1))-1]
# z1[,'tt' := 0]
# z1[get('annee') == 2009, 'tt' := 1]
# 
# 
# 
# 
# z2 <- data.table::data.table(
#   Id = 2L,
#   annee = 2010:2069,
#   age = 20:79,
#   findet = 20
# )
# 
# z2[,'salaire_tot' := lest::case_when(
#   get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
#   TRUE ~ 0
# )]
# 
# 
# z2[,'K2009' := NA_real_]
# #z[get('annee') == 2009, 'K2009' := 100]
# 
# z2[,'sexe' := 1]
# 
# z2[,'UC' := 1]
# z2[,'ageliq' := 60]
# z2[,'tau' := seq_len(nrow(z2))-1]
# z2[,'tt' := 0]
# 
# household_table <- data.table::copy(
#   data.table::rbindlist(
#     list(z1,z2),
#     use.names = TRUE)
# )
# 
# 
# 
# r <- .02
# gamma <- .5
# beta <- 0.98
# 
# # Id==1 is low r ; Id==2 is high r
# household_table[, 'income_group' := Id - 1]
# 
# 
# # ---------------------------------------------
# # WHEN r_low and r_high ARE NOT PROVIDED
# 
# 
# test_that("Message when r_high is missing", {
#   expect_message(
#     capitulation::life_cycle_model_heterogeneity(household_table,
#                                                  r = r,
#                                                  r_low = 0.01,
#                                                  beta = beta,
#                                                  gamma = gamma,
#                                                  wealthvar_survey = "K2009",
#                                                  observation_year = 2009,
#                                                  income_var = "salaire_tot",
#                                                  weight_var = NULL,
#                                                  group_var = "income_group"),
#     "r_high is NULL"
#   )
# })
# 
# 
# test_that("Message when r_high is missing", {
#   expect_message(
#     capitulation::life_cycle_model_heterogeneity(household_table,
#                                                  r = r,
#                                                  r_high = 0.04,
#                                                  beta = beta,
#                                                  gamma = gamma,
#                                                  wealthvar_survey = "K2009",
#                                                  observation_year = 2009,
#                                                  income_var = "salaire_tot",
#                                                  weight_var = NULL,
#                                                  group_var = "income_group"),
#     "r_low is NULL"
#   )
# })
# 
# 
# wealth_data <- capitulation::life_cycle_model_heterogeneity(household_table,
#                                 r = r,
#                                 beta = beta,
#                                 gamma = gamma,
#                                 wealthvar_survey = "K2009",
#                                 observation_year = 2009,
#                                 income_var = "salaire_tot",
#                                 weight_var = NULL,
#                                 group_var = "income_group",
#                                 output_var = "wealth_heterogeneity")
# 
# 
# wealth_data2 <- capitulation:::life_cycle_model_complete(household_table,
#                                               r = r,
#                                               beta = beta,
#                                               gamma = gamma,
#                                               wealthvar_survey = "K2009",
#                                               observation_year = 2009,
#                                               income_var = "salaire_tot",
#                                               weight_var = NULL,
#                                               output_var = "wealth_homogeneity")
# 
# 
# output <- merge(wealth_data,wealth_data2)
# 
# 
# 
# test_that("homogeneous (old version) and heterogeneous model give same solution when r_low & r_high are NULL", {
#   expect_equal(
#     output[['wealth_heterogeneity']],
#     output[['wealth_homogeneity']]
#   )
# })
# 
# household_table2 <- data.table::copy(household_table)
# household_table2[is.na(get('K2009')),'K2009' := 0L]
# 
# wealth_data2 <- capitulation:::life_cycle_model(household_table2,
#                                                          r = r,
#                                                          beta = beta,
#                                                          gamma = gamma,
#                                                          wealthvar_survey = "K2009",
#                                                          observation_year = 2009,
#                                                          income_var = "salaire_tot",
#                                                          weight_var = NULL,
#                                                          output_var = "wealth_homogeneity2")
# 
# 
# output <- merge(wealth_data,wealth_data2)
# 
# 
# test_that("homogeneous (new version) and heterogeneous model give same solution when r_low & r_high are NULL", {
#   expect_equal(
#     output[['wealth_heterogeneity']],
#     output[['wealth_homogeneity2']]
#   )
# })
# 
# 
# 
# # ---------------------------------------------
# # WHEN r_low and r_high ARE PROVIDED
# 
# 
# wealth_data <- capitulation::life_cycle_model_heterogeneity(household_table,
#                                                             r_low = 0.02,
#                                                             r_high = 0.04,
#                                                             beta = beta,
#                                                             gamma = gamma,
#                                                             wealthvar_survey = "K2009",
#                                                             observation_year = 2009,
#                                                             income_var = "salaire_tot",
#                                                             weight_var = NULL,
#                                                             group_var = "income_group",
#                                                             output_var = "wealth",
#                                                             return_last = TRUE)
# 
# 
# 
# 
# 
# test_that("heterogeneous model: 0 wealth in the end", {
#   expect_equal(
#     wealth_data[, .SD[which.max(annee)], by=Id][['wealth']],
#     rep(0,2)
#   )
# })
# 
# 
# wealth_data <- capitulation::life_cycle_model_heterogeneity(household_table,
#                                                             r_low = 0.02,
#                                                             r_high = 0.04,
#                                                             beta = beta,
#                                                             gamma = gamma,
#                                                             wealthvar_survey = "K2009",
#                                                             observation_year = 2009,
#                                                             income_var = "salaire_tot",
#                                                             weight_var = NULL,
#                                                             group_var = "income_group",
#                                                             output_var = "wealth",
#                                                             return_last = FALSE)
# 
# test_that("Kt(2009) consistent with observed value", {
#   expect_equal(
#     wealth_data[get("annee") == 2009][['wealth']],
#     wealth_data[get("annee") == 2009][['K2009']]
#     )
# })
