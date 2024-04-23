# # rm(list = ls())
# # path_data <- "./inst/dataINSEE"
# # menages_structural <- capitulation::prepare_data(path_data)
# # save(menages_structural, file = "../temporary/individual_data.RData")
# 
# 
# path_data <- "../temporary/individual_data.RData"
# load(path_data)
# 
# id_var = "Id"
# year_var = "annee"
# income_var = "y_indiv"
# wealthvar_survey = "K_observed"
# age_var = "age"
# weight_var = "weight"
# 
# menages_structural[,'weight' := 1]
# 
# traj_complete <- menages_structural[,.SD,
#                                     .SDcols = c(id_var,year_var,income_var,
#                                                 wealthvar_survey, age_var,
#                                                 "tt", weight_var)]
# 
# data <- menages_structural[Id<10]
# 
# data2 <- split(data, by = "Id")
# 
# 
# r=0.02
# gamma = 0.5
# beta = 1
# 
# 
# 
# 
# essai = lapply(data2, function(g)({
#   # data.table::data.table(
#   #   data.frame(
#   data.table::data.table(
#     data.frame(
#       wrapper_function(elem = g, f = life_cycle_model_cpp2_old,
#                        id_var = "Id",
#                        income_var = "y_indiv",
#                        observed_wealth_var = "K_observed",
#                        weight_var = "weight")
#     ))
#   # K = wrapper_function(elem = g, f = life_cycle_model_cpp2_old),
#   # Id = unique(g$Id),
#   # annee = g$annee
#   # ))
# })
# )
# 
# essai = data.table::rbindlist(essai)
# data.table::setnames(essai, "x","K")
# 
# 
# essai2 <- data.table::copy(data)
# essai2[, `:=`('wealth' =  life_cycle_model_cpp2_old(income = get(income_var),
#                                                 K2009vector = get(wealthvar_survey),
#                                                 timeIndex = get("tt"),
#                                                 UC = get(weight_var),
#                                                 r = r,
#                                                 gamma = gamma,
#                                                 beta = beta, returnLast = FALSE)
# ), by = 'Id']
# 
# 
# 
# 
# essai_new = merge(essai,essai2, by = c('Id','annee'))
# essai_new[K!=wealth]
# 
# 
# 
# essai3 = life_cycle_apply(input = data2,
#                           f = life_cycle_model_cpp2_old,
#                           id_var = "Id",
#                           income_var = "y_indiv",
#                           observed_wealth_var = "K_observed",
#                           weight_var = "weight")
# 
# essai3 <- data.table::rbindlist(
#   lapply(essai3, data.table::setDT)
# )
# 
# 
# essai_new = merge(essai_new,essai3, by = c('Id','annee'))
# essai_new[K!=x]
# 
# 
# 
# data = data.table::copy(menages_structural)
# 
# 
# 
# id_var = "Id"
# year_var = "annee"
# traj_complete <- data[,.SD,
#                       .SDcols = c(id_var,year_var,income_var,
#                                   wealthvar_survey,
#                                   "tt", weight_var)]
# 
# 
# profvis::profvis({
#   simulations_new <- capitulation::life_cycle_model(data = menages_structural,
#                                                     r = 0.05, gamma = 0.5,
#                                                     beta = 0.98,
#                                                     income_var = "y_real",
#                                                     weight_var = NULL,
#                                                     wealthvar_survey = "wealth2009")
# })
# 
# 
# essai = split(traj_complete, "Id")
# 
# profvis::profvis({
#   essai3 = life_cycle_apply(input = essai,
#                             f = life_cycle_model_cpp2_old)
#   essai3 <- lapply(essai3, function(i) ({
#     data.table::data.table(
#       data.frame(
#         i
#       )
#     )
#   })
#   )
#   essai3 <- data.table::rbindlist(essai3)
# })
# 
# 
# essai3
