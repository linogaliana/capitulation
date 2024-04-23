# r <- 0.04
# gamma <- .5
# beta <- 0.98
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
# 
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
# 
# z2[,'tt' := 0]
# 
# 
# household_table <- data.table::copy(
#   data.table::rbindlist(list(z1,z2))
# )
# 
# 
# household_table[,'weight' := 1]
# 
# data_list <- split(household_table, by = "Id")
# 
# 
# output_wrapper <- lapply(data_list, function(g)({
#   capitulation:::wrapper_function(elem = g, f = life_cycle_model_cpp2_old,
#                        id_var = "Id",
#                        income_var = "salaire_tot",
#                        observed_wealth_var = "K2009",
#                        weight_var = "weight")
# })
# )
# 
# 
# output_wrapper <- data.table::rbindlist(
#   lapply(output_wrapper, data.table::setDT)
# )
# 
# 
# 
# 
# test_that("K2009 consistent with observation", {
#   expect_equal(
#     output_wrapper[get('annee')==2009L][["x"]],
#     household_table[get('annee')==2009L][["K2009"]]
#   )
# })
# 
# test_that("K0=0 for people that start life after 2009", {
#   expect_equal(
#     output_wrapper[get('Id')==2 & (get('annee')==2010L)][["x"]],
#     0
#   )
# })
# 
# 
# 
# output_wrapper2 <- lapply(data_list, function(g)({
#   capitulation:::wrapper_function(elem = g, f = life_cycle_model_cpp2_old,
#                    id_var = "Id",
#                    income_var = "salaire_tot",
#                    observed_wealth_var = "K2009",
#                    weight_var = "weight",
#                    returnLast = TRUE)
# })
# )
# 
# output_wrapper2 <- data.table::rbindlist(
#   lapply(output_wrapper2, data.table::setDT)
# )
# 
# 
# test_that("KT=0", {
#   expect_equal(
#     output_wrapper2[(get('annee')==get('max_year'))][["x"]],
#     rep(0,2L)
#   )
# })
# 
# 
# household_table2 <- data.table::copy(household_table)
# household_table2[, `:=`('wealth' =  life_cycle_model_cpp2_old(income = get("salaire_tot"),
#                                                 K2009vector = get("K2009"),
#                                                 timeIndex = get("tt"),
#                                                 UC = get("weight"),
#                                                 r = r,
#                                                 gamma = gamma,
#                                                 beta = beta, returnLast = FALSE)
# ), by = 'Id']
# 
# 
# output <- merge(output_wrapper, household_table2,
#                 by = c("Id","annee"))
# 
# 
# 
# test_that("Results are equal when considering C++ and R implementations", {
#   expect_equal(
#     output[["x"]],
#     output[["wealth"]]
#   )
# })
