context('Ensure estimate_K0 gives consistent values for observation year')


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


z[,'K2009' := lest::case_when(
  get('annee') == 2009 ~ 100,
  TRUE ~ NA_real_
)]

z[,'UC' := 1]


household_table <- data.table::copy(z)


# ---------------------------------
# ESTIMATE WEALTH
# ---------------------------------

# CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
tempdf_noRA <- estimate_K0(data = household_table,
                           r = .02,
                           gamma = NULL,
                           wealthvar_survey = "K2009")


tempdf_RA <- estimate_K0(data = household_table,
                         r = .02,
                         gamma = .5,
                         wealthvar_survey = "K2009")

tempdf_RA_beta <- estimate_K0(data = household_table,
                              r = .02,
                              gamma = .5,
                              beta = 0.98,
                              wealthvar_survey = "K2009")


tempdf_noRA[, 'K' := simulate_wealth_structural(K0 = get('K0'),
                                                consumption0 = get('C0'),
                                                income = get('salaire_tot'),
                                                UC = get('UC'),
                                                r = .02), by = c('Id')]

tempdf_RA[, 'K' := simulate_wealth_structural_ra(K0 = get('K0'),
                                                 consumption0 = get('C0'),
                                                 income = get('salaire_tot'),
                                                 UC = get('UC'),
                                                 gamma = .5,
                                                 r = .02), by = c('Id')]

tempdf_RA_beta[, 'K' := simulate_wealth_structural_beta(K0 = get('K0'),
                                                 consumption0 = get('C0'),
                                                 income = get('salaire_tot'),
                                                 UC = get('UC'),
                                                 gamma = .5,
                                                 r = .02, beta = 0.98), by = c('Id')]


# ===============================================
# TEST 1: WE FIND AN ESTIMATED VALUE CONSISTENT WITH OBSERVED
#   FOR SURVEY YEAR
# ===============================================


test_that("Estimated value consistent with observed value for survey year [no risk aversion]", {
  expect_equal(
    round(tempdf_noRA[annee==2009][['K']]),
    as.numeric(
      round(tempdf_noRA[annee==2009][['K2009']]))
  )
})


test_that("Estimated value consistent with observed value for survey year  [risk aversion]", {
  expect_equal(
    round(tempdf_RA[annee==2009][['K']]),
    as.numeric(
      round(tempdf_RA[annee==2009][['K2009']]))
  )
})

test_that("Estimated value consistent with observed value for survey year  [risk aversion + beta]", {
  expect_equal(
    round(tempdf_RA_beta[annee==2009][['K']]),
    as.numeric(
      round(tempdf_RA_beta[annee==2009][['K2009']]))
  )
})


# ===============================================
# TEST 2: WE END UP WITH 0 WEALTH
# ===============================================

tempdf_noRA[, 'K' := simulate_wealth_structural(K0 = get('K0'),
                                                consumption0 = get('C0'),
                                                income = get('salaire_tot'),
                                                UC = get('UC'),
                                                r = .02,
                                                returnLast = T)[-1], by = c('Id')]

tempdf_RA[, 'K' := simulate_wealth_structural_ra(K0 = get('K0'),
                                                 consumption0 = get('C0'),
                                                 income = get('salaire_tot'),
                                                 UC = get('UC'),
                                                 gamma = .5,
                                                 r = .02,
                                                 returnLast = T)[-1], by = c('Id')]

tempdf_RA_beta[, 'K' := simulate_wealth_structural_beta(K0 = get('K0'),
                                                 consumption0 = get('C0'),
                                                 income = get('salaire_tot'),
                                                 UC = get('UC'),
                                                 gamma = .5,
                                                 r = .02,
                                                 beta = 0.98,
                                                 returnLast = T)[-1], by = c('Id')]

test_that("Death with 0 wealth [no risk aversion]", {
  expect_equal(
    round(tempdf_noRA[annee==max(annee)][['K']]),
    0
  )
})

test_that("Death with 0 wealth [risk aversion]", {
  expect_equal(
    round(tempdf_RA[annee==max(annee)][['K']]),
    0
  )
})

test_that("Death with 0 wealth [risk aversion + beta]", {
  expect_equal(
    round(tempdf_RA_beta[annee==max(annee)][['K']]),
    0
  )
})


# # df = data.table::copy(menages)
# # df[,val := as.numeric(matri==1)]
# # df[, val2 := (sum(val) == length(val)),by=Id]
# # df <- df[(val2)]
# 
# df <- menages[dernier_changement<=2009]
# wealthvar_survey = "PATFISOM"
# 
# traj_complete <- data.table::copy(df)
# 
# # Transform wealth into vector variables from Enquete Patrimoine
# traj_complete[, `:=` (wealth2009 = mean(get(wealthvar_survey),na.rm=TRUE)), by = c('Id')]
# 
# # KEEP PEOPLE THAT APPEAR IN ENQUETE PATRIMOINE (immigrants will be imputed K0=0 later)
# traj_complete <- traj_complete[!is.nan(get('wealth2009'))]
# 
# 
# # data2009 <- data[annee==2009][,.SD,.SDcols = c('Id','annee','age','PATFISOM','AGFINETU')]
# # #data.table::setnames(data2009,old = 'age', new = 'age2009')
# # 
# # 
# # traj_complete <- merge(traj_complete, data2009, by = c('Id','age','annee'), all.x = T)
# # traj_complete[, `:=` (wealth2009 = mean(get(wealthvar_survey),na.rm=T),
# #                       findet2009 = mean(get(findet_survey),na.rm=T)),
# #               by = c('Id')]
# # traj_complete <- traj_complete[!is.nan(wealth2009)]
# 
# traj_complete[,'tt' := as.numeric(get('annee')==2009)]
# 
# traj_complete <- traj_complete[!is.na(get('salaire_tot'))]
# 
# UCvar = "UC"
# if (is.null(UCvar)) traj_complete[,'nbreUC' := 1] else traj_complete[,'nbreUC' := get(UCvar)]
# 
# 
# r = 0.02
# 
# Rcpp::sourceCpp(file = "./src/calibrate_K0_V2.cpp")
# Rcpp::sourceCpp(file = "./src/simulate_wealth_structural.cpp")
# 
# 
# traj_complete = traj_complete[Id==1]
# 
# 
# 
# traj_complete2 = traj_complete[, .(listval = list(calibrate_K0_v2(
#   age = get('age'),
#   income = get('salaire_tot'),
#   findetVector = get('findet'),
#   K2009vector = get('wealth2009'),
#   timeIndex = get('tt'),
#   UC = get('nbreUC'),
#   r = r))),
#   by = c('Id')]
# 
# traj_complete2[, `:=`('K0' = as.numeric(lapply(listval, function(x) as.numeric(x["K0"]))),
#                       'C0' = as.numeric(lapply(listval, function(x) as.numeric(x["C0"]))))]
# 
# dt = merge(traj_complete2,traj_complete,all.y=T)
# 
# dt[, 'K' := simulate_wealth_structural_v2(K0 = K0,
#                                           consumption0 = C0,
#                                           income = salaire_tot,
#                                           UC = nbreUC,
#                                           r = r), by = c('Id')]
# dt[annee %in% 2006:2010]
# 
# 
# traj_complete2 = traj_complete[, .(listval = list(calibrate_K0_R_v2(df))), by = Id]
# traj_complete2[, `:=`('K0' = as.numeric(lapply(listval, function(x) as.numeric(x[[1]]))),
#                       'C0' = as.numeric(lapply(listval, function(x) as.numeric(x[[2]]))))]
# 
# dt = merge(traj_complete2,traj_complete,all.y=T)
# 
# dt[, 'K' := simulate_wealth_structural_v2(K0 = K0,
#                                           consumption0 = C0,
#                                           income = salaire_tot,
#                                           UC = nbreUC,
#                                           r = r), by = c('Id')]
# dt[annee %in% 2008:2010]
# 
# 
# ### OLD
# 
# 
# compute_K <- function(df){
#   
#   K0 <- compute_K0(df)
#   C0 <- (K0 + sum(df$betaY))/sum(df$beta)
#   t = df[annee==2009]$t
#   
#   # Terme de eq. (7) : (1+r)^(-tau)*(y_(tau) - C0)
#   df[, S:= (1+r)^(-tau) * (salaire_tot-C0)]
#   
#   get_Kt <- function(df, C0, K0, time = 1){
#     da <- df[tau< time]
#     K <- (1+r)^time*(K0 + sum(da$S))
#   }
#   
#   vectK <- c(K0,sapply(1:max(df$tau), function(indiceT)  get_Kt(df, C0, K0, time = indiceT)))
#   df[,'K' := vectK]
#   
#   ggplot2::ggplot(df) + ggplot2::geom_line(ggplot2::aes(x = annee,y=K), color = 'black') +
#     ggplot2::geom_line(ggplot2::aes(x = annee,y=salaire_tot), color = 'blue') +
#     ggplot2::geom_hline(yintercept = 0) +
#     ggplot2::geom_vline(xintercept = 2009) +
#     ggplot2::geom_hline(yintercept = mean(df$PATFISOM,na.rm=T), color = 'red')+
#     ggplot2::geom_hline(yintercept = mean(df$C0), color = 'green')
#   
#   return(df)
# }
# 
# #compute_K(df)
# 
# df2 <- data.table::copy(df) 
# df2[, c("K0", "C0") := NULL]
# 
# df2 <- df2[Id<60000]
# 
# df2[,tt := as.numeric(annee==2009)]
# df2[,t2009 := sum(tt,na.rm = T), by = c('Id')]
# df2 <- df2[t2009>0]
# df2[,`:=`(wealth2009 = mean(PATFISOM, na.rm = T), tt = as.numeric(annee==2009))]
# #,by = c('Id')]
# 
# Rcpp::sourceCpp(file = "./testf.cpp")
# 
# 
# ##############################
# ### C++ only
# 
# traj_complete2 = df2[, .(listval = list(calibrate_K0(age = age,
#                                                      income = salaire_tot,
#                                                      findetVector = findet,
#                                                      K2009vector = wealth2009,
#                                                      timeIndex = tt,
#                                                      r = 0.02))),
#                      by = c('Id')]
# 
# traj_complete2[, `:=`('K0' = as.numeric(lapply(listval, function(x) as.numeric(x["K0"]))),
#                       'C0' = as.numeric(lapply(listval, function(x) as.numeric(x["C0"]))))]
# 
# df2 <- merge(df2,traj_complete2, by = 'Id')
# 
# get_K_traj <- function(df2){
#   K <- simulate_wealth_structural(id = df2$Id,
#                                   K0 = df2$K0,
#                                   consumption0 = df2$C0,
#                                   year = df2$annee,
#                                   income = df2$salaire_tot,
#                                   age = df2$age,
#                                   r = r)
#   return(K[-length(K)])
# }
# 
# df2[, 'K' := K[-length(K)]]
# 
# 
# df2[annee %in% 2008:2010]
# 
# 
# 
# ggplot2::ggplot(df2) + ggplot2::geom_line(ggplot2::aes(x = annee,y=K/1000), color = 'black') +
#   ggplot2::geom_line(ggplot2::aes(x = annee,y=salaire_tot/1000), color = 'blue') +
#   ggplot2::geom_hline(yintercept = 0) +
#   ggplot2::geom_vline(xintercept = 2009) +
#   ggplot2::geom_hline(yintercept = mean(df2$PATFISOM,na.rm=T)/1000, color = 'red')+
#   ggplot2::geom_hline(yintercept = mean(df2$C0,na.rm=T)/1000, color = 'green', linetype = 'dashed')
# 
# 
# 
# ##############################
# ### Mix R and C++
# 
# 
# do_K <- function(data, r=0.02){
#   
#   df2 <- data.table::copy(data)
#   df2[, `:=`('K0' = compute_K0(data),'C0' = compute_C0(data))]
#   
#   K <- simulate_wealth_structural(id = df2$Id,
#                                   K0 = df2$K0,
#                                   consumption0 = df2$C0,
#                                   year = df2$annee,
#                                   income = df2$salaire_tot,
#                                   age = df2$age,
#                                   r = r)
#   
#   df2[, 'K' := K[-length(K)]]
#   #df2[annee %in% 2008:2010]
#   return(df2)
#   
# }
# 
# dfK = do_K(df2)
# 
# 
# 
# dplyr::lead(dfK$K) - (1+r)*(dfK$K + dfK$salaire_tot - dfK$C0)
# 
# 
# 
# # Simulons K0
# 
# K0 = 1000
# 
# df3 <- data.table::copy(df2)
# df3[,c('K0','C0'):=NULL]
# 
# r = .02
# df3[, `:=` (betaY = (1+r)^(-tau)*salaire_tot,
#             beta = (1+r)^(-tau),
#             rY = ((1+r)^tau)*salaire_tot)]
# 
# C0 <- (K0 + sum(df3$betaY))/sum(df3$beta)
# 
# df3[,`:=` (K0 = K0, C0 = C0) ]
# 
# df3[, 'K' := simulate_wealth_structural(id = Id,
#                                         K0 = K0,
#                                         consumption0 = C0,
#                                         year = annee,
#                                         income = salaire_tot,
#                                         age = age,
#                                         r = .02)[-1]]
# df3$K
# 
# ggplot2::ggplot(df3) + ggplot2::geom_line(ggplot2::aes(x=annee, y = K))
# 
# df3$K2 <- c(0,df3$K[-nrow(df3)])
# 
# df3$K - (1+r)*(dplyr::lag(df3$K) + df3$salaire_tot - df3$C0)
# df2[nrow(df2)]
# 
# 
# 
# # Discrepancy btw estimates and value
# 
# df2009 = df2[annee==2009]
# df2009[,'diff' := K-PATFISOM]
# df2009 = df2009[is.finite(diff)]
# 
# median(df2009$diff,na.rm = T)
