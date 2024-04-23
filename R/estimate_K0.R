#' Estimate initial wealth (K0) that is consistent with observed
#'  value in survey in a pure life-cycle model
#'
#' @param data Microsimulated data merged with household data
#'  in the relevent observation year
#' @param observation_year Survey data year
#' @param weight_var Variable storing individual weight inside household. This can be 1
#'  (individual is the unique adult representing household), 2 (couples) or
#'  a value related to consumption units
#' @param r Interest rate
#' @param beta Discount factor
#' @param gamma Risk aversion coefficient
#' @param wealthvar_survey Variable for wealth in survey data
#' @param income_var Income variable in microsimulated data
#' @export

estimate_K0 <- function(data,
                        r = 0.02,
                        beta = 1,
                        gamma = 0.5,
                        wealthvar_survey = "PATFISOM",
                        observation_year = 2009,
                        income_var = "salaire_tot",
                        weight_var = NULL){
  
  traj_complete <- data.table::copy(data)
  
  # Avoid NSE notes
  .<-NULL
  
  if (is.null(weight_var)){
    traj_complete[, "weight" := 1L]
    weight_var <- "weight"
  }
  

  # INDIVIDUAL TRAJECTORY
  # --------------------------
  
  # traj_complete <- simul$emp
  # traj_age <- indiv[,.SD,.SDcols = c('Id','annee','age')]
  
  # traj_complete <- merge(data,traj_age, by = c('Id','annee','age'),
  #                        all.x = TRUE)
  # age2009 <- traj_age[get('annee')==2009,.SD,.SDcols = c('Id','age')]
  # data.table::setnames(age2009, old = 'age', new = 'age2009')
  # 
  # traj_complete <- merge(traj_complete,age2009, by = c('Id'),
  #                        all.x = TRUE)
  
  
  # Transform wealth into vector variables from Enquete Patrimoine
  traj_complete[, `:=` (wealth2009 = mean(get(wealthvar_survey),na.rm=TRUE)), by = c('Id')]
  
  
  
  # data2009 <- data[annee==2009][,.SD,.SDcols = c('Id','annee','age','PATFISOM','AGFINETU')]
  # #data.table::setnames(data2009,old = 'age', new = 'age2009')
  # 
  # 
  # traj_complete <- merge(traj_complete, data2009, by = c('Id','age','annee'), all.x = TRUE)
  # traj_complete[, `:=` (wealth2009 = mean(get(wealthvar_survey),na.rm=TRUE),
  #                       findet2009 = mean(get(findet_survey),na.rm=TRUE)),
  #               by = c('Id')]
  # traj_complete <- traj_complete[!is.nan(wealth2009)]
  
  traj_complete[,'tt' := as.numeric(get('annee')==observation_year)]
  
  
  traj_complete <- traj_complete[!is.na(get(income_var))]
  

  # KEEP ONLY OBSERVATION AFTER STARTING WORKING LIFE
  # FILTER NOW PERFORMED IN prepare_data FUNCTION
  # traj_complete <- traj_complete[get('age')>=get('findet')]
  
  
  
  # CREATE tau VECTOR
  # -------------------------------
  
  traj_complete[, 'year_findet' := min(get('annee'),na.rm=TRUE) ,by = c('Id')]
  traj_complete[,'tau' := get('annee')-get('year_findet')]
  
  
  
  
  
  # KEEP PEOPLE THAT APPEAR IN ENQUETE PATRIMOINE (immigrants will be imputed K0=0 later)
  traj_complete2 <- traj_complete[!is.nan(get('wealth2009'))]
  
  
  # ==========================
  # CALIBRATE K0
  # ==========================
  
  
  # MENAGES AVEC PLUSIEURS OBSERVATIONS
  # --------------------------------------------
  
  # if (is.null(gamma)){
  #   
  #   traj_complete2 <- traj_complete2[, .('listval' = list(calibrate_K0(
  #     age = get('age'),
  #     income = get(income_var),
  #     findetVector = get('findet'),
  #     K2009vector = get('wealth2009'),
  #     timeIndex = get('tt'),
  #     UC = get('nbreUC'),
  #     r = r))),
  #     by = c('Id')]
  #   
  # } else{
  #   
  #   traj_complete2 <- traj_complete2[, .('listval' = list(calibrate_K0_ra(
  #     age = get('age'),
  #     income = get(income_var),
  #     findetVector = get('findet'),
  #     K2009vector = get('wealth2009'),
  #     timeIndex = get('tt'),
  #     UC = get('nbreUC'),
  #     r = r,
  #     gamma = gamma))),
  #     by = c('Id')]
  #   
  # }
  
  if (is.null(gamma)){
    traj_complete2 <- traj_complete2[, .('listval' = list(calibrate_K0(
      age = get('age'),
      income = get(income_var),
      findetVector = get('findet'),
      K2009vector = get('wealth2009'),
      timeIndex = get('tt'),
      UC = get(weight_var),
      r = r))),
      by = c('Id')]
  } else{
    traj_complete2 <- traj_complete2[, .('listval' = list(calibrate_K0_beta(
      age = get('age'),
      income = get(income_var),
      findetVector = get('findet'),
      K2009vector = get('wealth2009'),
      timeIndex = get('tt'),
      UC = get(weight_var),
      r = r,
      gamma = gamma,
      beta = beta))),
      by = c('Id')]
  }
  
  
  
  
  traj_complete2[, `:=`('K0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["K0"]))),
                        'C0' = as.numeric(lapply(get('listval'), function(x) as.numeric(x["C0"]))))]
  
  traj_complete2[, c('listval') := NULL]
  
  
  traj_complete2 <- traj_complete2[!is.nan(get('K0'))]
  
  
  # OTHERS SHOULD BE IMPUTED K0=0
  # --------------------------------------
  
  traj_others <- traj_complete[!(get('Id') %in% traj_complete2$Id)]
  
  # ON NE GARDE LA TRAJECTOIRE DES MIGRANTS QUE A PARTIR DE LEUR ARRIVEE EN FRANCE
  traj_others <- traj_others[get('annee')>=2010]
  
  if (is.null(gamma)){
    
    traj_others <- traj_others[, .('C0' = C0_given_K0_noRA(
      income = get(income_var),
      timeIndex = get('tau'),
      K0 = 0,
      r = r)
    ), by = 'Id']
    
  } else{
    
    traj_others <- traj_others[, . ('C0' = C0_given_K0_beta(
      income = get(income_var),
      timeIndex = get('tau'),
      K0 = 0,
      r = r,
      gamma = gamma,
      beta = beta)
    ), by = 'Id']
    
  }
  
  
  
  traj_others <- traj_others[,'K0' := 0]
  
  
  
  # BRING BACK TO HOUSEHOLD LEVEL DATA
  # --------------------------------------
  
  traj_complete2 <- data.table::rbindlist(
    list(traj_complete2,traj_others),
    use.names = TRUE
  )
  
  # INNER JOIN RATHER THAN LEFT JOIN
  menages_augm <- merge(data, traj_complete2,
                        by = c('Id'))
  
  # OTHERS START WITH 0 CAPITAL  
  menages_augm[, c('K0','C0') := lapply(.SD, function(c) tidyr::replace_na(c, 0)),
               .SDcols = c('K0','C0')]
  
  
  return(menages_augm)
}


# ggplot2::ggplot(essai) + ggplot2::geom_line(ggplot2::aes(x = annee, y = salaire_tot)) + ggplot2::geom_vline(xintercept = 2009)
# 
# ggplot2::ggplot(traj_complete2) + ggplot2::geom_histogram(ggplot2::aes(x = V1, y = ..density..)) +
#   ggplot2::geom_vline(xintercept = 0)  
# 
# mean(traj_complete2$V1<0,na.rm = TRUE)

