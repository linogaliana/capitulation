#' Aggregate income at household level
#' 
#' Match individual with income at household level
#' 
#' @param table_indiv Individual level data where
#'  people are matched to husband/spouse id
#' @param id_var Individual household identifier column name
#' @param income_var Individual income column name
#' @param age_var Individual age column name
#' @param ageliq_var Individual liquidation age variable name
#' @param matrimonial_var Matrimonial status variable
#' @return Individual level data with income aggregated
#'  at household level
#'  
#' @details By assumption, male is assumed to be the household head.
#' 
#' Several arguments are proposed to ensure a column name discrepancy
#'  between two inputs will not produce different results
#'  
#' @export

income_household <- function(table_indiv, id_var = 'Id', income_var = 'salaire',
                             age_var = "age", ageliq_var = "ageliq",
                             matrimonial_var = "matri"){
  
  # I - INCOME AT HOUSEHOLD LEVEL
  # ---------------------------------
  # Create a 'salaire_tot' variable which is the sum of income of at household level
  
  
  # a) Store husband/spouse information  
  # --------------------------------------------
  # Create a salaire_conjoints table that will be joined with individual level table
  
  
  salaires_conjoints <- data.table::copy(table_indiv)
  data.table::setnames(salaires_conjoints, old = c(id_var,income_var,age_var,ageliq_var),
                       new = c('conjoint', c(income_var,age_var, ageliq_var) %s+% "_conjoint"))
  salaires_conjoints <-
    salaires_conjoints[,.SD,
                       .SDcols = c('conjoint',
                                   'annee',
                                   c(income_var,
                                     age_var,
                                     ageliq_var) %s+% "_conjoint")]
  
  
  # b) Merge individual information with spouse information
  # -----------------------------------------------------------
  
  individus <- data.table::copy(table_indiv)
  data.table::setnames(individus, new = "salaire_tot", old = income_var)
  
  data.table::setkeyv(individus, c("conjoint","annee"))
  data.table::setkeyv(salaires_conjoints, c("conjoint","annee"))
  
  # GET TABLE WITH REFERENT AND WIFE INCOME INFORMATION
  individus <- merge(individus,salaires_conjoints, all.x = TRUE)
  
  # TRANSFORM NAs TO ZERO
  individus[, `:=` ('salaire_conjoint' = lest::case_when(
    is.na(get(income_var %s+% '_conjoint')) ~ as.numeric(0),
    TRUE ~ get(income_var %s+% '_conjoint')))]

  
  # FINALIZE BY SUMMING INCOME
  # ---------------------------------------------------------
  
  individus[,'salaire_tot' := lest::case_when(
    get(matrimonial_var) == 2 ~ get("salaire_tot") + get(income_var %s+% '_conjoint'),
    TRUE ~ get("salaire_tot")
  )]
  individus[, 'salaire' := get('salaire_tot')-get('salaire_conjoint')]
  individus[, 'salaire_indiv' := get('salaire')]
  individus[, 'salaire_conjoint' := get('salaire_conjoint')]
  
  # KEEP RELEVENT INCOME  
  # individus <- individus[,.SD, .SDcols = c(id_var,age_var,'annee',
  #                                          'salaire_tot',
  #                                          'salaire',
  #                                          'salaire_conjoint',
  #                                          'conjoint', 
  #                                          matrimonial_var,
  #                                          'sexe',
  #                                          'referent',
  #                                          'referent2',
  #                                          'findet',
  #                                          age_var %s+% '_conjoint',
  #                                          ageliq_var,
  #                                          ageliq_var %s+% '_conjoint',
  #                                          'UC','nbpersm',
  #                                          'list_children_charge','list_children_life')]
  
  colstodelete <- c("pere", "mere", "anaiss",
    "neFrance", "referent2",
    "ageMax", "ageMaxMere",
    "ageMaxPere")
  colstodelete <- colstodelete[colstodelete %in% colnames(individus)]
  
  if (length(colstodelete)>0) individus[, (colstodelete)  := NULL]
  
  # ORDER DATA
  individus <- individus[order(get(id_var),get('annee'))] 
  
  return(individus)
}
