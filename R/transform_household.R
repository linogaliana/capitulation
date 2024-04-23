#' Transform individual data into household data
#' 
#' Finalize transformation of individual data into household data.
#' Aggregate income at household level data and keep only
#'  one adult by household. It is assumed \link{assign_referent}
#'  has already been called. 
#'  
#' @inheritParams income_household
#' 
#' @details By assumption, male is assumed to be the household head.
#' 
#' Several arguments are proposed to ensure a column name discrepancy
#'  between two inputs will not produce different results
#' 
#' @return Individual level data with income aggregated
#'  at household level
#' @examples
#' # Toy example
#' z <- data.frame(
#'   Id = rep(1:2,2),
#'   annee = c(2008,2008,2009,2009),
#'   sexe = rep(1:2,2),
#'   # Married together
#'   matri = rep(2,4),
#'   conjoint = rep(2:1,2),
#'   # Male is household head
#'   referent = rep(1,4),
#'   ageliq = rep(c(60,65),2),
#'   salaire = c(10000,2,0,8),
#'   age = c(25,23,26,24),
#'   findet = rep(20,4)
#' )
#' 
#' # Results are consistent in dplyr or dt
#' # approaches
#' z_dplyr <- z
#' z_dt <- data.table::as.data.table(z)
#' head(z_dplyr)
#' head(z_dt)
#' @import data.table
#' @importFrom stringi %s+%
#' @export


transform_household <- function(table_indiv, id_var = 'Id', income_var = 'salaire',
                                age_var = "age", ageliq_var = "ageliq",
                                matrimonial_var = "matri"){
  
  isDT <- inherits(table_indiv, "data.table")
  
  # Call DT version of the function
  if (isDT) return(
    transform_household_dt(
      table_indiv = table_indiv,
      id_var = id_var,
      income_var = income_var,
      age_var = age_var,
      ageliq_var = ageliq_var,
      matrimonial_var = matrimonial_var)
  )
  
  message("tidyverse approach is deprected. Output might be incorrect")
  
  # I - INCOME AT HOUSEHOLD LEVEL
  # ---------------------------------
  # Create a 'salaire_tot' variable which is the sum of income of at household level
  
  
  # a) Store husband/spouse information  
  # --------------------------------------------
  # Create a salaire_conjoints table that will be joined with individual level table
  
  salaires_conjoints <- table_indiv %>%
    dplyr::mutate(!!rlang::sym('conjoint') := !!rlang::sym(id_var),
                  !!rlang::sym(income_var %s+% "_conjoint") := !!rlang::sym(income_var),
                  !!rlang::sym(age_var %s+% "_conjoint") := !!rlang::sym(age_var),
                  !!rlang::sym(ageliq_var %s+% "_conjoint") := !!rlang::sym(ageliq_var)) %>%
    dplyr::select(
      !!c('conjoint','annee',c(income_var,age_var, ageliq_var) %s+% "_conjoint")
    )
  
  
  # b) Merge individual information with spouse information
  # -----------------------------------------------------------
  
  
  individus <- table_indiv %>%
    dplyr::rename(!!rlang::sym('salaire_tot') := !!rlang::sym(income_var)) %>%
    dplyr::left_join(salaires_conjoints, by = c("conjoint", "annee")) %>%
    dplyr::mutate(
      !!rlang::sym('salaire_tot') := dplyr::case_when(
        # Individual in couple: HOUSEHOLD INCOME: referent income + spouse/husband income
        !!rlang::sym(matrimonial_var) == 2 ~ .data$salaire_tot + !!rlang::sym(income_var %s+% '_conjoint'),
        # Alone: household income = individual income
        TRUE ~ .data$salaire_tot
      )
    ) %>%
    dplyr::select(
      !!c(id_var,age_var,'annee','salaire_tot',
          'conjoint',matrimonial_var,'sexe',
          'referent','findet',age_var %s+% '_conjoint',
          ageliq_var, ageliq_var %s+% '_conjoint')
    ) %>%
    dplyr::arrange(!!rlang::sym(id_var), .data$annee)  
  # Keep individual information as well as some spouse information
  
  
  
  
  # II - KEEP ONE HOUSEHOLD HEAD
  # ---------------------------------
  
  # To get an household table, we only keep referent as being 
  # the head
  
  # Keep only referents
  referents <- individus %>% dplyr::filter(!!rlang::sym(id_var) == .data$referent) %>%
    dplyr::mutate(dernier_changement = .data$annee,
                  patrimoine = 0, annee_cle = 0) %>%
    dplyr::select(
      !!c(id_var, age_var, 'annee', 'salaire_tot',
          'conjoint',matrimonial_var, 'sexe','findet',
          'dernier_changement','patrimoine',age_var %s+% '_conjoint',
          ageliq_var, 'annee_cle')
    )
  
  
  return(referents)
}


transform_household_dt <- function(table_indiv, id_var = 'Id', income_var = 'salaire',
                                   age_var = "age", ageliq_var = "ageliq",
                                   matrimonial_var = "matri"){
  
  
  # I - INCOME AT HOUSEHOLD LEVEL
  # ---------------------------------
  # Create a 'salaire_tot' variable which is the sum of income of at household level
  
  individus <- income_household(table_indiv, id_var = id_var,
                                income_var = income_var,
                                age_var = age_var,
                                ageliq_var = ageliq_var,
                                matrimonial_var = matrimonial_var)
  
  
  # II - KEEP ONE HOUSEHOLD HEAD
  # ---------------------------------
  
  # To get an household table, we only keep referent as being 
  # the head
  
  # Keep only referents
  referents <- individus[get(id_var) == get('referent'),]
  
  referents[,`:=`(dernier_changement = get('annee'),
                  patrimoine = 0,
                  annee_cle = 0)]
  
  referents <- referents[,.SD,
                         .SDcols = c(id_var, age_var, 'annee',
                                     'salaire_tot',
                                     'conjoint',matrimonial_var,
                                     'sexe','findet',
                                     'dernier_changement','patrimoine',
                                     age_var %s+% '_conjoint',
                                     ageliq_var, 'annee_cle')]
  
  
  
  return(referents)
}
