#' Prepare individual panel data before household aggregation
#' 
#' @details In practice, this function handles special cases from Destinie to get a proper table
#' 
#' @param table_indiv Individual data table on income (wages or pensions)
#' @param pensions Dataframe of pensions by id:year
#' @param descript Individual information dataframe
#' @return Longitudinal individual data
#' @examples \dontrun{
#' pre_indiv = capitulation::prepare(table_indiv = pre_indiv, pensions = pensions,
#' descript = description)}
#' @export
#' @import lest


prepare <- function(table_indiv, pensions, descript){
  
  isDT <- 'data.table' %in% class(table_indiv)
  
  if (isDT) return(prepare_dt(table_indiv,pensions,descript)) else prepare_dplyr(table_indiv,pensions,descript)
}

#' @rdname prepare

prepare_dplyr <- function(table_indiv, pensions, descript){
  
  
  # On prevoit le cas ou le salaire net vaut NA
  # ----------------------------------------------
  
  ans <- table_indiv %>%
    dplyr::mutate(salaire = dplyr::case_when(
      is.na(.data$salaires_net) ~ 0,
      !is.na(.data$salaires_net) ~ .data$salaires_net
    )) %>%
    dplyr::select(.data$Id, .data$age,
                  .data$annee, .data$salaire) %>%
    dplyr::arrange(.data$Id, .data$age)
  
  # salaire_faux <- table_indiv %>% filter(is.na(salaires_net))
  # salaire_bon  <- table_indiv %>% filter(!is.na(salaires_net)) %>%
  #   mutate(salaire = salaires_net)
  # ans <- bind_rows(salaire_faux, salaire_bon) %>% select(Id, age, annee, salaire) %>% arrange(Id, age)
  
  
  # On calcule les valeurs de la variable annee
  # --------------------------------------------------
  
  # Join with birth year
  ans <- ans %>% dplyr::left_join(
    dplyr::select(descript, .data$Id, .data$anaiss),
    by = "Id") %>%
    dplyr::mutate(annee = .data$annee + .data$anaiss) %>%
    dplyr::select(.data$Id, .data$age, .data$annee, .data$salaire)
  
  
  # Pour les retraités on remplace le salaire (parfois encore généré par Destinie) par
  # la pension + réversion
  # -------------------------------------------------------------------
  
  ans <- ans %>% dplyr::left_join(pensions, by = c("Id", "annee"))
  
  #retraite <- ans %>% filter(!is.na(pension)) %>% mutate(salaire = pension)
  #actif    <- ans %>% filter(is.na(pension))
  #ans = bind_rows(retraite, actif) %>% select(Id, age, annee, salaire) %>% arrange(Id, age)
  ans <- ans %>%
    dplyr::mutate(salaire = dplyr::case_when(
      !is.na(.data$pension) ~ .data$pension,
      is.na(.data$pension) ~ .data$salaire
    )) %>%
    dplyr::select(.data$Id, .data$age, .data$annee, .data$salaire) %>%
    dplyr::arrange(.data$Id, .data$age)
  
  
  # Comme de toute façon on ne gèrera les individus qu'à partir de 2009, on enlève les observations d'avant
  return (ans %>% dplyr::filter(.data$annee >= 2009))
}


#' @rdname prepare


prepare_dt <- function(table_indiv, pensions, descript){
  
  
  # On prevoit le cas ou le salaire net vaut NA
  # ----------------------------------------------
  
  table_indiv[,"salaire" := lest::case_when(
    is.na(get('salaires_net')) ~ 0,
    !is.na(get('salaires_net')) ~ get('salaires_net')
  )]
  
  ans <- table_indiv[order(get('Id'),get('age')),.SD,
                     .SDcols = c('Id','age',
                                 'annee','salaire')]
  
  # salaire_faux <- table_indiv %>% filter(is.na(salaires_net))
  # salaire_bon  <- table_indiv %>% filter(!is.na(salaires_net)) %>%
  #   mutate(salaire = salaires_net)
  # ans <- bind_rows(salaire_faux, salaire_bon) %>%
  #   select(Id, age, annee, salaire) %>%
  #   arrange(Id, age)
  
  
  # On calcule les valeurs de la variable annee
  # --------------------------------------------------
  
  # Join with birth year
  data.table::setkeyv(ans,'Id')
  tempdf <- descript[,.SD, .SDcols = c('Id',
                                       'anaiss')]
  data.table::setkeyv(tempdf,'Id')
  ans <- merge(ans,tempdf,all.x = TRUE)
  ans[,'annee' := get("annee") + get("anaiss")]
  ans <- ans[,.SD,.SDcols = c('Id','age',
                              'annee','salaire','anaiss')]
  
  
  # Pour les retraités on remplace le salaire
  #   (parfois encore généré par Destinie) par
  #   la pension + réversion
  # -------------------------------------------------------------------
  
  data.table::setkeyv(ans,c('Id','annee'))
  data.table::setkeyv(pensions,c('Id','annee'))
  ans <- merge(ans,pensions,all.x = TRUE)
  
  ans[,'salaire' := lest::case_when(
    !is.na(get("pension")) ~ get('pension'),
    TRUE ~ get('salaire')
  )]
  ans <- ans[order(get('Id'),get('age')),.SD,
             .SDcols = c('Id','age',
                         'annee','salaire','anaiss')]
  #ans <- ans[get('annee') >= 2009]
  # Situation cycle de vie: on simule tout, y compris avant 2009
  #     donc on garde les années antérieures avant
  
  return (ans)
}


