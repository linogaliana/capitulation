
#' An intermediate function to arrange microsimulation data
#'  for inheritance prediction
#'  
#' This function aims to arrange microsimulated data in order
#'  to get the same variable names as *Enquete Patrimoine*
#' @param household_table Longitudinal data
#' @param data_event Data storing inheritance events
#' @param tables Destinie's tables

arrange_inheritance <- function(household_table,data_event,tables,
                                time_0 = c("graduation","birth"),
                                taille_tr_age = 5,
                                taille_tr_agfinetu = 2){
  
  time_0 <- match.arg(time_0)
  
  household_table[,c('Id') := as.character(get("Id"))]
  tables$description[,c('Id') := as.character(get("Id"))]
  
  household_table2 <- merge(household_table, tables$description[,.SD,.SDcols = c('Id','findet','sexe')],
                            by = 'Id')
  household_table2[,'AGFINETU' := get('findet')]
  
  
  household_table2[,'AGFINETU' := lest::case_when(
    get('AGFINETU') > 29 ~ 30L,
    get('AGFINETU') < 13 ~ 14L,
    TRUE ~ get('AGFINETU')
  )]
  
  household_table2[,'AGE' := lest::case_when(
    get('age') > 100 ~ 100L,
    get('age') < 15 ~ 15L,
    TRUE ~ get('age')
  )]
  
  
  household_table2[, `:=` ('tr_age' = factor(taille_tr_age*floor(get('age')/taille_tr_age)),
                          'tr_agfinetu' = factor(taille_tr_agfinetu*floor(get('AGFINETU')/taille_tr_agfinetu)))]
  
  household_table2[, c('tr_agfinetu') := as.factor(get('tr_agfinetu'))]
  household_table2[, c('tr_age') := as.factor(get('tr_age'))]
  
  # if (sum(grepl('SEXE', colnames(household_table2)))==0){
  #   household_table2[,'SEXE' := as.factor(get('sexe'))]
  # } else{
  #   household_table2[,'SEXE' := as.factor(get('SEXE'))]
  # }
  
  household_table2[,'revenu' := get('y_indiv')]
  # small hack for 0 income before graduation when time_0 is 'birth'
  if (time_0 == "birth") household_table2 <- household_table2[get("revenu")==0, c('revenu') := 1]
  household_table2 <- household_table2[get("revenu")>0]
  
  
  household_table2[,c('Id') := as.character(get('Id'))]
  
  household_table2[,'lw' := log(get('revenu'))]
  
  data.table::setnames(data_event, old = "Id",
                       new = "id_death")
  data_prediction <- merge(household_table2, data_event,
                           by.y = c("heir","annee"),
                           by.x = c("Id","annee"),
                           all.x = TRUE
  )
  
  
  return(data_prediction)
}
