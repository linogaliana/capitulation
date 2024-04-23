
#' Add expected inheritance from a statistical model
#' 
#' @param household_table Longitudinal data
#' @param data_event Data storing inheritance events
#' @param tables Destinie's tables
#' @param inheritance_model Inheritance model
#' @return \code{household_table} with two new columns:
#'  \code{H_received} et \code{H_given}
#' @import REtage
#' @importFrom stats predict
#' @export

predict_inheritance_old <- function(household_table,data_event,tables,
                                    inheritance_model){
  
  # Avoid NSE notes
  . <- NULL

  data_prediction <- arrange_inheritance(household_table = household_table,
                                         data_event = data_event,
                                         tables = tables)
  

  data_prediction2 <- data_prediction[,.SD,.SDcols = c("Id","annee","id_death",
                                                       "share",
                                                       "revenu","SEXE","tr_age","tr_agfinetu")]
  
  h_pred <- predict(inheritance_model, newdata = data_prediction2,
                   type = "latent")
  
  data_prediction2[, 'H' := exp(h_pred$y_latent_pred)*exp(-log(h_pred$sigma)^2/2)]
  data_prediction2[,c('H') := get('H')*get('share')]
  data_prediction2 <- data_prediction2[,.SD,.SDcols = c("Id","annee",
                                                        "id_death",
                                                        "H")]
  
  household_table[,'Id' := as.character(get('Id'))]
  data_prediction2[,'Id' := as.character(get('Id'))]
  
  # FIRST INHERITANCE RECEIVED
  household_table <- merge(
    household_table,
    data_prediction2[,.SD,.SDcols = c("Id","annee","H")],
    by = c("Id","annee"),
    all.x = TRUE
  )
  
  
  # SECOND INHERITANCE GIVEN
  data_prediction2[,'id_death' := as.character(get('id_death'))]
  household_table <- merge(
    household_table,
    data_prediction2[,.('H' = sum(get('H'), na.rm = TRUE)),
                     by = c("id_death","annee")],
    by.x = c("Id","annee"),
    by.y = c("id_death","annee"),
    all.x = TRUE,
    suffixes = c("_received","_given")
  )
  
  
  household_table[is.na(get('H_received')), c('H_received') := 0]
  household_table[is.na(get('H_given')), c('H_given') := 0]
  
  return(household_table)
}