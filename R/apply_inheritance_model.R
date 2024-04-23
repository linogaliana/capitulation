#' Apply interval regression model on children inheritance
#'   to microsimulated data
#' 
#' @importFrom REtage predict_inheritance
#' 
#' @param household_table Household longitudinal table
#' @param inheritance_model An object estimated by the means
#'  of \link[REtage]{ordered_model_threshold}
#' @param deduction Amount deducted from taxation
#' @param bounds Thresholds for inheritance tax rates brackets
#' @param tax_rates Tax rates
#' @param ... Additional arguments that should be passed
#' @return Initial dataframe with
#' \describe{
#'   \item{y_pred}{Inheritance someone should received given `inheritance_model`
#'     and family structure (his/her right to bequest)}
#' }
#' 
#' 

apply_inheritance_model <- function(household_table, inheritance_model,
                                    selection_model = NULL,
                                    bounds_y_model = NULL,
                                    deduction = 159325L,
                                    bounds = c(8072, 12109, 15932, 552324, 902838,
                                               1805677),
                                    tax_rates =  c(5,10,15,20,30,40,45)/100,
                                    ...){
  
  # Avoid NSE notes
  . <- NULL
  
  
  args <- list(...)
  
  if ('scale' %in% names(args)){
    scale <- args['scale']
  } else{
    scale <- "log"
  }
  
  if ('bias_correction' %in% names(args)){
    bias_correction <- args['bias_correction']
  } else{
    bias_correction <- FALSE
  }
  
  
  # I - PREDICT INHERITANCE RECEIVED ----------------------------
  
  
  ## 1/ APPLY MODEL (PREDICT) ====
  
  prediction_Hr <- data.table::copy(household_table)

  prediction_Hr <- na.omit(prediction_Hr)
  
  
  prediction_Hr <- REtage::predict_inheritance(inheritance_model = inheritance_model,
                                               selection_model = selection_model,
                                               data = prediction_Hr,
                                               scale = scale,
                                               bias_correction = bias_correction)
  
  
  ## 2/ COMBIEN LE MODELE PREVOIT QUE LE DEFUNT DONNE AU TOTAL, AVANT SYSTEMe
  ## SOCIOFISCAL --> harmonise les valeurs au sein de la fratrie ========
  
  prediction_Hr[, c("y_pred") := sum(get('y_pred')*get('share'))*get('share'),
                by = "id_death"]
  
  cols <- c("Id","annee","y_pred")
  if (!is.null(selection_model)) cols <- c(cols, "proba_selection_step1", "y_latent_step2")
    
  predict_inheritance_received <- merge(
    household_table,
    prediction_Hr[,.SD,.SDcols = cols],
    by = c("Id","annee"),
    all.x = TRUE
  )
  
  
  
  # II - FROM CHILDREN TO PARENT INHERITANCE ---------
  
  ## 1/ Use legal rules (tax rates, etc. ) deduce how much
  ##   parents must give to ensure children gets x euros
  ## in bequest
  
  
  data_prediction_parent <- predict_inheritance_received[
    ,.SD,
    .SDcols = c("Id","id_death","share","y_pred")]
  
  data_prediction_parent <- na.omit(data_prediction_parent)

    
  data_prediction_parent[,'H_given' := REtage::taxation_inheritance(
    x = get('y_pred'),
    deduction = deduction,
    bounds_legal = bounds,
    taxes = tax_rates
  )]
  
  
  
  data_prediction_parent[, c('id_death') := as.numeric(get('id_death'))]
  
  inheritance_temp <- data_prediction_parent[,.('H_given' = sum(get('H_given'))),
                                             by = "id_death"]
  
  
  predict_inheritance_received[, c('Id') := as.numeric(get('Id'))]
  
  data.table::setnames(predict_inheritance_received, old = "id_death",
                       new = "parent_dies")
  
  
  data_prediction_augm <- merge(
    predict_inheritance_received, inheritance_temp,
    by.x = 'Id', by.y = 'id_death',
    all.x = TRUE
  )
  
  
  # FILL WITH ZEROS
  data.table::setnames(data_prediction_augm,
                       old = "y_pred",
                       new = "H_received")
  data_prediction_augm[is.na(get('H_received')), 'H_received' := 0]
  data_prediction_augm[is.na(get('H_given')), 'H_given' := 0]
  
  if (is.null(selection_model)) return(data_prediction_augm)
  
  
  data_prediction_augm[, c("proba_selection_step1") := mean(get("proba_selection_step1"), na.rm = TRUE),
                       by = c("Id")]
  data_prediction_augm[, c("y_latent_step2") := mean(get("y_latent_step2"), na.rm = TRUE),
                       by = c("Id")]
  
  data_prediction_augm[,'non_ricardian' := (get("proba_selection_step1") < 0)]
  
  return(data_prediction_augm)
  
}

