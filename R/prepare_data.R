#' Prepare data for microsimulation of capitulation accumulation
#' 
#' \code{prepare_data} is a global function used to perform
#'  several data cleaning steps, see \code{details} section
#'  
#'  @details 
#'  
#' \code{prepare_data} executes sequentially:
#' \enumerate{
#'  \item \link{import_Destinie}, \link{arrange_Destinie}
#'   and \link{prepare}
#'    to read and arrange \code{Destinie} data into \code{R}
#'  \item \link{household_composition} and \link{income_household}
#'   to join household composition to individual information
#'  \item \link{match_wealthSurvey} to merge individual microsimulated
#'   trajectory with information observed in wealth survey
#'  \item Data cleaning to keep only useful information for
#'   life-cycle model
#' }
#' 
#' @inheritParams import_Destinie
#' @inheritParams arrange_Destinie
#' @inheritParams prepare
#' @inheritParams estimate_K0
#' @inheritParams assign_referent
#' @inheritParams transform_household
#' @inheritParams match_wealthSurvey
#' @inheritParams assign_children
#' @param findet_var End of studying year variable
#' @param weight_var Variable name to weight observations. 
#'  By default, set to \code{NULL} to consider individual data
#' @inheritParams income_group
#' @param inheritance_model A model that presents a `predict` method
#'  that can be used to predict inheritance
#' @param deflate Logical value indicating whether values should
#'  be deflated
#' @param ... Additional parameters that should be passed. Currently
#'  not used
#' 
#' @return Individual table finalized, ready for microsimulation.
#'  It is stored as a list
#' @export

prepare_data <- function(path_data = "./inst/dataINSEE",
                         path_data_suffix = "/Destinie",
                         extension = ".Rda",
                         inheritance_model = NULL,
                         selection_model = NULL,
                         call.Rcpp = TRUE,
                         estimation_year = 2009,
                         id_var = "Id",
                         income_var = "salaire",
                         age_var = "age",
                         ageliq_var = "ageliq",
                         matrimonial_var = "matri",
                         wealthvar_survey = "PATFISOM",
                         debt_wealthSurvey = NULL,
                         findet_var = "findet",
                         weight_var = NULL,
                         deflate = TRUE,
                         quantile = 0.9,
                         groupvar = "top",
                         time_0 = c("graduation","birth"),
                         drawK = TRUE,
                         taille_tr_age = 5,
                         taille_tr_agfinetu = 2,
                         ...){
  
  args <- list(...)
  
  time_0 <- match.arg(time_0)
  
  # =========================================
  #         I - IMPORT DESTINIE
  # =========================================
  
  simul <- import_Destinie(path_data = paste0(path_data, path_data_suffix),
                           format = "data.table", extension = extension)
  
  tables <- arrange_Destinie(simul)
  
  # Put those tables into global environment rather than inside a list
  #list2env(tables,globalenv())
  
  
  # =========================================
  #       II - TRANSFORM INDIVIDUAL TRAJECTORY
  #           INTO HOUSEHOLD LEVEL DATA
  # =========================================
  
  
  pre_indiv <- prepare(table_indiv = tables$pre_indiv,
                       pensions = tables$pensions,
                       descript = tables$description)
  
  
  indiv <- household_composition(table_indiv = pre_indiv,
                                 simul = tables,
                                 descript =  tables$description,
                                 call.Rcpp = TRUE)
  
  data_event <- death_event(indiv)
  
  indiv <- income_household(table_indiv = indiv)
  
  
  indiv <- match_wealthSurvey(indiv,
                              path_survey =  path_data,
                              drawK = drawK,
                              time_0 = time_0,
                              wealth_wealthSurvey = wealthvar_survey,
                              debt_wealthSurvey = debt_wealthSurvey)
  
  
  macro <- simul$macro[,.SD,.SDcols = c('annee','Prix')]
  
  # GET REAL VALUES
  household_table <- merge(indiv, macro, by = 'annee')
  household_table[,'y_real' := get('salaire_tot')/get('Prix')]
  
  
  # WEALTH CONCEPT: NOMINAL OR REAL
  if (deflate)
    household_table[, (wealthvar_survey) := get(wealthvar_survey)/get('Prix')]
  
  # KEEP ONLY OBSERVATION AFTER STARTING WORKING LIFE
  if (time_0 == "graduation") household_table <- household_table[get(age_var)>=get(findet_var)]
  
  
  household_table[,`:=` ('y_indiv' = get('y_real')/get('nspouses'),
                         'K_observed' = get(wealthvar_survey)/get('nspouses')
  )]
  
  household_table[, c("K_observed") := mean(get("K_observed"),na.rm=TRUE), by = c('Id')]
  
  if (is.null(weight_var)){
    household_table[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  id_household <- as.character(
    household_table[, paste0(do.call(pmin, .SD), "_", do.call(pmax, .SD)), .SDcols = c("Id",'conjoint')]
  )
  household_table[, 'id_household' := id_household]

  
  household_table <- household_table[,.SD,
                                     .SDcols = c(id_var,"annee", 'id_household',
                                                 'y_indiv',
                                                 'K_observed', age_var,
                                                 ageliq_var,
                                                 "tt", #weight_var,
                                                 "sexe", "UC"
                                                 #findet_var
                                     )]  
  
  
  household_table <- income_group2(household_table,
                                  quantile = quantile,
                                  groupvar = groupvar,
                                  incomevar = "y_indiv",
                                  yearvar = "annee"
  )
  
  
  # FOR PEOPLE THAT ONLY LIVE ONE YEAR
  household_table[is.nan(get("K_observed")), "K_observed" := 0L]
  
  
  if (is.null(inheritance_model)) return(household_table)
  

  print("Preparing everything to simulate inheritance")
  
  
  household_table <- arrange_inheritance(
    household_table,
    data_event = data_event,
    tables = tables,
    time_0 = time_0,
    taille_tr_age = taille_tr_age,
    taille_tr_agfinetu = taille_tr_agfinetu
  )
  

  if (sum(grepl('SEXE', colnames(household_table))) == 0) household_table[,'SEXE' := factor(get('sexe.x'))]

  print("Simulate inheritance")
  

  household_table2 <- apply_inheritance_model(household_table, inheritance_model,
                                              selection_model = selection_model)
  
  
  # 
  # 
  # # 
  # 
  # household_table <- predict_inheritance(household_table = household_table,
  #                                        data_event = data_event,
  #                                        tables = tables)
  # 
  # 
  # 
  # data_prediction <- arrange_inheritance(household_table = household_table,
  #                                        data_event = data_event,
  #                                        tables = tables)
  # 
  # 
  
  return(household_table2)
  
}
