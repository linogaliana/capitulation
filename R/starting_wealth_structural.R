
#' [Discarded] Determine first generation wealth given observation in
#'  survey data (structural approach)
#'  
#'  This function is discarded
#'
#' @param r Exogeneous interest rate
#' @param gamma Risk aversion coefficient
#' @param beta Discount factor
#' @param wealthvar_survey Wealth variable in household survey
#' @param data Household level longitudinal trajectories
#' @param deflate Logical value indicating whether we want
#'  to use nominal (\code{FALSE}) or real (\code{TRUE}, default) series
#' @param weight_var Variable storing individual weight inside household
#'  (necessary to ensure consistency between household and
#'  individual levels)

starting_wealth_structural <- function(data,
                                       deflate = TRUE,
                                       wealthvar_survey = 'PATFISOM',
                                       r = 0.02,
                                       gamma = 0.5,
                                       beta = 1,
                                       weight_var = NULL){
  
  
  
  # # MATCH DESTINIE WITH ENQUETE PATRIMOINE
  # household_table <- match_wealthSurvey(data, path_survey = path_survey)
  # 
  # # WEALTH CONCEPT: NOMINAL OR REAL
  # if (deflate)
  #   household_table[, (wealthvar_survey) := get(wealthvar_survey)/get('Prix')]
  
  
  income_var <- if (deflate) "y_real" else "salaire_tot"
  
  # CALIBRATE K0 FOR PEOPLE OBSERVED IN 2009
  tempdf <- estimate_K0(data = data,
                        r = r,
                        gamma = gamma,
                        beta = beta,
                        wealthvar_survey = wealthvar_survey,
                        income_var = income_var,
                        weight_var = weight_var)
  
  return(tempdf)
}
