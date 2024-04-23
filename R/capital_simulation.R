
capital_simulation_old <- function(data,
                               method = "structural",
                               path_data = "./inst/dataINSEE",
                               start_year = 2009,
                               estimation_year = 2009,
                               final_year = 2070,
                               id_var = "Id",
                               income_var = "salaire",
                               age_var = "age",
                               ageliq_var = "ageliq",
                               matrimonial_var = "matri",
                               findet_var = "findet",
                               wealthvar_survey = "PATFISOM",
                               deflate = TRUE,
                               UCvar = c(NULL,"UC"),
                               r = 0.03,
                               gamma = 0.5,
                               beta = 0.98,
                               verbose = TRUE){
  
  
  if (method != "structural")
    stop("Only structural approach is implemented for the moment")
  
  
  UCvar <- match.arg(UCvar)

  
  # ================================
  #   PART I: PREPARE DATA
  # ================================
  
  
  if (verbose) message("------- Part 1: Prepare data -------")
  
  # if (is.null(data)){
  #   
  #   data <- prepare_data(
  #     path_data = paste0(path_data,"/Destinie"),
  #     start_year = start_year,
  #     estimation_year = estimation_year,
  #     final_year = final_year,
  #     id_var = id_var,
  #     income_var = income_var,
  #     age_var = age_var,
  #     ageliq_var = ageliq_var,
  #     matrimonial_var = matrimonial_var,
  #     UCvar = UCvar,
  #     deflate = deflate,
  #     wealthvar_survey = wealthvar_survey
  #   )
  #   
  # }
  
  
  if ('UC' %in% colnames(data)){
    data[,'UC' := 1]   
  }
  
  # ================================
  #   PART II: ESTIMATE K0
  # ================================
  
  if (verbose) message("------- Part 2: Estimate K0 -------")
  
  
  if (verbose) message(
    paste0(
      "Estimate K0 from  ", estimation_year,
      " wealth survey"
    )
  )
  
  if (beta*(1+r)==1)
    message("beta*(1+r)=1: Risk aversion does not affect consumption path")
  
  data_structural <- starting_wealth_structural(
    data = data,
    deflate = deflate,
    wealthvar_survey = wealthvar_survey,
    r = r,
    gamma = gamma,
    beta = beta
    )

  # WE NO LONGER NEED YEARS BEFORE 2009  
  data_structural2 <- data_structural[get('annee')>=2009]
  data_structural2[,'tau' := get(age_var)-get(findet_var)]

  # ================================
  #   PART III: SIMULATE K
  # ================================
  
  income_var <- if (deflate) "y_real" else "salaire_tot"
  
  if (verbose) message("------- Part 3: Simulations -------")
  
  if (verbose) message(
    paste0(
      "Simulations from ",
      start_year, " to ", final_year
    )
  )
  
  # # NOW THAT K0 IS ESTIMATED, RESTRICTIONS CAN BE MADE
  # data_structural <- data_structural[get('annee') <= max(years)]
  
  
  wealth_simulation <- wealth_accumulation(
    household_table = data_structural2,
    years = seq(from = start_year, to = final_year, by = 1),
    start_year = start_year,
    income_var = income_var,
    r = r,
    gamma = gamma,
    beta = beta,
    verbose = verbose
  )
  
  wealth_simulation <- capital_income(simulations = wealth_simulation,
                 wealth_var = "wealth",
                 income_var = income_var,
                 r = r)
  
  
  
  return(wealth_simulation)
  
}




#' Wealth simulations based on life-cycle approach
#' 
#' This global function (i) shapes Destinie data if needed
#'  (ii) estimate initial wealth for first generation
#'  (iii) simulate wealth for first and following generations
#' 
#' @param start_year Starting year
#' @param findet_var End of studying year variable
#' @param return_last Logical value indicating whether we should
#'  return \eqn{Kt} sequence up to `T-1` (default) or `T`
#' @inheritParams prepare_data
#' @inheritParams starting_wealth_structural
#' @inheritParams wealth_accumulation
#' @inheritParams capital_income
#' @param data If a dataset has already been prepared for microsimulation
#'  use it to save time. Otherwise set \code{data} to \code{NULL} and
#'  \link{prepare_data} will be used to import and reshape Destinie
#'  simulations
#' @param method Modelization adopted. For the moment, only
#'  \emph{structural} approach is implemented
#' @param deflate Logical value indicating whether values
#'  should be deflated
#' 
#' @return \link[data.table]{data.table} object with
#'  wealth simulations between \code{start_year} and
#'  \code{final_year}
#' @export

capital_simulation <- function(data,
                               method = "structural",
                               start_year = 2009,
                               estimation_year = 2009,
                               final_year = 2070,
                               id_var = "Id",
                               income_var = "salaire",
                               age_var = "age",
                               ageliq_var = "ageliq",
                               matrimonial_var = "matri",
                               findet_var = "findet",
                               wealthvar_survey = "PATFISOM",
                               deflate = TRUE,
                               r = 0.03,
                               gamma = 0.5,
                               beta = 0.98,
                               verbose = TRUE,
                               weight_var = NULL,
                               return_last = FALSE){
  
  
  if (method != "structural")
    stop("Only structural approach is implemented for the moment")
  
  

  # ================================
  #   PART I: PREPARE DATA
  # ================================
  
  
  if (verbose) message("------- Part 1: Prepare data -------")
  
  # if (is.null(data)){
  #   
  #   data <- prepare_data(
  #     path_data = paste0(path_data,"/Destinie"),
  #     start_year = start_year,
  #     estimation_year = estimation_year,
  #     final_year = final_year,
  #     id_var = id_var,
  #     income_var = income_var,
  #     age_var = age_var,
  #     ageliq_var = ageliq_var,
  #     matrimonial_var = matrimonial_var,
  #     UCvar = UCvar,
  #     deflate = deflate,
  #     wealthvar_survey = wealthvar_survey
  #   )
  #   
  # }
  
  
  # ================================
  #   PART II: ESTIMATE K0
  # ================================
  
  if (verbose) message("------- Part 2: Estimate K0 -------")
  
  
  if (verbose) message(
    paste0(
      "Estimate K0 from  ", estimation_year,
      " wealth survey"
    )
  )
  
  if (beta*(1+r)==1)
    message("beta*(1+r)=1: Risk aversion does not affect consumption path")
  
  data_structural <- starting_wealth_structural(
    data = data,
    deflate = deflate,
    wealthvar_survey = wealthvar_survey,
    r = r,
    gamma = gamma,
    beta = beta,
    weight_var = weight_var
  )
  
  # WE NO LONGER NEED YEARS BEFORE 2009  
  data_structural2 <- data_structural[get('annee')>=2009]
  data_structural2[,'tau' := get(age_var)-get(findet_var)]
  
  # ================================
  #   PART III: SIMULATE K
  # ================================
  
  income_var <- if (deflate) "y_real" else "salaire_tot"
  
  if (verbose) message("------- Part 3: Simulations -------")
  
  if (verbose) message(
    paste0(
      "Simulations from ",
      start_year, " to ", final_year
    )
  )
  
  # # NOW THAT K0 IS ESTIMATED, RESTRICTIONS CAN BE MADE
  # data_structural <- data_structural[get('annee') <= max(years)]
  
  
  wealth_simulation <- wealth_accumulation(
    household_table = data_structural2,
    years = seq(from = start_year, to = final_year, by = 1),
    wealthvar_survey = wealthvar_survey,
    start_year = start_year,
    income_var = income_var,
    r = r,
    gamma = gamma,
    beta = beta,
    verbose = verbose,
    return_last = return_last,
    weight_var = weight_var
  )
  
  wealth_simulation <- capital_income(simulations = wealth_simulation,
                                      wealth_var = "wealth",
                                      income_var = income_var,
                                      r = r)
  
  
  
  return(wealth_simulation)
  
}
