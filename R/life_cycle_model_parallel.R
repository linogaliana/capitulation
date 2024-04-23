

#' Life cycle model parallelized at \code{R} level
#' 
#' This function is the \code{R}-level function
#'  to run life-cycle model
#'  
#' @inheritParams life_cycle_model_old
#' @param return_last Logical value indicating whether we want the sequence
#'  of wealth to be \eqn{\{K_0,...,K_{T-1}\}} (\code{FALSE}, default)
#'  or  \eqn{\{K_1,...,K_{T}\}} (\code{TRUE})
#' @export

life_cycle_model_parallel <- function(data,
                                      r = 0.02,
                                      beta = 1,
                                      gamma = 0.5,
                                      observation_year = 2009,
                                      id_var = "Id",
                                      year_var = "annee",
                                      age_var = "age",
                                      wealthvar_survey = "PATFISOM",
                                      income_var = "salaire_tot",
                                      weight_var = NULL,
                                      return_last = FALSE,
                                      get_capital_income = FALSE){
  
  
  # Avoid NSE notes
  .<-NULL
  g <- NULL
  
  if (is.null(weight_var)){
    data[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  traj_complete <- data[,.SD,
                        .SDcols = c(id_var,year_var,
                                    age_var,
                                    income_var,
                                    wealthvar_survey,
                                    "tt", weight_var)]
  
  # Transform wealth into vector variables from Enquete Patrimoine
  # traj_complete[, `:=` (wealth2009 = mean(get(wealthvar_survey),na.rm=TRUE)), by = c('Id')]
  # 
  # traj_complete[,'tt' := as.numeric(get('annee')==observation_year)]
  
  
  # traj_complete <- traj_complete[!is.na(get(income_var))]
  # traj_complete[is.nan(get("wealth2009")), ("wealth2009") := NA_real_]
  
  
  data.table::setkeyv(traj_complete, id_var)
  
  
  traj_complete <- split(traj_complete, by = id_var)
  
  
  # CLUSTER INITIALIZATION  
  cl <- parallel::makeCluster(parallel::detectCores()-1,
                              outfile = "")
  doSNOW::registerDoSNOW(cl)
  
  # pb <- txtProgressBar(max = length(traj_complete), style = 3)
  # progress <- function(n) setTxtProgressBar(pb, n)
  # opts <- list(progress = progress)
  
  output <- foreach::foreach(g = traj_complete, .combine = "list",
                             .multicombine = TRUE,
                             .maxcombine = length(traj_complete)#,
                             # .options.snow=opts
                             #                       .export = c("descript"),
                             #                       .packages = c("dplyr")
  ) %dopar% {
    
    g[, `:=`('wealth' = life_cycle_model_cpp2_old(income = get(income_var),
                                              K2009vector = get(wealthvar_survey),
                                              timeIndex = get("tt"),
                                              UC = get(weight_var),
                                              r = r,
                                              gamma = gamma,
                                              beta = beta,
                                              returnLast = return_last)
    )]
    
  }
  
  
  # CLOSE CLUSTERS  
  parallel::stopCluster(cl)
  
  
  traj_complete <- data.table::rbindlist(output)
  
  if (!get_capital_income) return(traj_complete)
  
  wealth_simulation <- capital_income(simulations = traj_complete,
                                      wealth_var = "wealth",
                                      income_var = income_var,
                                      r = r)
  return(wealth_simulation)
}