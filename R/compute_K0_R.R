#' Given measured wealth in survey, estimate/calibrate K0 (starting wealth). \code{calibrate_K0} for version
#' without risk aversion and  \code{calibrate_K0_ra} for the version with risk aversion
#' 
#' @details
#' Calibrate K0 and C0 such that (i) individual leaves no wealth when he/she dies
#'  (ii) wealth level is consistent with observed level in 2009 (iii) consumption
#'  is fixed at C0 level. This function is programmed with R and can be compared
#'  with the \code{C++} implementation (\link{calibrate_K0})
#'  \code{calibrate_K0} is designed for risk neutral agents, \code{calibrate_K0_ra}
#'   for risk adverse agents
#' 
#' In pure permanent income approach with no inheritance, consumption path
#'  is fixed at a level such that agent does not leave any inheritance. The
#'  relationship between \eqn{K(t)} and \eqn{K(t-1)} is given by \eqn{K_t = (1+r)(K_{t-1}+Y_t-C_0)}
#'  with \eqn{C_0 = \frac{K_0+\sum_{t=0}^{Ti-1} \beta^t Y_t}{\sum_{t=0}^{Ti-1} \beta^t}}. By
#'  inverse recursion, K(0) can be determined from K(t)
#' @param df \link[data.table]{data.table} object
#' @param yvar Income vector
#' @param var_wealth_survey Measured wealth variable
#' @param estimation_year Year where wealth is measured in survey
#' @param r Exogeneous interest rate
#' @return List where first element (\code{K0}) is initial wealth
#'  and second element (\code{C0}) is life-cycle consumption
#' @seealso \link{calibrate_K0} for the alternative \code{C++} function,
#'  \link{simulate_wealth_structural}
#'  for the dynamics of capital accumulation
#' @export
calibrate_K0_R <- function(df,
                           yvar = "salaire_tot",
                           var_wealth_survey = 'PATFISOM',
                           estimation_year = 2009,
                           r=0.02){
  
  # TimeVector
  df[,'tau' := .I-1, by = c('Id')]

  # Capitalized income used in summations
  df[, `:=` (betaY = (1+r)^(-get('tau'))*get(yvar),
             beta = (1+r)^(-get('tau')))]

  # Measured wealth in survey data
  Kt <- mean(df[[var_wealth_survey]],na.rm=TRUE)
  
  # Time index at estimation year
  t <- df[get('annee')==estimation_year]$tau
  
  # Death period
  Ti <- max(df$tau)+1

  # First term
  a <- sum(df[get('annee')<estimation_year]$betaY) 
  
  # Second term
  b <- sum(df$betaY)
  
  # K0
  K0 <- Kt - (a*(1+r)^t + b*(1 - (1+r)^t)/(1 - (1+r)^(-Ti)))
  K0 <- K0 * (1-(1+r)^(-Ti))/(1 - (1+r)^(t-Ti))

  # Consumption level C0  
  C0 <- (K0 + sum(df$betaY))/sum(df$beta)
  
  return(list(K0,C0))
}


#' @rdname calibrate_K0_R
#' @inheritParams calibrate_K0
#' @export
calibrate_K0_R_ra <- function(df,
                           yvar = "salaire_tot",
                           var_wealth_survey = 'PATFISOM',
                           estimation_year = 2009,
                           r=0.02,
                           gamma = 0.5){
  
  # TimeVector
  df[,'tau' := .I-1, by = c('Id')]
  
  # Capitalized income used in summations
  df[, `:=` (betaY = (1+r)^(-get('tau'))*get(yvar),
             beta = (1+r)^((1-gamma)*get('tau')/gamma),
             R = (1+r)^(get('tau')-get('tau')/gamma))]
  
  # Measured wealth in survey data
  Kt <- mean(df[[var_wealth_survey]],na.rm=TRUE)
  
  # Time index at estimation year
  t <- df[get('annee')==estimation_year]$tau
  
  # Death period
  Ti <- max(df$tau)+1
  
  # First term
  a <- sum(df[get('annee')<estimation_year]$betaY) 
  
  # Second term
  b <- sum(df$betaY)
  
  # Terms in denominator
  denom1 <- (1+r)^(t/gamma)*
    sum(df[get('annee')<=estimation_year & get('tau')>0]$R)
  c <- sum(df$beta)
  
  denom <- (1+r)^t - denom1/c
  
  # K0
  K0 <- Kt - a*(1+r)^t + b*denom1/sum(df$beta)
  K0 <- K0/denom
    
  # Consumption level C0  
  C0 <- (K0 + sum(df$betaY))/sum(df$beta)
  
  return(list(K0,C0))
}
