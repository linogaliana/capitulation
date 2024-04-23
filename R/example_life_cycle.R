#' Simple example to illustrate life cycle
#'  dynamic
#'
#' @param K0 Initial capital that should be used. If set to
#'  \code{NULL}, deduced recursively given \code{K2009} value
#' @param K2009 Wealth measured in survey data for this
#'  example individual
#' @param r Exogeneous interest rate
#' @param beta Utility discount factor
#' @param gamma Risk aversion coefficient
#' @return \code{ggplot} object
#' 
#' @export
#' @examples \dontrun{example_life_cycle(K0=0)}

example_life_cycle <- function(r = .02,
                               gamma=.5,
                               beta = 1,
                               K0 = c(NULL,0),
                               K2009 = 100,
                               time_0 = c("graduation","birth"),
                               income_seq = NULL,
                               seed = NULL,
                               return_data = FALSE){
  
  if (!is.null(seed)) set.seed(seed)  
  
  if (missing(K2009) & missing(K0)) K0 <- 0
  if (missing(K0)) K0 <- NULL
  time_0 <- match.arg(time_0)
  
  z <- data.table::data.table(
    Id = rep(1,80),
    annee = 1980:2059,
    age = 0:79,
    findet = 20
  )
  
  if (time_0 == "graduation") z <- z[age>=findet]
  
  z[,'tau' := seq_len(nrow(z))]
  
  if (is.null(income_seq)){
    z[,'salaire_tot' := lest::case_when(
      get('age')<60 ~ get('age')-20 + exp(rnorm(length(get('age')<60))),
      TRUE ~ 0
    )]
  } else{
    z[,'salaire_tot' := income_seq]
  }
  
  if (time_0 == "birth")  z[age<findet, c('salaire_tot') := 0]
  
  z[,'UC' := 1]
  
  if (!is.null(K0)){
    
    C0_given_K0(z, r = r,gamma = gamma, beta = beta,
                K0 = K0, timeIndex_var = 'tau')
    z[,'K0' := K0]
    
  } else{
    
    z[,'K2009' := lest::case_when(
      get('annee') == 2009 ~ 100,
      TRUE ~ NA_real_
    )]
    
    
    z <- estimate_K0(data = z,
                     r = r,
                     gamma = gamma,
                     beta = beta,
                     wealthvar_survey = "K2009")    
  }
  
  if (is.null(gamma)){
    z[, 'K' := simulate_wealth_structural(K0 = get('K0'),
                                          consumption0 = get('C0'),
                                          income = get('salaire_tot'),
                                          UC = get('UC'),
                                          r = r,
                                          returnLast = TRUE)[-1], by = c('Id')]
  } else{
    z[, 'K' := simulate_wealth_structural_beta(K0 = get('K0'),
                                               consumption0 = get('C0'),
                                               income = get('salaire_tot'),
                                               UC = get('UC'),
                                               r = r, gamma = gamma,
                                               beta = beta,
                                               returnLast = TRUE)[-1], by = c('Id')]
  }
  
  
  z_augm <- death_obs(z)
  
  
  z_augm[,'K' := c(mean(get('K0'),na.rm=TRUE),
                   z$K
  )]
  
  
  tempdf <- z_augm[,.SD,.SDcols = c('age', 'salaire_tot', 'K', 'C0')]
  
  
  
  if (!is.null(gamma)){
    C <- rep(NA, nrow(tempdf))
    lapply(seq_len(nrow(tempdf)), function(i)({
      if (i==1) a <- mean(tempdf$C0,na.rm=TRUE) else a <<- C[i-1]*(1+r)^(1/gamma) 
      C[i] <<- a
    }))
    tempdf[,'C' := C]
    tempdf[,'C0' := NULL]
    tempdf[,'income' := get('salaire_tot') + r*get('K')]
  } else{
    data.table::setnames(tempdf,old = 'C0',new = 'C')
    tempdf[,'C' := mean(get('C'),na.rm=TRUE)]
    tempdf[,'income' := get('salaire_tot') + r*get('K')]
  }
  
  
  tempdf <- data.table::melt(tempdf, id.var = 'age')
  
  p <- ggplot2::ggplot(tempdf) +
    ggplot2::geom_line(ggplot2::aes_string(x = "age", y = "value", color = "variable"), size = 1) +
    ggplot2::geom_vline(xintercept = 60, linetype = 'dashed') +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black", 
                                                     size = .5, linetype = "solid")) +
    ggplot2::labs(x = 'Age', y = "Thousand euros", color = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size=34))
  
  
  if (is.null(K0)){
    age_2009 <- z_augm[get('annee')==2009L][['age']]
    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = 20, y = K2009, xend = age_2009, yend = K2009),
                            linetype = "dashed", color = "black") +
      ggplot2::geom_segment(ggplot2::aes(x = age_2009, y = 0, xend = age_2009, yend = K2009),
                            linetype = "dashed", color = "black")
  }
  
  p <- p + ggplot2::scale_x_continuous(expand = c(0, 0)) + ggplot2::scale_y_continuous(expand = c(0, 0))
  
  p <- p  + ggplot2::scale_colour_viridis_d(labels=c("Labor income", "Wealth",
                                                     "Consumption", "Total income (rK+w)"),
                                            option = "plasma")  +
    ggplot2::theme(legend.position=c(0.25, 0.8), legend.background = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(1, "cm"))
  
  if (isTRUE(return_data)) return(z_augm)
  
  return(p)
}


death_obs <- function(data, C0var = 'C0'){
  
  data2 <- rbind(data, data.table::data.table(Id = unique(data$Id),
                                              annee = max(data$annee)+1,
                                              age = max(data$age)+1,
                                              findet = unique(data$findet),
                                              salaire_tot = 0,
                                              K2009 = NA,
                                              UC = 1,
                                              C0var = mean(data[[C0var]])
  ),
  fill = TRUE
  )
  
  return(data2)
}
