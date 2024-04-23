
#' Plot Gini index evolution on microsimulated series
#' 
#' Determine evolution of Gini index
#' 
#' @inheritParams plot_rK_age
#' @param vars Variables that should be represented
#' @param labels Variable labels for legend
#' @param ... Additional arguments to customize \code{ggplot2} call
#' 
#' @return A ggplot object
#' 
#' @importFrom ineq ineq
#' @export

plot_gini <- function(simulations,
                      year_var = "annee",
                      vars = c("y_indiv","wealth",
                               "Y"),
                      labels = c("Labor income", "Financial wealth",
                                 "Total income"),
                      start_year = 2009,
                      final_year = 2040,
                      export_plot = FALSE,
                      export_path = getwd(),
                      export_filename = "04_gini",
                      langage = c("French","English"),
                      scale_viridis = FALSE,
                      negative_values = c("truncate","keep","remove"),
                      ...){
  
  langage <- match.arg(langage)
  negative_values <- match.arg(negative_values)
  
  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")
  
  simulations <- simulations[get(year_var) %between% c(start_year,final_year)]
  
  if (isTRUE(negative_values == "truncate")){
    simulations <- simulations[get('wealth')<0, c('wealth') := 0]
  }
  if (isTRUE(negative_values == "remove")){
    simulations <- simulations[get('wealth')>0]
  }
  
  gini_coeff <- simulations[, lapply(.SD, ineq::ineq, na.rm=TRUE),
                            .SDcols = vars,
                            by = year_var]
  
  # NB: rK et K etant identiques à une transformation linéaire près,
  #  les gini du revenu du capital (rK) et du patrimoine (K)
  #  sont identiques
  
  gini_coeff <- data.table::melt(gini_coeff, id.vars = 'annee')
  gini_coeff <- merge(gini_coeff, data.frame(variable = vars,
                                             label = labels) )
  gini_coeff[,'variable' := NULL]
  data.table::setnames(gini_coeff, old = "label", new = "variable")
  
  ylab <- ifelse(langage == "French",
                 "Indice de Gini",
                 "Gini index")
  
  p <- ggplot2::ggplot(gini_coeff) + 
    ggplot2::geom_line(ggplot2::aes_string(x = "annee",
                                           y = "value",
                                           color = "variable"),
                       ...) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'bottom') +
    ggplot2::labs(x = "Year", y = ylab, color = NULL) +
    ggplot2::theme(text = ggplot2::element_text(size=34))
  
  if (!scale_viridis){
    p <- p +
      ggplot2::scale_color_discrete(labels = labels)
  }else{
    p <- p + ggplot2::scale_color_viridis_d(labels = labels)    
  }
  
  return(p)  
}