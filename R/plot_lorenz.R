
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

plot_lorenz <- function(simulations,
                      year_var = "annee",
                      year = 2020,
                      wealth_var = "wealth",
                      labor_income_var = "revenu",
                      total_income_var = "Y",
                      export_plot = FALSE,
                      export_path = getwd(),
                      export_filename = "05_lorenz",
                      langage = c("French","English"),
                      scale_viridis = FALSE,
                      negative_values = c("truncate","keep","remove"),
                      ...){
  
  langage <- match.arg(langage)
  negative_values <- match.arg(negative_values)
  
  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")
  
  if (isTRUE(negative_values == "truncate")){
    simulations <- simulations[get(wealth_var)<0, c(wealth_var) := 0]
  }
  if (isTRUE(negative_values == "remove")){
    simulations <- simulations[get(wealth_var)>0]
  }
  
  
  
  LC1 <- ineq::Lc(simulations[get(year_var)==year][[wealth_var]], plot = FALSE)
  LC2 <- ineq::Lc(simulations[get(year_var)==year][[labor_income_var]], plot = FALSE)
  LC3 <- ineq::Lc(simulations[get(year_var)==year][[total_income_var]], plot = FALSE)
  df <- data.frame(p1 = LC1$p,
                   wealth = LC1$L,
                   uniform = LC1$p, 
                   labor = LC2$L,
                   total = LC3$L)
  df <- data.table::data.table(reshape2::melt(df, id.vars = "p1"))
  
  df[, 'lab' := "Pure equality"]
  df[get('variable') == "wealth", c('lab') := "Financial Wealth"]
  df[get('variable') == "labor", c('lab') := "Labor income"]
  df[get('variable') == "total", c('lab') := "Total income"]
  
  
  p <- ggplot2::ggplot(df) +
    ggplot2::geom_line(ggplot2::aes(x = p1, y = value, color = lab))+
    ggplot2::theme(legend.position="bottom") +
    ggplot2::labs(x = "", y = "", color = "") +
    ggplot2::theme(text = ggplot2::element_text(size=24),
                   axis.title = ggplot2::element_text(size=20,face="bold"))
  
  if (isTRUE(scale_viridis)) p <- p + ggplot2::scale_color_viridis_d()
  
  return(p)  
}