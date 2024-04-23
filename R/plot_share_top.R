
#' Plot top earners and owners shares in time
#' 
#' Plot the evolution of time of top owners and
#'  earners share
#' 
#' @inheritParams plot_gini
#' @param thr Threshold for top income/wealth groups
#' @param return_data Logical value indicating whether we
#'  should return values (\code{TRUE}) or a plot
#'  (\code{FALSE}, default)
#' @import ggplot2
#' @import data.table
#' @importFrom stats quantile
#' 
#' @export

plot_share_top <- function(simulations,
                      thr = 0.9,
                      year_var = "annee",
                      vars = c("y_indiv","wealth",
                               "Y"),
                      labels = c("Labor income",
                                 "Financial wealth",
                                 "Total income"),
                      start_year = 2009,
                      final_year = 2040,
                      return_data = FALSE,
                      export_plot = FALSE,
                      export_path = getwd(),
                      export_filename = "04_gini",
                      langage = c("French","English"),
                      scale_viridis = FALSE,
                      ...){

  langage <- match.arg(langage)
  
  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")
  
  simulations <- simulations[get(year_var) %between% c(start_year,final_year)]
  simulations <- simulations[get('wealth')>0]
  
  top_share <- simulations[, lapply(.SD, function(x)
    sum(x[x > quantile(x, probs = thr)])/sum(x)),
                            .SDcols = vars,
                            by = year_var]
  
  top_share <- data.table::melt(top_share, id.vars = 'annee')
  
  
  ylab <- ifelse(langage == "French",
                 paste0("Part du total d\u00E9tenu par le top ", 100*(1-thr),"%"),
                 paste0("Share of total owned by top ", 100*(1-thr),"%"))
  
  if (return_data) return(top_share)
  
  p <- ggplot2::ggplot(top_share) + 
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
  