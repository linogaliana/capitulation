
#' Automatize table for top income/wealth shares
#' 
#' @inheritParams plot_share_top
#' @param year Year that should be used
#' @param filename A filename (including directory path).
#'  If \code{NULL}, table is returned and file is not
#'  written
#' 
#' @return Depending on the value of \code{filename}
#' @importFrom xtable xtable
#' @export




table_shares <- function(simulations, 
                         thr = c(0.9, 0.99), 
                         vars = c("y_indiv","wealth","Y"), 
                         labels = c("Labor income",
                                    "Financial wealth", "Total income"),
                         year = 2015L,
                         year_var = "annee",
                         filename = NULL){
  
  stopifnot(length(thr)==2L)
  
  tempddf1 <- plot_share_top(simulations, thr = thr[1],
                             vars = vars,
                             labels = labels,
                             start_year = year,
                             scale_viridis = TRUE, return_data = TRUE)
  tempddf1[get(year_var)==year]
  
  
  tempddf2 <- plot_share_top(simulations, thr = thr[2],
                             vars = vars,
                             labels = labels,
                             start_year = year,
                             scale_viridis = TRUE, return_data = TRUE)
  
  tempdf <- rbind(
    tempddf1[get(year_var)==year][,'top' := 10L],
    tempddf2[get(year_var)==year][,'top' := 1L]
  )
  
  tempdf[,'value' := 100*get('value')]
  
  tempdf <- data.table::dcast(tempdf, "... ~ variable", value.var = "value")
  data.table::setcolorder(tempdf, c("top",vars))
  tempdf <- tempdf[order(-get('top'))]
  tempdf[,c(year_var):=NULL]
  data.table::setnames(tempdf, old = c("top",vars),
                       new = c("Group",labels))
  tempdf[,'Group' := as.character(paste0("Top ", get('Group'), " %"))]
  
  if (!is.null(filename)){
    print(
      xtable::xtable(tempdf, caption = "Top income and wealth shares simulated",
                     label = "tab: Top income and wealth shares simulated"),
      include.rownames = FALSE,
      file = filename
    )
  }
  
  return(tempdf)
  
}