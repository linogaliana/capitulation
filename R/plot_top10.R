#' @export

plot_top_share <- function(simulations,
                      year_var = "annee",
                      threshold = 0.9,
                      vars = c("revenu","wealth",
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
  
  topvars <- c("top_labor", "top_wealth", "top_income")
  
  
  simulations[,c(topvars) := lapply(.SD, function(x) x > quantile(x, na.rm = TRUE, probs = threshold)),
              .SDcols = vars, by = year_var]
  simulations[,'top_labor' := as.character(data.table::fifelse(get('top_labor'),
                                                 sprintf('top%s labor income', 100*(1 - threshold)),
                                                 'others'))]
  simulations[,'top_income' := as.character(data.table::fifelse(get('top_income'),
                                                               sprintf('top%s total income', 100*(1 - threshold)),
                                                               'others'))]
  simulations[,'top_wealth' := as.character(data.table::fifelse(get('top_wealth'),
                                                               sprintf('top%s wealth', 100*(1 - threshold)),
                                                               'others'))]
  
  
  dfs <- lapply(seq_len(3), function(pos){
    byvar <- topvars[pos]
    tempdf <- simulations[, .('share' = sum(get(vars[pos]), na.rm = TRUE)), by = c(byvar, year_var)]
    tempdf[,c('share') := get('share')/sum(get('share')), by = year_var]
    tempdf <- tempdf[get(byvar) != "others"]
    data.table::setnames(tempdf, old = byvar, new = "category")
  })
  
  graphdf <- data.table::rbindlist(dfs, use.names = TRUE, fill = TRUE)
  
  
  ggplot2::ggplot(graphdf) +
    ggplot2::geom_line(ggplot2::aes(x = annee, y = share, color = category))
}
