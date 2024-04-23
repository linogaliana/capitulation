#' Estimate capital share by age or income
#'
#' @param simulations Longitudinal household level dataframe
#'  storing wealth simulations
#' @param year_var Year variable
#' @param estimation_year Estimation year
#' @param start_year First year of simulation
#' @param final_year Last year of simulation
#' @param export_plot Logical value indicating whether we want
#'  to export the plot (\code{TRUE}, default) or not
#' @param export_path Path to export data
#' @param export_filename Filename if data are exported. PDF extension
#'  will be added if missing
#' @param langage Should we label axes in English or French ?
#' @param scale_viridis Logical value. Should we use viridis (\code{TRUE})
#'  or not not (\code{FALSE})
#' @param byvar Optional variable name to separate in two figures
#' @param size_text Parameter for text size in output
#' @return A \code{ggplot} object. Saved on disk if \code{export_plot=TRUE}
#'
#' @import scales
#' @export

plot_rK_age <- function(simulations,
                        wealth_var = 'rKY',
                        year_var = "annee",
                        age_var = "age",
                        estimation_year = 2009,
                        method = c('smooth','median'),
                        start_year = 2009,
                        final_year = 2040,
                        export_plot = FALSE,
                        export_path = getwd(),
                        export_filename = "04_art_rev_capital_by_age",
                        langage = c("French","English"),
                        ylab = NULL,
                        scale_viridis = FALSE,
                        byvar = NULL, size_text = 32,
                        xlims = NULL){

  langage <- match.arg(langage)

  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")

  if (method != "smooth"){
    simulations2 <- simulations[, .("wealth" = median(get(wealth_var), na.rm = TRUE)),
                                 by = c(year_var, age_var)]
    data.table::setnames(simulations2, old = "wealth", new = wealth_var)
  }
  
  simulations2[,'size' := .5 + 0.5*as.numeric(get(year_var) == estimation_year)]

  simulations2[,'annee2' := factor(get(year_var))]

  simulations2 <- simulations2[get(year_var) %between% c(start_year,final_year)]

  
  
  if (!is.null(xlims)) simulations2 <- simulations2[get(age_var) %between% xlims]

  simulations2 <- simulations2[!is.nan(get("rKY"))]
  simulations2 <- simulations2[get('rKY')>0 & get('rKY')<1]

  if (is.null(ylab)) ylab <- ifelse(langage == "French",
                 "Part des revenus du patrimoine dans le revenu total",
                 "Capital income share in total income")

  p <- ggplot2::ggplot(simulations2) +
    ggplot2::geom_smooth(ggplot2::aes_string(x = "age", y = "rKY", color = "annee2", size = "size"), se = FALSE)

  if (!is.null(byvar)) p <- p + ggplot2::facet_wrap(~simulations2[[byvar]])

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(x = "Age", y = ylab) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme(text = ggplot2::element_text(size=24),
                   axis.title = ggplot2::element_text(size=size_text,face="bold"))

  if (scale_viridis) p <- p + ggplot2::scale_color_viridis_d()

  if (export_plot)
    ggplot2::ggsave(
      paste0(export_path,"/",export_filename,".pdf"), height = 12, width = 8,
      device = 'pdf', plot = p
    )

  return(p)
}


#' Estimate financial wealth by age
#'
#' @param simulations Longitudinal household level dataframe
#'  storing wealth simulations
#' @param wealth_var Wealth variable in \code{simulations}
#' @param age_var Age variable in \code{simulations}
#' @param year_var Year variable
#' @param estimation_year Estimation year
#' @param start_year First year of simulation
#' @param final_year Last year of simulation
#' @param export_plot Logical value indicating whether we want
#'  to export the plot (\code{TRUE}, default) or not
#' @param export_path Path to export data
#' @param export_filename Filename if data are exported. PDF extension
#'  will be added if missing
#' @param method Should we apply smoothing directly (\code{smooth}) or first
#'  compute median income (\code{median}) before smoothing
#' @param langage Should we label axes in English or French ?
#' @param scale_viridis Logical value. Should we use viridis (\code{TRUE})
#'  or not not (\code{FALSE})
#' @param byvar Optional variable name to separate in two figures
#' @param xlab Label for x variable
#' @param ylab Label for y variable
#' @param size_text Parameter for text size in output
#' @return A \code{ggplot} object. Saved on disk if \code{export_plot=TRUE}
#' @importFrom data.table %between%
#'
#' @import scales
#' @export

plot_K_age <- function(simulations,
                      wealth_var = "wealth",
                      year_var = "annee",
                      age_var = "age",
                      graduation_var = "findet",
                      estimation_year = 2009,
                      start_year = 2009,
                      final_year = 2040,
                      trans = NULL,
                      time_0 = c("graduation", "birth"),
                      method = c('smooth','median'),
                      export_plot = FALSE,
                      export_path = getwd(),
                      export_filename = "06_profil_K_age",
                      langage = c("French","English"),
                      xlab = "Age",
                      ylab = "Simulated financial wealth (in euros)",
                      size_text = 28,
                      scale_viridis = FALSE,
                      byvar = NULL,
                      xlims = NULL){
  
  # Avoid NSE note
  . <- NULL
  
  # if (!endsWith(export_filename,".pdf"))
  #   export_filename <- paste0(export_filename,".pdf")
  
  method <- match.arg(method)
  langage <- match.arg(langage)
  time_0 <- match.arg(time_0)
  
  
  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]
  
  if (!is.null(trans)){
    
    if (trans == "log"){
      simulations2[,'wealth' := log(wealth)]
    }
    if (trans == "exp"){
      simulations2[,'wealth' := exp(wealth)]
    }
  }  
  
  
  
  if (!is.null(xlims)) simulations2 <- simulations2[get(age_var) %between% xlims]
  
  if (time_0 == "graduation") simulations2 <- simulations2[get(age_var)>get(graduation_var)]
  
  if (method != "smooth"){
    simulations2 <- simulations2[, .("wealth" = median(get(wealth_var), na.rm = TRUE)),
                                 by = c(year_var, age_var)]
  }
  
  
  simulations2[,'size' := .5 + 0.5*as.numeric(get(year_var) == estimation_year)]
  simulations2[,'annee2' := factor(get(year_var))]
  
  if (is.null(ylab)) ylab <- ifelse(
    langage == "French",
    "Patrimoine financier estim\u00e9 (splines) par \u00e2ge",
    "Financial wealth estimated (splines) by age"
  )
  
  p <- ggplot2::ggplot(simulations2) +
    ggplot2::geom_smooth(ggplot2::aes_string(x = "age", y = "wealth",
                                             color = "annee2", size = "size"),
                         se = FALSE)
  
  if (!is.null(byvar)) p <- p + ggplot2::facet_wrap(~simulations2[[byvar]])
  
  p <- p +
    ggplot2::scale_size(guide="none") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none',
                   axis.title = ggplot2::element_text(size = 14)) +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme(text = ggplot2::element_text(size=24),
                   axis.title = ggplot2::element_text(size=size_text,face="bold"))
  
  
  if (scale_viridis) p <- p + ggplot2::scale_color_viridis_d()
  
  if (export_plot)
    ggplot2::ggsave(
      paste0(export_path,"/",export_filename,".pdf"), height = 12, width = 8,
      device = 'pdf', plot = p
    )
  
  return(p)
}



#' @rdname plot_rK_age
#' @param income_var Income variable to use as income definition
#'  in \code{plot_rK_income}
#' @param scale.x,scale.y Scale for income variables. Can be \emph{level} (default)
#'  or \emph{log} (recommanded option)
#' @param ylab A label for y variable if default is not ok
#' @param lower_bound_income A lower bound for income in \code{plot_rK_income}.
#'  Ignored if \code{NULL} (default)
#' @param xlab Label for x axis
#' @param capital_income_var Variable name storing capital income
#'
#' @export

plot_rK_income <- function(simulations,
                           year_var = "annee",
                           income_var = "y_indiv",
                           capital_income_var = "rK",
                           estimation_year = 2009,
                           start_year = 2009,
                           final_year = 2040,
                           export_plot = FALSE,
                           export_path = getwd(),
                           export_filename = "04_art_rev_capital_by_age",
                           langage = c("French","English"),
                           scale.x = c("level","log"),
                           scale.y = c("level","log"),
                           ylab = NULL,
                           size_text = 32,
                           scale_viridis = FALSE,
                           lower_bound_income = NULL,
                           byvar = NULL){

  langage <- match.arg(langage)
  scale.x <- match.arg(scale.x)
  scale.y <- match.arg(scale.y)

  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")

  simulations[,'size' := .5 + 0.5*as.numeric(get(year_var) == estimation_year)]

  simulations[,'annee2' := factor(get(year_var))]

  simulations2 <- simulations[get(year_var) %between% c(start_year,final_year)]

  simulations2 <- simulations2[!is.nan(get(capital_income_var))]
  simulations2 <- simulations2[get('rKY')>0 & get('rKY')<1]

  if (is.null(ylab)) ylab <- ifelse(langage == "French",
                                    "Part des revenus du patrimoine dans le revenu total",
                                    "Capital income share in total income")
  xvar <- ifelse(scale.x == "log",
                 yes = "ly", no = income_var
  )
  yvar <- ifelse(scale.y == "log",
                 yes = "lk", no = capital_income_var
  )

  if (scale.x == "log") simulations2[,'ly' := log(get(income_var))]
  if (scale.y == "log") simulations2[,'lk' := log(get(capital_income_var))]

  if (!is.null(lower_bound_income)) simulations2 <- simulations2[get(income_var)>=lower_bound_income]

  p <- ggplot2::ggplot(simulations2) +
    ggplot2::geom_smooth(
      ggplot2::aes_string(x = xvar,
                          y = yvar,
                          color = "annee2",
                          size = "size"), se = FALSE
    )

  if (!is.null(byvar)) p <- p + ggplot2::facet_wrap(~simulations2[[byvar]])

  p <- p +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(x = "Income", y = ylab) +
    ggplot2::theme(text = ggplot2::element_text(size=24),
                   axis.title = ggplot2::element_text(size=size_text,face="bold"))

  if (scale_viridis) p <- p + ggplot2::scale_color_viridis_d()

  if (export_plot)
    ggplot2::ggsave(
      paste0(export_path,"/",export_filename,".pdf"), height = 12, width = 8,
      device = 'pdf', plot = p
    )

  return(p)
}


#' @rdname plot_rK_age
#' @param year Year to keep. Only valid for \code{plot_rK_income_1y}
#' @export

plot_rK_income_1y <- function(simulations,
                           year_var = "annee",
                           income_var = "y_indiv",
                           capital_income_var = "rK",
                           year = 2019,
                           export_plot = FALSE,
                           export_path = getwd(),
                           export_filename = "04_art_rev_capital_by_age",
                           langage = c("French","English"),
                           scale.x = c("level","log"),
                           scale.y = c("level","log"),
                           xlab = NULL,
                           ylab = NULL,
                           scale_viridis = FALSE,
                           lower_bound_income = NULL,
                           byvar = NULL){

  # Avoid NSE notes
  .<-NULL

  langage <- match.arg(langage)
  scale.x <- match.arg(scale.x)
  scale.y <- match.arg(scale.y)

  if (!endsWith(export_filename,".pdf"))
    export_filename <- paste0(export_filename,".pdf")

  simulations2 <- simulations[get(year_var) == year]
  simulations2 <- simulations2[!is.nan(get(capital_income_var))]
  simulations2 <- simulations2[get('rKY')>0 & get('rKY')<1]

  if (is.null(ylab)) ylab <- ifelse(langage == "French",
                                    "Part des revenus du patrimoine dans le revenu total",
                                    "Capital income share in total income")
  if (is.null(xlab)) xlab <- "Income"

  xvar <- ifelse(scale.x == "log",
                 yes = "ly", no = income_var
  )
  yvar <- ifelse(scale.y == "log",
                 yes = "lk", no = capital_income_var
  )

  if (scale.x == "log") simulations2[,'ly' := log(get(income_var))]
  if (scale.y == "log") simulations2[,'lk' := log(get(capital_income_var))]

  deciles <- simulations2[,.("decile" = quantile(get(income_var),
                                                 probs = 1:9/10))]
  deciles[,"thr" := paste0("D",1:9) ]
  if (!is.null(lower_bound_income)) deciles <- deciles[get("decile")>=lower_bound_income]

  if (scale.x == "log") deciles[,c("decile") := log(get("decile"))]

  if (!is.null(lower_bound_income)) simulations2 <- simulations2[get(income_var)>=lower_bound_income]

  # position_label <- quantile(simulations2$rK, 0.9)
  # if (scale.y == "log") position_label <- log(position_label)

  # Avoid aving log(x)<0
  simulations2 <- simulations2[get(yvar)>0]
  simulations2 <- simulations2[get(xvar)>0]

  p <- ggplot2::ggplot(simulations2) +
    ggplot2::geom_smooth(
      ggplot2::aes_string(x = xvar,
                          y = yvar), se = TRUE
    ) +
    ggplot2::geom_point(data = simulations2[sample(seq_len(nrow(simulations2)),
                                                   size = nrow(simulations2)/10,
                                                   replace = FALSE)],
                        ggplot2::aes_string(x = xvar,
                                            y = yvar), alpha = .2)

  if (!is.null(byvar)) p <- p + ggplot2::facet_wrap(~simulations2[[byvar]])

  p <- p + ggplot2::theme_bw() +
    ggplot2::theme(legend.position = 'none') +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme(text = ggplot2::element_text(size=34))

  if (scale_viridis) p <- p + ggplot2::scale_color_viridis_d()

  if (export_plot)
    ggplot2::ggsave(
      paste0(export_path,"/",export_filename,".pdf"), height = 12, width = 8,
      device = 'pdf', plot = p
    )

  return(p)
}
