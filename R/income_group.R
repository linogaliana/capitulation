

#' Income group an individual belongs to
#' 
#' Determine to which income group an individual belongs to
#' 
#' This
#' 
#' @param data A \link[data.table]{data.table} object
#' @param quantile Quantile used (between 0 and 1) to cut
#'  population in two classes
#' @param groupvar Variable prefix for the group identification.
#'  Variable name will be \code{group_var}_(1-\code{quantile})
#' @param idvar Id variable name
#' @param incomevar Income variable
#' @param yearvar Year variable. Used for \code{by} statement
#' 
#' @export




income_group <- function(data,
                         quantile = 0.9,
                         idvar = "Id",
                         groupvar = "top",
                         incomevar = "y_indiv",
                         yearvar = "annee"){
  
  # Avoid NSE notes
  . <- NULL
  
  data_income <- data[,.('permanent_income' = mean(get(incomevar))),
                      by = idvar]

  
  data_income[,'quant_year' := quantile(get("permanent_income"), na.rm = TRUE,
                                        probs = quantile)]
  
  
  data_income[, c(paste0(groupvar,"_",100*(1-quantile))) :=
                as.numeric(get("permanent_income")>=get('quant_year'))]
  
  data_income[,'quant_year' := NULL]
  
  data <- merge(data,data_income, by = idvar)

  return(data)
}


income_group2 <- function(data,
                         quantile = 0.9,
                         idvar = "Id",
                         groupvar = "top",
                         incomevar = "y_indiv",
                         yearvar = "annee"){
  
  # Avoid NSE notes
  . <- NULL
  
  data_income <- data[get(incomevar) > 0]
  
  
  data_income[,'income_group' := cut(get(incomevar),
                                    breaks = sort(quantile(get(incomevar),
                                                      c(0, 0.5, 6:10/10)
                                    ) # + rnorm(length(c(0, 0.5, 6:10/10)))
                                    ),
                                    labels = c("bottom50", paste0("D",6:10)), right = FALSE),
             by = yearvar]
  
  data_income <- merge(data, data_income,
                       by = intersect(colnames(data), colnames(data_income)),
                       all.x = TRUE)
  
  data_income[is.na(get("income_group")), c("income_group") := "bottom50"]
  
  
  return(data_income)
}

