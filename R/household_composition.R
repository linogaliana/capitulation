
#' Associate individuals with the family they belong to
#' 
#' This function is designed to associate individual with
#' \enumerate{
#'   \item Household head or heads (couples)
#'   \item Children
#' }
#'  
#' @inheritParams assign_referent
#' @inheritParams assign_children
#' @param simul Destinie tables stored as a list
#' @export

household_composition <- function(table_indiv, simul, descript,
                                  call.Rcpp = TRUE){
  
  indiv <- assign_referent(table_indiv = table_indiv, fam = simul$fam, descript = descript,
                           call.Rcpp = TRUE)
  
  indiv <- assign_children(table_indiv = indiv, descript = descript)  
  
}
