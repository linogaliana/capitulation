#' capitulation: microsimulation of capit(al accumu)lation
#' 
#' An R package to microsimulate household capital accumulation
#'  decision with a particular emphasis on life-cycle dynamic
#' 
#' @section C++ functions:
#' The main C++ functions are the following:
#' \itemize{
#'  \item{\code{assign_referent_cpp}: match individual list with household heads. This is a C++ version
#'   of \link{assign_referent_R}} 
#'  \item{\code{inherit_wealth}}: wealth that should be given to household at $K_{t_0}$
#' }
#' @docType package
#' @name capitulation
#' @useDynLib capitulation, .registration=TRUE
#' @importFrom rlang .data
#' @importFrom Rcpp evalCpp
#' @importFrom magrittr %>%
#' @importFrom foreach %dopar%
#' @importFrom rlang !!
#' @importFrom utils setTxtProgressBar
#' @importFrom utils txtProgressBar
#' @import ggplot2
NULL


