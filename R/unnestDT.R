
#' A small method to unnest objects
#'
#' @param data A `data.table` object
#' @param ... Additional arguments
#' @return A character vector with names
#'  that can be passed to latex
#' @export
unnest <- function(data, ...){
  UseMethod("unnest")
}


#' @rdname unnest
#' @export
unnest.data.table <- function(data, ...){
  
  args <- list(...)
  col  <- args[['col']]
  id   <- args[['id']]
  
  stopifnot(data.table::is.data.table(data))
  
  by <- substitute(id)
  col <- substitute(unlist(col, recursive = FALSE))
  
  dt <- data[, eval(col), by = id]
  
  return(dt)
}
