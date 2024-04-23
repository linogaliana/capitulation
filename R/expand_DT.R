#' Expand data.table to prepare unnesting
#' 
#' @param DT \link[data.table]{data.table} object
#' @param listCol Nested column
#' @param idCol Id column that should be exploded
#' 
#' @return \link[data.table]{data.table} object with
#'  just one column exploded

expand_data.table <- function(DT, listCol = 'x',
                              idCol = 'Id'){
  
  # Just a sanity check to ensure no error
  colClasses <- lapply(DT,FUN=class)
  listCols <- colnames(DT)[colClasses=='list']
  if(length(listCols) == 0) return(DT)
  
  # Count number of duplications by row
  numb_row <- vapply(DT[[listCol]], FUN = nrow)
  DT[,get('n') := get('numb_row')]
  
  DT1 <- lapply(seq_len(nrow(DT)), function(k)({
    data.table::data.table(rep(DT[[idCol]][k],DT[['n']][k]))
  })) %>%
    data.table::rbindlist()
  
  data.table::setnames(DT1, old = names(DT1), new = idCol)
  
  return(DT1)
} 