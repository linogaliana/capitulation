#' Count number of lines in package
#' 
#' Just for fun
#' @param path Directory

count_line_code <- function(path = getwd()){

  message("Compte toutes les lignes de code")
  
  list.files(path = path, recursive = TRUE, full.names = TRUE) %>%
    stringr::str_subset("[.](R|cpp)$") %>%
    vapply(function(x) x %>% readLines() %>% length()) %>%
    sum()
  
}

