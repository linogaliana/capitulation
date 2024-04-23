#' Determine last family change
#' 
#' Determine when was last family change (marriage, divorce, etc.)
#'  an individual has known. The breaks of family change give us information
#'  on the years where household wealth should be splitted/merged between individuals
#'  
#' @details This step is necessary to ease microsimulation computations
#'  since family change provoke splitting and merging individual wealth
#'  
#' @param household_table Longitudinal household table
#' @return Initial table with \code{last_year} variable giving
#'  year of last change
#' @export


last_change_indiv <- function(household_table){
  
  cols <- names(household_table)
  
  # COMPUTE LAST YEAR SINCE CHANGE
  df <- household_table %>% dplyr::group_by(.data$Id, .data$matri, .data$conjoint) %>%
    dplyr::mutate(diff = .data$annee - dplyr::lag(.data$annee)) %>%
    dplyr::mutate(x = dplyr::case_when(
      .data$diff != 1 | is.na(.data$diff)
      ~ as.numeric(1),
      TRUE ~ as.numeric(0))) %>%
    #CREATE NESTED STRUCTURES OF SITUATIONS
    dplyr::mutate(x = cumsum(.data$x)) %>%
    dplyr::group_by(.data$Id, .data$matri, .data$conjoint, .data$x) %>%
    # YEAR BEFORE THIS SITUATION OCURRED
    dplyr::mutate(dernier_changement = min(.data$annee)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$x,-.data$diff)
  
  # IMPUTE annee_cle: YEAR BEFORE A SITUATION CHANGE      
  df  <- df %>% dplyr::arrange(.data$Id,.data$annee) %>%
    dplyr::mutate(annee_cle =
                    dplyr::case_when(
                      dernier_changement != dplyr::lead(.data$dernier_changement)
                      ~ as.numeric(1),
                      TRUE ~ as.numeric(0)
                    ))
  
  # Individual disappar (e.g. die): change thus last appearance year in sample should be annee_cle=1
  df <- df %>% dplyr::group_by(.data$Id) %>%
    dplyr::mutate(
      annee_cle = dplyr::case_when(
        .data$annee_cle == 0 & .data$annee == max(.data$annee)
        ~ as.numeric(1),
        TRUE ~ .data$annee_cle
      )
    ) %>% dplyr::ungroup()
  
  
  return (df %>% dplyr::select(!!cols))
}