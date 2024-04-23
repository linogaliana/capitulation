#' Create dataframe storing heir share to family
#'  wealth when a death event occurs
#'  
#' @param df_longitudinal Longitudinal data taking the form
#'  of the output of \link{household_composition}
#' 
#' @return A dataframe with following variables
#' \describe{
#'   \item{Id}{Identifier for the individual that dies}
#'   \item{annee}{Year at which event occurs}
#'   \item{share}{Share that heir will have}
#'   \item{heir}{Identifier for heir}
#' }
#' 
#' @importFrom stringi stri_count
#' @export

death_event <- function(df_longitudinal){
  
  # Avoid NSE notes
  . <- NULL
  
  data_death <- df_longitudinal[,.SD[.N],by = 'Id']
  data_death <- data_death[get('annee')<max(get('annee'), na.rm = TRUE)] #2070: end of Destinie's world
  
  # WE KEEP ONLY PEOPLE WITH HUSBAND/WIFE OR CHILDREN
  data_death <- data_death[!is.na(get('list_children_alive')) | ((get('matri')==2) & (get('conjoint') != 0))]
  
  data_death[,'spouse_share' := as.numeric(get('matri')==2L)*1L/4L]
  data_death[is.na(get('list_children_alive')), 'spouse_share' := 1L]
  data_death[,'children_share' := 1 - get('spouse_share')]
  
  data_death[!is.na(get('list_children_alive')),
             c('children_share') := get('children_share')/(stringi::stri_count(get("list_children_alive"),fixed = ",")+1)]
  
  
  # FOR CHILDREN, TRANSFORM INTO LONGITUDINAL DATA
  
  data_death_long <- data_death[,.SD,
                                .SDcols = c('Id', 'annee',
                                            'list_children_alive',
                                            'children_share')]
  
  data_death_long <- data_death_long[!is.na(get('list_children_alive')),
                                     .('heir' = unlist(
                                       data.table::tstrsplit(get('list_children_alive'),
                                                             ",",fixed=TRUE))
                                     ), by = c("Id","annee","children_share")]
  data.table::setnames(data_death_long,
                       old = "children_share",
                       new = "share")
  
  
  data_death_spouse <- data_death[,.SD,
                                  .SDcols = c('Id','annee','conjoint','spouse_share')]
  
  
  data.table::setnames(data_death_spouse,
                       old = c('conjoint','spouse_share'),
                       new = c('heir','share'))
  
  data_event <- data.table::rbindlist(
    list(data_death_long, data_death_spouse),
    use.names = TRUE
  )
  
  return(data_event)
}

