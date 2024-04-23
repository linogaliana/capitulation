#' Match referent with children to finalize household composition
#' 
#' A function to match household husband and spouse information
#'  with the children they have 
#' @param table_indiv Individual level table, output from
#'  \link{assign_referent}
#' @param descript Individual auxiliary information used to determine
#'  if a child is still in household
#' @return \code{table_household} with two additional variables:
#' \itemize{
#'  \item{\code{list_children}: comma separated list of children ids}
#'  \item{\code{UC}: number consumption units at household level}
#' }
#' 
#' @importFrom stringi %s+%
#' @importFrom data.table melt
#' @export
#' 

assign_children <- function(table_indiv, descript){
  
  if (inherits(table_indiv, "data.table")){
    return(
      assign_children_dt(table_indiv, descript)
    )
  }else{
    return(
      assign_children_dt(data.table::data.table(table_indiv),
                         data.table::data.table(descript)
      )
    )
  }
}

#' @importFrom stats na.omit

assign_children_dt <- function(table_indiv, descript){
  
  
  # Avoid NSE notes
  .<-NULL
  
  table_indiv2 <- data.table::copy(table_indiv)
  table_indiv2 <- table_indiv2[,.SD,
                               .SDcols = c('Id','annee',
                                           'pere','mere','age')]
  
  
  # II - KEEP ONLY PERIOD WHEN INDIVIDUAL CAN BE INSIDE HOUSEHOLD
  # ------------------------------------------------------------
  
  description_child <- data.table::copy(descript)
  description_child[,'anmort':= get('anaiss') + get('ageMax')-1L]
  description_child <- description_child[,.SD,.SDcols = c('Id','findet','anmort','anaiss')]
  data.table::setkeyv(table_indiv2, 'Id')
  data.table::setkeyv(description_child, 'Id')
  
  table_indiv2 <- merge(table_indiv2,description_child, all.x = TRUE)
  
  # KEEP CHILDREN LIST BY ADULT 
  list_children <- data.table::melt(
    table_indiv2[,.SD,.SDcols = c('Id','pere','mere')],
    id.vars = 'Id')
  list_children[,'variable' := NULL]
  data.table::setnames(list_children,
                       old = c('Id','value'),
                       new = c('id_child','id_referent'))
  list_children <- unique(list_children)
  
  # KEEP CHILDREN STILL IN HOUSEHOLD    
  table_indiv2 <- table_indiv2[get('age') < get('findet')]
  
  
  # III - GET LONG FORMATTED DATA
  # ------------------------------------------------------------
  
  # a) Transform id_children into id:age:UC
  table_indiv2[, 'UC' := 0.5]
  table_indiv2[get('age')<14, 'UC' := 0.3]
  
  table_indiv2 <- table_indiv2[,.SD,
                               .SDcols = c('annee','pere',
                                           'mere','Id','UC','anmort')]
  
  table_indiv2 <- data.table::melt(table_indiv2,
                                   id.vars = c("annee","Id",'UC','anmort'),
                                   value.name = "referent")
  table_indiv2[,'variable' := NULL]
  
  
  # IV - COLLECT
  # --------------------------------
  
  # IVa - COLLECT CHILDREN IN HOUSEHOLD
  # ----------------------------------------
  
  table_indiv3 <- table_indiv2[, .('list_children' =
                                     paste(
                                       unique(get('Id')),collapse=", "
                                     ),
                                   'UC' = sum(get('UC')),
                                   'nbpersm' = sum(1L)),
                               by = c("referent", "annee")]
  
  
  table_indiv3[get('referent') == 0, `:=` (
    'list_children' = NA_character_,
    'UC' = NA_real_)]
  
  
  # IVb - COLLECT CHILDREN ALONG LIFE
  # ----------------------------------------
  
  list_children2 <- list_children[get('id_referent') != 0]
  
  list_children2 <- list_children2[,.('list_children' = paste(
    unique(get('id_child')),collapse=", ")
  ),
  by = c("id_referent")]
  
  # RENAME FOR CONSISTENCY
  data.table::setnames(list_children2,
                       old = c('id_referent','list_children'),
                       new = c('referent','list_children_life'))
  
  data.table::setnames(table_indiv3, old = 'list_children',
                       new = 'list_children_charge')
  
  # MERGE TOGETHER INFORMATION REGARDING CHILDREN
  table_indiv3 <- merge(table_indiv3,list_children2,
                        all.x = TRUE)
  
  
  # IVc - COLLECT CHILDREN STILL ALIVE
  # ----------------------------------------
  
  # LONG FORMAT DATA WILL BE EASIER TO HANDLE
  long_table_children <- table_indiv3[,.('list_child' = unlist(
    strsplit(get('list_children_life'), ","))
  ), by = c('referent','annee')]
  long_table_children <- na.omit(long_table_children)
  long_table_children[, c('list_child') := trimws(get('list_child'))]
  
  # A TABLE REFERENT | CHILD NUMBER | YEAR
  description_child[,'Id' := as.character(get('Id'))]
  table_children_alive <- merge(long_table_children, description_child, by.y = "Id", by.x = "list_child",
                                all.x = TRUE)
  
  # KEEP ONLY OBSERVATIONS WHERE CHILD IS ALIVE
  table_children_alive <- table_children_alive[get('annee')<= get('anmort') & get('annee') >= get('anaiss')]
  table_children_alive <- table_children_alive[,.('list_children_alive' = paste(
    unique(get('list_child')),collapse=", ")
  ),
  by = c("referent",'annee')]
  
  # V - MERGE WITH FAMILY
  # --------------------------------
  
  data.table::setkeyv(table_indiv3,c("referent","annee"))
  data.table::setkeyv(table_indiv,c("Id","annee"))
  
  data_household <- merge(table_indiv,table_indiv3,
                          all.x = TRUE,
                          by.y = c('referent',"annee"),
                          by.x = c('Id',"annee"))
  
  data_household <- merge(data_household,table_children_alive,
                          all.x = TRUE,
                          by.y = c('referent',"annee"),
                          by.x = c('Id',"annee"))
  
  # V - COMPUTE UC
  # --------------------------------
  
  # REPLACE NAs WITH ZEROS  
  data_household[is.na(get('UC')), 'UC' := 0]
  data_household[is.na(get('nbpersm')), 'nbpersm' := 0L]
  data_household[, 'nspouses' := 1L]
  
  # FIRST UC: ADULT
  data_household[, `:=` ('UC' = get('UC') + 1,
                         'nbpersm' = get('nbpersm') + 1L)]
  
  # ADD 1/2 UC FOR SPOUSE
  data_household[get('matri') == 2, `:=` ('UC' = get('UC') + 1/2,
                                          'nbpersm' = get('nbpersm') + 1L,
                                          'nspouses' = get('nspouses') + 1L)]
  
  
  return(data_household)
  
}

