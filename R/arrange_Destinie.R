#' Arrange Destinie simulation for longitudinal data
#' 
#' Create intermediary tables from Destinie's simulation.
#' 
#' @details The following tables are created:
#' \itemize{
#'  \item{\code{pre_indiv}: Individual panel information}
#'  \item{\code{description}: Individual characteristics}
#'  \item{\code{macro}: Macro environment variables}
#'  \item{\code{familles}: Family information}
#'  \item{\code{pensions}: Individual pensions}
#'  }
#' Format (either \emph{dplyr} or \emph{data.table}) depends on the argument
#'  provided to \link{import_Destinie}
#' 
#' @param simul List of Destinie simulations created by \link{import_Destinie}
#' @return List whose elements are described in \emph{details} section
#' 
#' @examples \dontrun{
#' path_data <- "./data"
#' simu <- capitulation::import_Destinie(path_data = path_data)
#' tables <- capitulation::arrange_Destinie(simu)
#' 
#' # If you prefer data.table approach
#' simu <- capitulation::import_Destinie(path_data = path_data, format = 'data.table')
#' tables <- capitulation::arrange_Destinie(simu)
#' }
#' 
#' @export

arrange_Destinie <- function(simul){
  
  format <- class(simul[[1]])
  
  if ("data.table" %in% format){
    arrange_Destinie_dt(simul)
  } else{
    arrange_Destinie_dplyr(simul)
  }
  
}


arrange_Destinie_dt <- function(simul){
  
  # 1/ Création d'une base pre_indiv qui contient plusieurs
  #       donnees utiles par individus et par ans
  # -------------------------------------------------------
  
  data.table::setkeyv(simul$salairenet,
                      intersect(
                        names(simul$salairenet),
                        names(simul$emp)))
  data.table::setkeyv(simul$emp,
                      intersect(names(simul$salairenet),
                                names(simul$emp)))
  pre_indiv <- merge(simul$emp, simul$salairenet, all.x = TRUE)
  pre_indiv[,'annee':=get('age')]
  pre_indiv <- pre_indiv[,.SD,
                         .SDcols = c("Id","age","annee",
                                     "salaire","salaires_net")]
  
  
  # 2/ On prépare une base "description" avec des
  #     infos sur chaque individus
  # -------------------------------------------------------
  
  description <- simul$ech[,.SD,.SDcols = c("Id","sexe","neFrance","anaiss",
                                            "findet","ageMax","pere","mere")]
  temp_liquid <- simul$liquidations[,.SD,.SDcols = c("Id","ageliq")]
  data.table::setkeyv(description,"Id")
  data.table::setkeyv(temp_liquid,"Id")
  description <- merge(description, temp_liquid, all.x = TRUE)
  
  non_liq <- description[is.na(get("ageliq")),]
  non_liq[,'ageliq' := get('ageMax') - 1]
  
  description <- data.table::rbindlist(list(non_liq,
                                            description[!is.na(get("ageliq"))]))
  description <- description[order(get("Id"))]
  
  
  # 3/ Quelques autres bases utiles
  # ------------------------------------------------
  #   * "macro" contient les indices des prix pour chaque année
  #   * "familles" les enfants, parents, conjoints et statuts matrimoniaux
  
  macro <- simul$macro[,.SD, .SDcols = c('annee', 'Prix')]
  familles <- simul$fam
  
  
  # 4/ Pensions contient pour chaque individu et chaque année la
  # somme des pensions et réversions qu'il touche cette année
  # ------------------------------------------------
  
  pensions <- simul$retraites
  pensions[,'pension' := get("pension") + get("rev")]
  pensions <- pensions[,.SD,.SDcols = c("Id","annee","pension")]
  
  return(list(
    "pre_indiv" = pre_indiv,
    "description" = description,
    "macro" = macro,
    "familles" = familles,
    "pensions" = pensions)
  ) 
  
  
}


arrange_Destinie_dplyr <- function(simul){
  
  # 1/ Création d'une base pre_indiv qui contient
  #     plusieurs donnees utiles par individus et par ans
  # -------------------------------------------------------
  
  pre_indiv  <- simul$emp %>% dplyr::left_join(simul$salairenet)
  pre_indiv  <-  pre_indiv %>%
    dplyr::select(.data$Id, .data$age,
                  .data$salaire, .data$salaires_net) %>%
    dplyr::mutate(annee = .data$age) %>%
    dplyr::select(.data$Id, .data$age, .data$annee,
                  .data$salaire, .data$salaires_net)
  
  
  # 2/ On prépare une base "description" avec
  #     des infos sur chaque individus
  # -------------------------------------------------------
  
  description <- simul$ech %>%
    dplyr::select(.data$Id, .data$sexe, .data$neFrance,
                  .data$anaiss,
                  .data$findet, .data$ageMax,
                  .data$pere, .data$mere) %>%
    dplyr::left_join(
      simul$liquidations %>% dplyr::select(.data$Id, .data$ageliq),
      by = "Id")
  non_liq <- description %>%
    dplyr::filter(is.na(.data$ageliq)) %>%
    dplyr::mutate(ageliq = .data$ageMax - 1)
  description <- description %>%
    dplyr::filter(!is.na(.data$ageliq)) %>%
    dplyr::bind_rows(non_liq) %>%
    dplyr::arrange(.data$Id)
  
  
  # 3/ Quelques autres bases utiles
  # ------------------------------------------------
  #   * "macro" contient les indices des prix pour chaque année
  #   * "familles" les enfants, parents, conjoints et statuts matrimoniaux
  
  macro <- simul$macro %>% dplyr::select(.data$annee, .data$Prix)
  familles <- simul$fam
  
  
  # 4/ Pensions contient pour chaque individu et chaque année la
  # somme des pensions et réversions qu'il touche cette année
  # ------------------------------------------------
  pensions <- simul$retraites %>%
    dplyr::select(.data$Id, .data$annee, .data$pension, .data$rev) %>%
    dplyr::mutate(pension = .data$pension + .data$rev) %>%
    dplyr::select(.data$Id, .data$annee, .data$pension)
  
  
  return(list(
    "pre_indiv" = pre_indiv,
    "description" = description,
    "macro" = macro,
    "familles" = familles,
    "pensions" = pensions)
  ) 
}
