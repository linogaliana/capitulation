#' Assign household head to individual data
#' 
#' Starting from individual panel data and family information,
#'  match individual to a household head
#'  
#' @param table_indiv Dataframe storing individual ids. Output of
#'  \link{prepare}
#' @param fam Family information from \link{arrange_Destinie}
#' @param descript Personal characteristics table from \link{arrange_Destinie}
#' @param call.Rcpp Logical value indicating whether we internally want to use 
#'  \code{C++} (\code{TRUE}) or \code{R} (\code{FALSE}) language
#'  
#' @seealso \link{assign_referent_cpp} for the internally called
#' C++ function (recommended one) and \link{assign_referent_R}
#' for the R version
#'  
#' @details The following rules are applied:
#' \itemize{
#' \item{\emph{Immigrants}: Arrive without referent}
#' \item{\emph{Couples}: Male is assumed to be the head}
#' \item{\emph{Husband/Spouse death}: Individual is his/her own head}
#' \item{\emph{Divorce}: Individual is his/her own head}
#' \item{\emph{Potentially under father/mother authority}:
#'  If education is accomplished or young adult already earns income,
#'  individual is his/own head.
#' Father is assumed to be the household head. If father is dead,
#'  mother is the head. If she is dead, individual is his/her own head}
#' }
#'  
#' @return Individual panel data after being mapped with household head
#' @examples \dontrun{
#' system.time({
#' capitulation::assign_referent(table_indiv = pre_indiv, fam = simu$fam, descript = description,
#'                               call.Rcpp = TRUE)
#' })
#' # utilisateur     système      écoulé 
#' #11.300       0.400      11.693 
#' 
#' system.time({
#'   capitulation::assign_referent(table_indiv = pre_indiv, fam = simu$fam, descript = description,
#'                                 call.Rcpp = FALSE)
#' })
#' # utilisateur     système      écoulé 
#' # 12.844       2.872      22.348 
#' 
#' }
#' @export



assign_referent <- function(table_indiv, fam, descript,
                            call.Rcpp = TRUE){
  
  isDT <- 'data.table' %in% class(table_indiv)
  
  if (!isDT) message(
    paste0("tidyverse approach is deprecated: result ",
           "will not be consistent with data.table ",
           "implementation")
  )
  
  if (isDT) return(assign_referent_dt(table_indiv = table_indiv, fam = fam,
                                      descript = descript,
                                      call.Rcpp = call.Rcpp))
  
  # ============================================
  # I - CREATE DATAFRAME FOR HOUSEHOLD HEAD
  # ============================================
  
  # ADD INFORMATION TO INDIVIDUAL DATA FROM ADDITIONAL 'FAM' TABLE
  
  z <- table_indiv %>%
    dplyr::left_join(
      dplyr::select(fam, .data$Id, .data$annee, .data$conjoint, .data$matri),
      by = c("Id", "annee")
    ) %>%
    dplyr::mutate(referent = NA) %>% dplyr::tbl_df()
  
  # ============================================
  # II - AGE OF RELATIVES
  # ============================================
  
  age_parents <- function(x, descript, isDT = TRUE){
    
    # Join with descript data
    if (isDT){
      data.table::setkeyv(x, intersect(names(x),names(descript)))
      data.table::setkeyv(descript, intersect(names(x),names(descript)))
      x <- merge(x,descript,all.x = TRUE)
    } else{
      x <- x %>% dplyr::tbl_df() %>%
        dplyr::left_join(descript %>% dplyr::tbl_df())
    }
    
    # Get father and mother death age
    if (!isDT){
      ageMaxMere <- descript %>% dplyr::tbl_df() %>%
        dplyr::select(.data$Id,.data$ageMax) %>%
        dplyr::rename(ageMaxMere = .data$ageMax, mere = .data$Id)
      ageMaxPere <- descript %>% dplyr::tbl_df() %>%
        dplyr::select(.data$Id,.data$ageMax) %>%
        dplyr::rename(ageMaxPere = .data$ageMax, pere = .data$Id)
      x <- x %>% dplyr::left_join(ageMaxMere) %>%
        dplyr::left_join(ageMaxPere)
    } else{
      ageMaxMere <- descript[,.SD,.SDcols = c('Id','ageMax')]
      data.table::setnames(ageMaxMere,
                           old = c('ageMax','Id'),
                           new = c('ageMaxMere','mere'))
      ageMaxPere <- descript[,.SD,.SDcols = c('Id','ageMax')]
      data.table::setnames(ageMaxPere,
                           old = c('ageMax','Id'),
                           new = c('ageMaxPere','pere'))
      x <- merge(
        merge(x,ageMaxMere, all.x = TRUE,
              by = intersect(names(x),names(ageMaxMere))),
        ageMaxPere, all.x = TRUE,
        by = intersect(names(x),names(ageMaxPere)))
    }
    
    #x %>% filter(is.na(ageMaxMere) | is.na(ageMaxPere))
    return(x)    
  }
  
  z <- age_parents(z,descript,isDT)
  
  
  # =============================================================
  # MATCH PRE-2009 TRAJECTORY WITH 2009 HOUSEHOLD COMPOSITION
  # =============================================================
  
  z[, 'pre2009' :=  get('annee')<=2009]
  z[, `:=` ('conjoint2009' = mean(get('conjoint'), na.rm = TRUE),
            'matri2009' = mean(get('matri'), na.rm = TRUE)),
    by = c('pre2009','Id')]
  
  z[, `:=` (
    'conjoint' = lest::case_when(
      (get('pre2009')) ~ as.numeric(get('conjoint2009')),
      TRUE ~ as.numeric(get('conjoint'))
    ),
    'matri' = lest::case_when(
      (get('pre2009')) ~ as.numeric(get('matri2009')),
      TRUE ~ as.numeric(get('matri'))
    )
  )]
  z[,c('pre2009', 'conjoint2009', 'matri2009') := NULL]
  
  
  # ASSIGN HOUSEHOLD HEAD
  # -----------------------------
  
  
  if (call.Rcpp){
    
    df_referent <- assign_referent_cpp(id = z$Id,
                                       annee = z$annee,
                                       conjoint = z$conjoint,
                                       sexe = z$sexe,
                                       matri = z$matri,
                                       referent = z$referent,
                                       findet = z$findet,
                                       age = z$age,
                                       salaire = z$salaire,
                                       pere = z$pere,
                                       ageMaxPere = z$ageMaxPere,
                                       mere = z$mere,
                                       ageMaxMere = z$ageMaxMere,
                                       neFrance = z$neFrance)
    if (isDT){
      df_referent <- do.call(cbind,df_referent) %>%
        data.table::as.data.table()
      z[,c('referent','matri','conjoint') := NULL]
      data.table::setkeyv(df_referent,
                          intersect(names(df_referent),names(z)))
      data.table::setkeyv(z, intersect(names(df_referent),names(z)))
      z <- merge(z,df_referent,all.x = TRUE)
    } else{
      df_referent <- do.call(cbind,df_referent) %>% dplyr::tbl_df()
      z <- z %>% dplyr::select(-.data$referent,-.data$matri,-.data$conjoint)
      z <- z %>% dplyr::left_join(df_referent)
    }
    
    return(z)
    
  }  
  
  # STANDARD R OTHERWISE
  ans <- assign_referent_R(z,descript)
  
  
  return(ans)
}


assign_referent_dt <- function(table_indiv, fam, descript,
                               call.Rcpp = TRUE){
  
  
  # ============================================
  # I - CREATE DATAFRAME FOR HOUSEHOLD HEAD
  # ============================================
  
  # REMOVE MULTIPLE MATCH FROM 2009 YEAR (PSEUDO-COUPLES)
  # -----------------------------------------------------------
  
  # Pseudo-couples: multiple 'conjoint' in 2009
  fam[,'N' := .N, by = c('conjoint','annee')]
  
  # Keep only one couple for duplicated pseudo-husband/wife
  fam <- fam[!((get('N')>1) & (get('matri')==3) & (get('conjoint') != 0))]
  
  
  # ADD INFORMATION TO INDIVIDUAL DATA FROM ADDITIONAL 'FAM' TABLE
  # ------------------------------------------------------------------
  
  data.table::setkeyv(table_indiv, c('Id','annee'))
  data.table::setkeyv(fam, c('Id','annee'))
  fam <- fam[,.SD,.SDcols = c('Id','annee','conjoint','matri')]
  z <- merge(table_indiv,fam,all.x = TRUE)
  z[,'referent' := NA]
  z[,'referent2' := NA]
  
  # ============================================
  # II - AGE OF RELATIVES
  # ============================================
  
  age_parents <- function(x, descript){
    
    data.table::setkeyv(x, intersect(names(x),names(descript)))
    data.table::setkeyv(descript, intersect(names(x),names(descript)))
    x <- merge(x,descript,all.x = TRUE)
    
    # Get father and mother death age
    ageMaxMere <- descript[,.SD,.SDcols = c('Id','ageMax')]
    data.table::setnames(ageMaxMere,
                         old = c('ageMax','Id'),
                         new = c('ageMaxMere','mere'))
    ageMaxPere <- descript[,.SD,.SDcols = c('Id','ageMax')]
    data.table::setnames(ageMaxPere,
                         old = c('ageMax','Id'),
                         new = c('ageMaxPere','pere'))
    x <- merge(
      merge(x,ageMaxMere, all.x = TRUE,
            by = intersect(names(x),names(ageMaxMere))),
      ageMaxPere, all.x = TRUE,
      by = intersect(names(x),names(ageMaxPere)))
    
    #x %>% filter(is.na(ageMaxMere) | is.na(ageMaxPere))
    return(x)    
  }
  
  z <- age_parents(z,descript)
  
  
  # =============================================================
  # MATCH PRE-2009 TRAJECTORY WITH 2009 HOUSEHOLD COMPOSITION
  # =============================================================
  
  z[, 'pre2009' :=  get('annee')<=2009]
  z[, `:=` ('conjoint2009' = mean(get('conjoint'), na.rm = TRUE),
            'matri2009' = mean(get('matri'), na.rm = TRUE)),
    by = c('pre2009','Id')]
  
  z[, `:=` (
    'conjoint' = lest::case_when(
      (get('pre2009')) ~ as.numeric(get('conjoint2009')),
      TRUE ~ as.numeric(get('conjoint'))
    ),
    'matri' = lest::case_when(
      (get('pre2009')) ~ as.numeric(get('matri2009')),
      TRUE ~ as.numeric(get('matri'))
    )
  )]
  z[,c('pre2009', 'conjoint2009', 'matri2009') := NULL]
  
  
  # ASSIGN HOUSEHOLD HEAD
  # -----------------------------
  
  
  if (call.Rcpp){
    
    df_referent <- assign_referent_cpp(id = z$Id,
                                       annee = z$annee,
                                       conjoint = z$conjoint,
                                       sexe = z$sexe,
                                       matri = z$matri,
                                       referent = z$referent,
                                       referent2 = z$referent2,
                                       findet = z$findet,
                                       age = z$age,
                                       salaire = z$salaire,
                                       pere = z$pere,
                                       ageMaxPere = z$ageMaxPere,
                                       mere = z$mere,
                                       ageMaxMere = z$ageMaxMere,
                                       neFrance = z$neFrance)
    
    df_referent <- do.call(cbind,df_referent) %>% data.table::as.data.table()
    z[,c('referent','matri','conjoint') := NULL]
    data.table::setkeyv(df_referent,
                        intersect(names(df_referent),names(z)))
    data.table::setkeyv(z, intersect(names(df_referent),names(z)))
    z <- merge(z,df_referent,all.x = TRUE)
    
    return(z)
    
  }  
  
  # STANDARD R OTHERWISE
  ans <- assign_referent_R(z,descript)
  
  
  return(ans)
}