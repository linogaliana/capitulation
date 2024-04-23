#' Determine household head for individual level data (R version)
#' 
#' \code{R} function to determine household referent from
#'  individual data. This is a necessary step to transform individual
#'  level data into household data. To speed up computations,
#'  parallelization is performed.
#'  
#' @seealso \link{assign_referent} ; \link{assign_referent_cpp} for a
#' C++ version (recommended one)
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
#' @inheritParams assign_referent
#' @param z Individual data that should be mapped with household head
#'  
#' @return Dataframe storing, between others, individual
#'  id with household head id
#' @export



assign_referent_R <- function(z, descript){
  
  # Avoid NSE notes
  g <- NULL
  
  # Intermediate function to assign household referents
  # to a group of individual
  referent_actif_groupe <- function(x, descript){
    
    # Stop execution if empty table    
    if (nrow(x)==0) return(NULL)
    
    # Special cases: immigrants
    #   Absent from fam table before being in France,
    #        present into simul$emp --> NAs for `conjoint`
    #   They are assumed to be without spouse/husband
    #        before arriving into France
    
    x <- x  %>%
      dplyr::mutate(matri = dplyr::case_when(
        is.na(conjoint) ~ as.numeric(1L),
        !is.na(conjoint) ~ as.numeric(matri)
      )) %>%
      dplyr::mutate(conjoint = dplyr::case_when(
        is.na(conjoint) ~ as.numeric(0L),
        !is.na(conjoint) ~ as.numeric(conjoint)
      )) %>%
      dplyr::mutate(matri = dplyr::case_when(
        !is.na(matri) ~ matri,
        TRUE ~ as.numeric(1L)
      ))
    
    # USE RULES TO ASSIGN HOUSEHOLD HEAD
    
    x2 <- x %>%
      dplyr::mutate(referent = dplyr::case_when(
        # Individu a un conjoint encore en vie & homme
        #    --> household head
        matri == 2 & sexe == 1
        ~ as.numeric(Id),
        # Individu a un conjoint encore en vie & femme
        #    --> husband is household head
        matri == 2 & sexe != 1
        ~ as.numeric(conjoint),
        # En cas de divorce chez les parents
        #    --> household head 
        matri == 3 | matri == 4
        ~ as.numeric(Id),
        # Si l'individu est célibataire, et qu'il
        # a fini ses études ou gagne
        # déjà un salaire (quitte foyer parental)
        # --> household head
        findet < age | salaire != 0
        ~ as.numeric(Id),
        # Reste: NA temporairement
        TRUE  ~ NA_real_
      ))
    
    # ATTRIBUTE REFERENT FOR SINGLE PERSONS
    # ------------------------------------------
    
    # Young people still studying  (age<findet) are
    # still not given an household head. That's what we do
    
    # We know who the father is    
    x2 <- x2 %>%
      dplyr::mutate(referent = dplyr::case_when(
        # Conserve les valeurs déjà imputées
        !is.na(referent)
        ~ as.numeric(referent),
        # Si le père n'est pas mort --> father household head
        (is.na(referent)) & (pere != 0) & (ageMaxPere>age)
        ~ as.numeric(pere),
        # Si le père est mort mais pas la mère
        #    --> mother household head
        (is.na(referent)) &
          (pere != 0) & (ageMaxPere<=age) &
          (ageMaxMere > age)          
        ~ as.numeric(mere),
        # Si le père est mort et la mère aussi
        # (plus de foyer parental) --> household head
        (is.na(referent) & pere != 0) &
          (ageMaxPere<=age & ageMaxMere <= age)
        ~ as.numeric(Id),
        TRUE  ~ NA_real_
      ))
    
    # We don't know who the father is
    x2 <- x2 %>%
      dplyr::mutate(referent = dplyr::case_when(
        # Conserve les valeurs déjà imputées
        !is.na(referent)
        ~ as.numeric(referent),
        # Si le père est inconnu et mère pas morte
        #    --> mother household head
        is.na(referent) & mere != 0 & (ageMaxMere > age)
        ~ as.numeric(mere),
        # Si le père est inconnu et mère morte
        #    --> household head
        is.na(referent) & mere != 0 & (ageMaxMere <= age)
        ~ as.numeric(Id),
        # Si le père est inconnu et pas né en France
        #    --> unknown household head
        is.na(referent) & neFrance == 3
        ~ as.numeric(0),
        # Addin to ensure Theo's reproduction:
        #     Parents inconnus et né en France
        #     -> household head
        is.na(referent) & neFrance == 1
        ~ as.numeric(Id),
        # Other cases
        TRUE  ~ as.numeric(0)
      ))
    
    
    return(x2)
  }
  
  # SPLIT DATAFRAME FOR PARALLEL PROCESSING
  # -------------------------------------------
  
  # DIVIDE INTO 200 ELEMENTS
  nbId <- max(z$Id)
  nbdiv <- 200
  taille <- ceiling(nbId / nbdiv)
  liste_tranche <- split(z, z$Id %/% taille)
  
  
  # PARALLEL PROCESSING
  # -------------------------------------------
  
  # CLUSTER INITIALIZATION  
  cl <- parallel::makeCluster(parallel::detectCores()-1,
                              outfile = "")
  doSNOW::registerDoSNOW(cl)
  
  pb <- txtProgressBar(max = length(liste_tranche), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  
  df <- foreach::foreach(g = liste_tranche, .combine = "list",
                         .multicombine = TRUE,
                         .maxcombine = length(liste_tranche),
                         .options.snow=opts,
                         .export = c("descript"),
                         .packages = c("dplyr")) %dopar% {
                           referent_actif_groupe(g, descript)
                         }
  
  # CLOSE CLUSTERS  
  parallel::stopCluster(cl)
  
  
  # ARRANGE RESULTS INTO DATAFRAME
  ans <- do.call(dplyr::bind_rows,df)
  
  return(ans)
}