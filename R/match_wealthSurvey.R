#' Match wealth household survey with Destinie data
#' 
#' Match simulated household in Destinie with their counterpart in
#'  observed data (household survey 'Enquete Patrimoine').
#' @param data Destinie data
#' @param path_survey Directory where survey data are stored
#' @param filename_survey Survey data filename 
#' @param level_matching Should we match at individual or household level. For the moment,
#'  only household level is implemented
#' @param id_wealthSurvey Identifier in household survey
#' @param age_wealthSurvey Age in household survey
#' @param wealth_wealthSurvey Wealth in household survey
#' @param findet_wealthSurvey Studying end age in household survey
#' @param .colsWealth Columns of interest in survey data
#' @param estimation_year Estimation year in wealth survey
#' @param drawK Logical value indicating whether we would like to draw observed value
#'  (wealth, age) for people that have no wealth observed in \code{estimation_year}
#' 
#' @return \code{data} augmented with 'Enquete Patrimoine' information for 2009 year
#' 
#' @importFrom haven read_sas
#' @import stringr
#' @export


match_wealthSurvey <- function(data,
                               path_survey = "./inst/dataINSEE",
                               filename_survey = "basecomplete0_62663.sas7bdat",
                               level_matching = c("household","individual"),
                               id_wealthSurvey = 'IDENTMEN',
                               age_wealthSurvey = 'AGEPR',
                               wealth_wealthSurvey = 'PATFISOM',
                               debt_wealthSurvey = NULL, 
                               findet_wealthSurvey =  "AGFINETU",
                               .colsWealth = c('IDENTIND','IDENTMEN',
                                               'age','AGEPR','POND',
                                               "MTDETTES", 'PATRI_NET',
                                               'PATRI_BRUT',
                                               'PATFI','PATFIMTC_DECL',
                                               'PATFISOM','PATIMM',
                                               'PATPROFENT','PATPROFHENT',
                                               'PATRIC_DECL','AGFINETU'),
                               estimation_year = 2009L,
                               time_0 = c("graduation","birth"),
                               drawK = FALSE
){
  
  . <- NULL

  if (missing(level_matching)) level_matching <- "household"
  
  time_0 <- match.arg(time_0)
  
  
  # IMPORT HOUSEHOLD WEALTH SURVEY
  # ----------------------------------
  
  if (endsWith(filename_survey,"sas7bdat")){
    df <- haven::read_sas(paste0(path_survey,"/Enquete Patrimoine/",filename_survey))
    df <- data.table::data.table(df)
  }else{
    df <- data.table::fread(paste0(path_survey,"/Enquete Patrimoine/",filename_survey))
  }
  
  # KEEP ONLY HOUSEHOLD HEAD INFORMATION: IDENTIND = IDENT || NOI
  # --------------------------------------------------------------------
  
  # Extract NOI
  df[,'NOI' := as.numeric(stringr::str_sub(get("IDENTIND"),start = -2))]
  # Head: minimal NOI
  df[, 'minNOI' := min(get('NOI'),na.rm=TRUE), , by = id_wealthSurvey]
  df <- df[get('NOI') == get('minNOI')]
  
  # KEEP RELEVENT INFORMATION
  df <- if (!is.null(.colsWealth)) df[,.SD,
                                      .SDcols = .colsWealth] else df[,.SD,
                                                                     .SDcols = c(id_wealthSurvey,
                                                                                 age_wealthSurvey,
                                                                                 wealth_wealthSurvey,
                                                                                 findet_wealthSurvey)]
  df <- unique(df)
  
  # IMPORT CORRESPONDANCE TABLE
  # ----------------------------------
  
  load(paste0(path_survey,"/Destinie/table.Rda"))
  table <- data.table::as.data.table(table)
 
  # MATCH WITH DESTINIE
  # ----------------------------------
  
  
  # If wealth shoube be net of debt, retrieve debt
  if (!is.null(debt_wealthSurvey)) df <- df[, c(wealth_wealthSurvey) := get(wealth_wealthSurvey) - get(debt_wealthSurvey)]
  
  df <- df[,.SD,.SDcols = c(id_wealthSurvey,
                            wealth_wealthSurvey,
                            findet_wealthSurvey)]
  
  df[,'annee' := estimation_year]
  
  
  data.table::setkeyv(df,id_wealthSurvey)
  data.table::setkeyv(table,id_wealthSurvey)
  
  table[,c(age_wealthSurvey) := estimation_year - get('ANAIS')]
  
  pat_info <- merge(df,table)[,c('anaiss','ANAIS') := NULL]
  pat_info <- pat_info[get(age_wealthSurvey) >= get(findet_wealthSurvey)]
    
  data.table::setkeyv(pat_info, c("Id","annee"))
  data.table::setkeyv(data, c("Id","annee"))
  destinie_augmented <- merge(data,pat_info,all.x = TRUE)
  if (time_0 == "graduation") destinie_augmented <- destinie_augmented[get("age")>= get("findet")]
  
  

  if (!drawK) return(destinie_augmented)  
  
  # ADD K0 FOR PEOPLE NOT OBSERVED IN 2009
  # -----------------------------------------------
  
  destinie_to_complete <- data.table::copy(destinie_augmented)
  destinie_to_complete <- destinie_to_complete[, .(wealth2009 = mean(get(wealth_wealthSurvey),na.rm=TRUE)),
                                             by = 'Id']
  destinie_to_complete <- destinie_to_complete[is.nan(get('wealth2009'))][['Id']]

  data_incomplete <- data.table::data.table(
    'Id' = unique(destinie_to_complete)
  )

  
  data_incomplete <- cbind(
    data_incomplete, pat_info[sample(.N, size = nrow(data_incomplete), replace = TRUE), .SD,
           .SDcols = c(wealth_wealthSurvey, age_wealthSurvey)]
  )

  data.table::setnames(data_incomplete, old = "AGEPR",
                       new = "age")
  
  
  data.table::setkeyv(data_incomplete, "Id")
  
  # WE MUST TAKE CARE OF THE FACT THAT OBSERVED WEALTH SHOULD BE WHEN INDIVIDUAL IS ALIVE
  data_incomplete <- merge(data_incomplete,
        destinie_augmented[,.SD,.SDcols = c('Id','age')][,.('age_max' = max(get('age'), na.rm = TRUE)-1), by = 'Id'],
        by = 'Id')
  
  # WE LOSE PEOPLE ONLY OBSERVED IN 2070
  data_incomplete <- data_incomplete[get('age_max')>0]

  # WE KEEP DATE BEFORE DEATH
  data_incomplete[, 'age' := pmin(get('age'),get('age_max'))]
  data_incomplete[,'age_max' := NULL]
  
  
  data_incomplete <- merge(data_incomplete, unique(destinie_augmented[,.SD,.SDcols = c('Id','findet')]))
  
  data_incomplete[,'age_min' := get('findet')+1]
  data_incomplete[, 'age' := pmax(get('age'),get('age_min'))]
  data_incomplete[, c('age_min','findet') := NULL]
  
  destinie_completed <- merge(destinie_augmented, data_incomplete,
                              by = c('Id','age'),
                              all.x = TRUE, suffixes = c("","_complement"))
  
  destinie_completed[, c(wealth_wealthSurvey) := pmin(get(wealth_wealthSurvey),
                                        get(paste0(wealth_wealthSurvey,"_complement")), na.rm = TRUE)]
  
  destinie_completed[,paste0(wealth_wealthSurvey,"_complement") := NULL]
  
  
  # IL RESTE A GERER LES CAS DES PERSONNES QUI MEURENT LORS DE LEUR PREMIER ANNEE DE CYCLE DE VIE --> 0 WEALTH
  death_young <- destinie_completed[,.('N_imputations' = sum(!is.na(get(wealth_wealthSurvey)))),
                                    by = "Id"]
  death_young <- death_young[get('N_imputations') == 0]
  destinie_completed[get('Id') %in% death_young[['Id']], c(wealth_wealthSurvey) := 0]
  
  
  # INDICATOR WHEN OBSERVED/SIMULATED WEALTH
  destinie_completed[,'tt' := as.numeric(!is.na(get(wealth_wealthSurvey)))]

  return(destinie_completed)    
}
