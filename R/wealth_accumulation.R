

#' Simulate wealth dynamics at household level
#' 
#' Main function of \code{capitulation} package to simulate
#'  wealth accumulation across time. Handle family breaks and marriage
#'  as well as capitalize wealth given behavioral assumptions.
#'  
#' @inheritParams starting_wealth
#' @inheritParams inherit_wealth
#' @inheritParams last_change_indiv
#' @inheritParams transform_household
#' @inheritParams estimate_K0
#' @param years Sequence of years to consider. By default from 2009 to 2100
#' @param r Exogeneous interest rate
#' @param verbose Logical value indicating whether we want to print some messages
#'  during computations
#' @param return_split Logical value indicating whether we want to return pivot
#'  years where family change ocurred
#' @param start_year Year where microsimulation starts 
#' @param findet_var Variable name storing end of year studying information
#' @param return_last Logical value indicating whether we want to return
#'   \eqn{Kt} sequence up to `T-1` (`FALSE`, default) or `T` (`TRUE`)
#' @return Dataframe with wealth variable simulated
#' @export
#' @importFrom data.table %between%

wealth_accumulation <- function(household_table, years = 2009:2070,
                                start_year = 2009,
                                r = 0.02,
                                gamma = 0.5,
                                beta = 1,
                                verbose = TRUE,
                                age_var = "age",
                                findet_var = "findet",
                                income_var = "salaire_tot",
                                wealthvar_survey = "PATFISOM",
                                weight_var = NULL,
                                return_split = FALSE,
                                return_last = FALSE){
  
  
  # # COPY DATA.TABLE OBJECT TO AVOID MODIFY BY REFERENCE  
  # household_table2 <- data.table::copy(household_table)
  # 
  # 
  # # KEEP ONLY YEARS WHERE LIFE-CYCLE APPROACH MAKES SENSE
  # household_table2 <- household_table2[get(age_var)>=get(findet_var)]
  
  # NOW THAT K0 IS ESTIMATED, RESTRICTIONS CAN BE MADE
  household_table2 <- household_table[get('annee') %between% c(start_year, max(years))]
  
  if ('wealth' %in% colnames(household_table2)) household_table2[,'wealth' := NULL]
  
  # CREATE TAU VECTOR
  household_table2[,'tau' := get('age') - get('findet')]
  
  
  # CREATE SEQUENCE OF YEARS  
  years <- seq(from = min(years),
               to = min(c(max(years),max(household_table2$annee))),
               by = 1)
  
  updated_tab <- NULL
  
  
  
  
  # GET 2009 WEALTH POINT INTO VECTOR
  # --------------------------------------
  
  household_table2[,`:=`('wealth2009' = mean(get(wealthvar_survey), na.rm = TRUE)),
                   by = c('Id')]
  
  # IMMIGRANTS ARE ASSUMED TO ARRIVE WITH 0 WEALTH  (Faster than ifelse)
  household_table2[!is.finite(get('wealth2009')), 'wealth2009' := 0L]
  
  
  if (is.null(weight_var)){
    household_table2[, "weight" := 1L]
    weight_var <- "weight"
  }
  
  # WE INDIVIDUALIZE OBSERVED WEALTH
  household_table2[get('annee') == start_year, 'tempweight' := get(weight_var)]
  household_table2[, 'weight2009' := mean(get("tempweight"), na.rm = TRUE),
                   by = c('Id')]
  household_table2[,'tempweight' := NULL]
  household_table2[, ('wealth2009') := get("wealth2009")/get('weight2009')]
  
  df <- household_table2
  
  
  if (!return_last){
    
    # Create progress bar operator
    if (verbose){
      
      grpn <- data.table::uniqueN(df$Id)
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
      
      df[,'wealth' := {setTxtProgressBar(pb, .GRP) ;
        wealth <- simulate_wealth_structural_beta2(
          K0 = get('wealth2009'),
          consumption0 = get('C0'),
          income = get(income_var),
          UC = get(weight_var),
          tau = get('tau'),
          r = r,
          gamma = gamma,
          beta = beta,
          returnLast = return_last
        );
      }, by = c('Id')]
      
    } else{
      
      df[,'wealth' := simulate_wealth_structural_beta2(
        K0 = get('wealth2009'),
        consumption0 = get('C0'),
        income = get(income_var),
        UC = get(weight_var),
        tau = get('tau'),
        r = r,
        gamma = gamma,
        beta = beta,
        returnLast = return_last
      ), by = c('Id')]
      
    }
    
  } else{
    
    # Create progress bar operator
    if (verbose){
      
      grpn <- data.table::uniqueN(df$Id)
      pb <- txtProgressBar(min = 0, max = grpn, style = 3)
      
      df[,'wealth' := {setTxtProgressBar(pb, .GRP) ;
        wealth <- simulate_wealth_structural_beta2(
          K0 = get('wealth2009'),
          consumption0 = get('C0'),
          income = get(income_var),
          UC = get(weight_var),
          tau = get('tau'),
          r = r,
          gamma = gamma,
          beta = beta,
          returnLast = return_last
        )[-1];
      }, by = c('Id')]
      
    } else{
      
      df[,'wealth' := simulate_wealth_structural_beta2(
        K0 = get('wealth2009'),
        consumption0 = get('C0'),
        income = get(income_var),
        UC = get(weight_var),
        tau = get('tau'),
        r = r,
        gamma = gamma,
        beta = beta,
        returnLast = return_last
      )[-1], by = c('Id')]
      
    }    
    
  }
  
  
  
  return(df)
}



