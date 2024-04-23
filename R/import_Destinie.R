#' Import Destinie demographic simulations
#' 
#' Import simulations from Destinie model
#' 
#' @details The following output from INSEE's Destinie model are required:
#' \itemize{
#'  \item{\code{emp}: Individual identifiers} 
#'  \item{\code{salairenet}: Simulated individual income path}
#'  \item{\code{ech}: Simulated family structure}
#'  \item{\code{macro}: Macroeconomic variables (simulated or observed)}
#'  \item{\code{fam}: Another table regarding family links}
#'  \item{\code{liquidations}: Simulated retirement decision}
#'  \item{\code{retraites}: Simulated pensions}
#'  }
#'  
#'  They should be stored as \code{.Rda} files
#'  
#' @source  <https://github.com/InseeFr/Destinie-2>
#' 
#' @param filenames Names of the files, excluding extension
#' @param path_data Directory where files should be found
#' @param format Should we use \code{tidyverse} approach (format = "dplyr") or
#'  \code{data.table} approach (format = "data.table"). Default to "\emph{dplyr}"
#'  
#' @note It is recommended to favor \link{data.table} approach though it is not,
#'  for compability and continuity purposes, the default approach. This choice
#'  determines following steps: all functions have been built to adapt computations
#'  with format required
#' 
#' @return List where each slot stores a \link[tibble]{tibble} or a
#'  \link[data.table]{data.table} objects with simulations
#'  from Destinie
#' @seealso \link{arrange_Destinie} to get intermediary tables
#' @examples \dontrun{
#' path_data <- "./data"
#' simu <- capitulation::import_Destinie(path_data = path_data)
#' tables <- capitulation::arrange_Destinie(simu)
#' 
#' # If you prefer data.table approach
#' simu <- capitulation::import_Destinie(path_data = path_data, format = 'data.table')
#' tables <- capitulation::arrange_Destinie(simu)
#' }
#' @export


import_Destinie <- function(filenames = c("emp","salairenet","ech","macro","fam","liquidations","retraites"),
                         path_data = getwd(), format = c("dplyr", "data.table"),
                         extension = ".Rda"){
  
  # Avoid NSE notes
  tempdf<-NULL
  
  if (missing(format)) format <- "dplyr"
  if (!any(format %in% c("dplyr", "data.table"))) stop("'format' argument must be 'dplyr' or 'data.table'")

  # IMPORT SIMULATIONS FROM DESTINIE
  import_simu <- function(filename = "emp", path_data = "V:/DG75-G210/Lino/Patrimoine"){
    

    # IMPORT DATA
    load(paste0(path_data,"/",filename,extension))
    
    
    # RETURN THAT VECTOR  
    if (format == "dplyr"){
      eval(parse(text = paste0("tempdf <- ", filename, "%>% dplyr::tbl_df()"
      )
      ))
    } else{
      eval(parse(text = paste0("tempdf <- ", filename, "%>% data.table::as.data.table()"
      )
      ))
    }
    
    return(tempdf)
    
  }
  
  
  simul <- lapply(filenames, function(f)
    import_simu(filename = f, path_data = path_data))
  
  names(simul) <- filenames
  
  return(simul)
}
