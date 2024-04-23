
#' Generate a fake \code{pre_indiv} table
#' 
#' @param Nindiv Number of individuals
#' @param anaiss Birth years
#' @param lifetime Lifetime for individuals in dataset
#' @param findet End of studying year vector
#' @return Dataframe
#' @export

generate_pre_indiv <- function(Nindiv = 2, anaiss = c(1980,1990),
                               lifetime = c(50,60),
                               findet = c(25,22)){
  
  if (is.null(anaiss)) anaiss <- sample(1980:2030, size = Nindiv, replace = TRUE)
  if (is.null(lifetime)) lifetime <- sample(50:80, size = Nindiv, replace = TRUE)
  if (is.null(findet)) findet <- sample(18:30, size = Nindiv, replace = TRUE)
  

  df <- lapply(seq_len(Nindiv), function(i) ({
    dt <- data.table::data.table(
      data.frame("Id" = i,
                 "annee" = seq(anaiss[i],anaiss[i]+lifetime[i]-1),
                 "anaiss" = anaiss[i],
                 "findet" = findet[i]
      )
    )
    
  })
  )
  df <- data.table::rbindlist(df)
  
  df[,'age' := seq_len(.N), by = 'Id']
  df[, `:=` ('age' = get('age')-1)]
  df[,'salaire' := as.numeric(get('age')>=get('findet'))]
  df[, `:=`('salaire' = get('salaire')*exp(rnorm(nrow(df))))]
  
  df <- df[,.SD,.SDcols = c("Id","age","annee","salaire","anaiss")]
}


#' Generate fake \code{description} table
#' @inheritParams generate_pre_indiv
#' @export

generate_description <- function(Nindiv = 4, anaiss = c(1980,1990),
                                 lifetime = c(50,60),
                                 findet = c(25,22)){
  
  if (is.null(anaiss)) anaiss <- sample(1980:2030, size = Nindiv, replace = TRUE)
  if (is.null(lifetime)) lifetime <- sample(60:80, size = Nindiv, replace = TRUE)
  if (is.null(findet)) findet <- sample(18:30, size = Nindiv, replace = TRUE)
 
  df <- lapply(seq_len(Nindiv), function(i) ({
    dt <- data.table::data.table(
      data.frame("Id" = i,
                 "sexe" = i %% 2,
                 "neFrance" = 1,
                 "anaiss" = anaiss[i],
                 "findet" = findet[i],
                 "ageMax" = lifetime[i],
                 "ageliq" = sample(50:59, size = 1L, replace = FALSE)
      ))
  })
  )
 
  df <- data.table::rbindlist(df)
  
  df[, 'rn' := seq_len(nrow(df))-1]
  
  df[,'family' := floor(get('rn')/4)]
  df[,'pere' := min(get('Id')), by = 'family']
  df[,'mere' := min(get('Id'))+1, by = 'family']
  
  df[(get('Id') == get('pere') |
        get('Id') == get('mere')),`:=`('pere' = NA,
                                       'mere' = NA)]
  
  df <- df[,.SD,.SDcols = c("Id", "sexe",
                            "neFrance",
                            "anaiss", "findet", 
                            "ageMax",
                            "pere", "mere",
                            "ageliq")]  
  
  return(df)
}




#' Generate fake \code{Destinie} data
#' @inheritParams generate_pre_indiv
#' @importFrom stats median rnorm
#' @export

generate_fake_data <- function(Nindiv = 4, anaiss = NULL,
                               lifetime = NULL,
                               findet = NULL){
  
  if (is.null(anaiss)) anaiss <- sample(1980:2030, size = Nindiv, replace = TRUE)
  if (is.null(lifetime)) lifetime <- sample(60:80, size = Nindiv, replace = TRUE)
  if (is.null(findet)) findet <- sample(18:30, size = Nindiv, replace = TRUE)
  
  descript <- generate_description(Nindiv = Nindiv, anaiss = anaiss,
                                   lifetime = lifetime,
                                   findet = findet)
  
  pre_indiv <- generate_pre_indiv(Nindiv = Nindiv, anaiss = anaiss,
                                  lifetime = lifetime,
                                  findet = findet)
  
  return(
    list("pre_indiv" = pre_indiv,
         "descript" = descript)
  )
}
