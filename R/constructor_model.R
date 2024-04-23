#' Construct interest rate model in a heterogeneous or homogeneous agents model
#'
#' @param r A numeric value or a formula applied to data
#' @param gamma A numeric value or a formula applied to data
#' @param beta A numeric value or a formula applied to data
#' @param data Dataframe where r should be created
#' @param r.parameters Parameters value that should be used if
#'  r is a formula
#' @param beta.parameters Parameters value that should be used if
#'  beta is a formula
#' @param gamma.parameters Parameters value that should be used if
#'  gamma is a formula
#' @param ... Additional arguments that should be passed
#' @importFrom stats as.formula model.matrix
#' @export
constructor_interest <- function(r, data, r.parameters = NULL,
                                 ...){
  if (isTRUE("r" %in% colnames(data))) data[,'r' :=  NULL]
  UseMethod("constructor_interest")
}

#' @rdname constructor_interest
#' @export
#' @export
constructor_gamma <- function(gamma, data, gamma.parameters = NULL,
                                 ...){
  if (isTRUE("gamma" %in% colnames(data))) data[,'gamma' :=  NULL]
  UseMethod("constructor_gamma")
}

#' @rdname constructor_interest
#' @export
#' @export
constructor_beta <- function(beta, data, beta.parameters = NULL,
                              ...){
  if (isTRUE("beta" %in% colnames(data))) data[,'beta' :=  NULL]
  UseMethod("constructor_beta")
}



# INTEREST --------------------


#' @rdname constructor_interest
#' @export
constructor_interest.default <- function(r, data, r.parameters = NULL, ...){
  return(
    data[, "r" := r]
  )
}

#' @rdname constructor_interest
#' @export
constructor_interest.double <- function(r, data, r.parameters = NULL, ...){
  return(
    data[, "r" := r]
  )
}

#' @rdname constructor_interest
#' @export
constructor_interest.formula <- function(r, data, r.parameters = NULL, ...){
  args <- list(...)
  
  if (is.null(r.parameters)) stop("When r is a formula, r.parameters vector should be provided")
  
  # LEFT HAND SIDE IS SECOND TERM
  formula <- r[-2]
  model_interest <- model.matrix(object = formula, data = data)
  model_interest <- model_interest %*% r.parameters
  
  return(
    data[, "r" := as.numeric(model_interest)]
  )
}

#' @rdname constructor_interest
#' @export
constructor_interest.character <- function(r, data, r.parameters = NULL, ...){
  return(
    constructor_interest.formula(r = as.formula(r), data = data, r.parameters = r.parameters, ...)
  )
}



# GAMMA ---------------------------


# INTEREST --------------------


#' @rdname constructor_gamma
#' @export
constructor_gamma.default <- function(gamma, data, gamma.parameters = NULL, ...){
  return(
    data[, "gamma" := gamma]
  )
}

#' @rdname constructor_gamma
#' @export
constructor_gamma.double <- function(gamma, data, gamma.parameters = NULL, ...){
  return(
    data[, "gamma" := gamma]
  )
}

#' @rdname constructor_gamma
#' @export
constructor_gamma.formula <- function(gamma, data, gamma.parameters = NULL, ...){
  
  args <- list(...)
  
  if (is.null(gamma.parameters)) stop("When gamma is a formula, gamma.parameters vector should be provided")
  
  # LEFT HAND SIDE IS SECOND TERM
  formula <- gamma[-2]
  model_interest <- model.matrix(formula, data)
  model_interest <- model_interest %*% gamma.parameters
  
  return(
    data[, "gamma" := as.numeric(model_interest)]
  )
}

#' @rdname constructor_gamma
#' @export
constructor_gamma.character <- function(gamma, data, gamma.parameters = NULL, ...){
  return(
    constructor_gamma.formula(gamma = as.formula(gamma), data = data, gamma.parameters = gamma.parameters, ...)
  )
}

# beta ------------------------------

#' @rdname constructor_beta
#' @export
constructor_beta.default <- function(beta, data, beta.parameters = NULL, ...){
  return(
    data[, "beta" := beta]
  )
}

#' @rdname constructor_beta
#' @export
constructor_beta.double <- function(beta, data, beta.parameters = NULL, ...){
  return(
    data[, "beta" := beta]
  )
}

#' @rdname constructor_beta
#' @export
constructor_beta.formula <- function(beta, data, beta.parameters = NULL, ...){
  
  args <- list(...)
  
  if (is.null(beta.parameters)) stop("When beta is a formula, beta.parameters vector should be provided")
  
  # LEFT HAND SIDE IS SECOND TERM
  formula <- beta[-2]
  model_interest <- model.matrix(formula, data)
  model_interest <- model_interest %*% beta.parameters
  
  return(
    data[, "beta" := as.numeric(model_interest)]
  )
}

#' @rdname constructor_gamma
#' @export
constructor_beta.character <- function(beta, data, beta.parameters = NULL, ...){
  return(
    constructor_beta.formula(beta = as.formula(beta), data = data, beta.parameters = beta.parameters, ...)
  )
}
