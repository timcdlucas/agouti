#' Fit an aggregated output GLM
#' 
#' This function has flexibility for link functions both inside and outside the
#' aggregation step as well as a variety of error models. The models are fitted
#' with maximum likelihood and confidence intervals are given using the normal
#' approximation to the likelihood surface.
#' 
#' @export
#' @param formula A formula for the y and x variables.
#' @param data A data frame suitable for aggregate outputs models.
#' @param ID The column to use as the group ID column. Unquoted value.
#' @param inner_link A character defining the inner link function.
#' @param outer_link A character defining the outer link function.
#' @param family The error distribution defined with a family object or character.
#' @param weights Which column to use as within group weights such as 
#'   population or time.
#' @param ... Further parameters. Currently unused.
#' 
#' @examples
#' data(madagascar_malaria)
# agoutiGLM(case_rate ~ LSTmean, data = madagascar_malaria,
#           ID = madagascar_malaria$ID, weights = madagascar_malaria$pop,
#           inner_link = 'identity', outer_link = 'identity',
#           family = 'gaussian')


agoutiGLM <- function(formula, data,
                      ID, inner_link = 'identity',
                      outer_link = 'identity',
                      family = stats::gaussian(), weights, ...){
  
    
  # mostly stolen from glm as I want it to be consistent.
  if(is.character(family)) family <- get(family, mode = "function", envir = parent.frame()) 
  if(is.function(family)) family <- family() 
  if(is.null(family$family)) {	
    print(family)	
    stop("'family' not recognized") 
  }
  
  
  if(!is.null(weights) && !is.numeric(weights)) stop("'weights' must be a numeric vector") ## check weights and offset 
  if( !is.null(weights) && any(weights < 0) )	stop("negative weights not allowed")
  
  mf <- stats::model.frame(formula = formula, data = data,
                    subset = NULL, weights = weights)
  
  
  X <- stats::model.matrix(formula, mf)
  Y <- stats::model.response(mf, "any") # e.g. factors are allowed ## avoid problems with 1D arrays, but keep names
  
  if(family$family == 'gaussian'){
    likelihood_function <- function(yhat, y, theta) stats::dnorm(yhat, y, 3, log = TRUE)
  }

  
  links <- 
    list(identity = function(x) x,
         exp = function(x) exp(x),
         logit = function(x) plogis(x),
         probit = function(x) pnorm(x))
  
  
  inverse_links <- 
    list(identity = function(x) x,
         exp = function(x) log(x),
         logit = function(x) qlogis(x),
         probit = function(x) qnorm(x))
  
  inner <- do.call(switch, c(inner_link, links))
  outer <- do.call(switch, c(outer_link, links))
  

  
  ll <- function(beta) objective(beta, X, Y, ID, inner, outer, likelihood_function, weights)
  

  fit <- maxLik::maxLik(ll, start = rep(0, ncol(X)))
  
  hess <- numDeriv::hessian(ll, fit$estimate)
  
  est <- fit$estimate
  
  se <- sqrt(diag(solve(-hess)))
  
  CI <- rbind(est - 1.96 * se, 
              est + 1.96 * se)
  
  out <- list(coefficients = est,
              standard_errors = se,
              ConfInt = CI)
  class(out) <- 'agoutiGLM'
  return(out)
  
}


#' Calculate the log likelihood for each observation given a set of parameters.
#' 
#' @param beta The parameter vector
#' @param x covariate matrix
#' @param y observation vector.
#' @param ID The column to use as the group ID column. Unquoted value.
#' @param inner A character defining the inner link function.
#' @param outer A character defining the outer link function.
#' @param likelihood_function The likelihood (error) function given as a 
#' function such as dnorm.
#' @param weights Which column to use as within group weights such as 
#'   population or time.
#' @importFrom rlang .data


loglike <- function(beta, x, y, ID, inner,
                outer, likelihood_function, weights){

    linear_predictor <- (x %*% beta)[, 1]

    summand <- inner(linear_predictor) * weights

    d <- data.frame(ID, summand)
    dsum <-
        d %>%
        dplyr::group_by(ID) %>%
        dplyr::summarise(summed = sum(.data$summand),
                         yhat = outer(.data$summed))

    ydf <- data.frame(y, ID) %>% 
             dplyr::distinct(y, ID)

    
    dsum_join <-
        dsum %>%
            dplyr::left_join(ydf, by = c('ID' = 'ID')) %>%
            dplyr::mutate(loglike = likelihood_function(.data$yhat, .data$y, .data$theta))

  return(dsum_join)
}



#' Calculate the log likelihood for the full dataset given a set of parameters.
#' 
#' @param beta The parameter vector
#' @param x covariate matrix
#' @param y observation vector.
#' @param ID The column to use as the group ID column. Unquoted value.
#' @param inner A character defining the inner link function.
#' @param outer A character defining the outer link function.
#' @param likelihood_function The likelihood (error) function given as a 
#' function such as dnorm.
#' @param weights Which column to use as within group weights such as 
#'   population or time.


objective <- function(beta, x, y, ID, inner, outer, likelihood_function, weights){

   dsum <- loglike(beta, x, y, ID, inner, outer, likelihood_function, weights)

   loglike <- sum(dsum$loglike)
 return(loglike)
}






