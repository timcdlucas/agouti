
# 
# ff <- function(formula, data,
#                      ID, inner_link,
#                      outer_link,
#                      family, weights){
# 
#   m <- match.call()
#    mf <- match.call(expand.dots = FALSE)
#   return(list(m, mf))
# }
# 
# call <- ff(formula = form, d,
#         ID = ID, inner,
#         outer,
#         family, weights)
# m <- call[[1]]
# mf <- call[[2]]



agouti <- function(formula, data,
                   ID, inner_link,
                   outer_link,
                   family, weights, ...){
  
    
  # mostly stolen from glm as I want it to be consistent.
  if(is.character(family)) family <- get(family, mode = "function", envir = parent.frame()) 
  if(is.function(family)) family <- family() 
  if(is.null(family$family)) {	
    print(family)	
    stop("'family' not recognized") 
  }
  
  
  if(!is.null(weights) && !is.numeric(weights)) stop("'weights' must be a numeric vector") ## check weights and offset 
  if( !is.null(weights) && any(weights < 0) )	stop("negative weights not allowed")
  
  mf <- model.frame(formula = formula, data = data,
                    subset = NULL, weights = weights)
  
  
  X <- model.matrix(formula, mf)
  Y <- model.response(mf, "any") # e.g. factors are allowed ## avoid problems with 1D arrays, but keep names
  
  if(family$family == 'gaussian'){
    likelihood_function <- function(yhat, y, theta) dnorm(yhat, y, 3, log = TRUE)
  }
  if(inner_link == 'identity') inner <- function(x) x
  if(outer_link == 'identity') outer <- function(x) x

  ll <- function(beta) objective(beta, X, Y, ID, inner, outer, likelihood_function, weights)
  

  fit <- maxLik::maxLik(ll, start = rep(0, ncol(X)))
  
  est <- fit$estimate
  
  se <- sqrt(diag(solve(-fit$hessian)))
  
  CI <- rbind(est - 1.96 * se, 
              est + 1.96 * se)
  
  out <- list(coefficients = est,
              standard_errors = se,
              ConfInt = CI)
  class(out) <- 'agouti'
  return(out)
  
}




nll <- function(beta, x, y, ID, inner,
                outer, likelihood_function, weights){

    linear_predictor <- (x %*% beta)[, 1]

    summand <- inner(linear_predictor) * weights

    d <- data.frame(ID, summand)
    dsum <-
        d %>%
        group_by(ID) %>%
        summarise(summed = sum(summand),
                  yhat = outer(summed))

    ydf <- data.frame(y, ID)

    
    dsum <-
        dsum %>%
            left_join(ydf, by = c('ID' = 'ID')) %>%
            mutate(nll = likelihood_function(yhat, y, theta))

  return(dsum)
}





objective <- function(beta, x, y, ID, inner, outer, likelihood_function, weights){

   dsum <- nll(beta, x, y, ID, inner, outer, likelihood_function, weights)

  nll <- sum(dsum$nll)
 return(nll)
}






