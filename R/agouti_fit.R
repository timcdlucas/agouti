


agouti <- function(formula, data,
                   ID, inner_link,
                   outer_link,
                   family, weights, ...){

# mostly stolen from glm as I want it to be consistent.
call <- match.call() ## family if(is.character(family)) family <- get(family, mode = "function", envir = parent.frame()) if(is.function(family)) family <- family() if(is.null(family$family)) {	print(family)	stop("'family' not recognized") }

  
Y <- model.response(mf, "any") # e.g. factors are allowed ## avoid problems with 1D arrays, but keep names

## avoid any problems with 1D or nx1 arrays by as.vector. weights <- as.vector(model.weights(mf)) if(!is.null(weights) && !is.numeric(weights)) stop("'weights' must be a numeric vector") ## check weights and offset if( !is.null(weights) && any(weights < 0) )	stop("negative weights not allowed")

mf <- match.call(expand.dots = FALSE) m <- match(c("formula", "data", "subset", "weights", "na.action", "etastart", "mustart", "offset"), names(mf), 0L) mf <- mf[c(1L, m)] mf$drop.unused.levels <- TRUE ## need stats:: for non-standard evaluation mf[[1L]] <- quote(stats::model.frame) mf <- eval(mf, parent.frame())

mt <- attr(mf, "terms") # allow model.frame to have updated it


X <- model.matrix(mt, mf, contrasts)



  
  # Easy setup for now.
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = 2)
  yy <- rnorm(N1)
  ID <- sample(letters[seq(N2)], N1, replace = TRUE)
  weights <- runif(N1)
  inner <- function(x) x
  outer <- function(x) x
  
  likelihood_function <- function(yhat, y, theta) dnorm(yhat, y, 3, log = TRUE)

  
  ll <- function(beta) objective(beta, x, yy, ID, inner, outer, likelihood_function, weights)
  
  fit <- maxLik::maxLik(ll, start = rep(0, length(beta)))
  
  est <- fit$estimate
  
  se <- sqrt(diag(solve(-fit$hessian)))
  
  CI <- rbind(est - 1.96 * se, 
              est + 1.96 * se)
  
  out <- list(coefficients = est,
              standard_errors = se,
              ConfInt = CI)
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






