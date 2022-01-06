


agouti <- function(formula, data,
                   ID, inner_link,
                   outer_link,
                   family, weights, ...){

  
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






