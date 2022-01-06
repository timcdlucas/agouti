context('Test the model fitting functions fully')

test_that('nll works with Gaussian family.', {
  
  # nll <- function(beta, x, y, ID, inner,
  #                outer, likelihood_function, weights){
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

  out <- nll(beta, x, yy, ID, inner, outer, likelihood_function, weights)

  expect_true(all(!(is.na(out$nll))))
  expect_true(all(!(is.nan(out$nll))))
  

})



test_that('nll works with 1 covariate.', {
  
  beta <- 2
    
    
    
    
})


test_that('Objective function works', {
  
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
  nll_val <- objective(beta, x, yy, ID, inner, outer, likelihood_function, weights)
  
  
  expect_true(length(nll_val) == 1)
  expect_true(!is.na(nll_val))
  expect_true(is.numeric(nll_val))
  
  
})


test_that('We can optimise a model', {
  
  
  out <- agouti()
  
})


