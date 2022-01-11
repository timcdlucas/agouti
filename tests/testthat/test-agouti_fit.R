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


test_that('Objective function works with gaussian.', {
  
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


test_that('agouti function works with gaussian.', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = 2)
  yy <- rnorm(N1)
  ID <- sample(letters[seq(N2)], N1, replace = TRUE)
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form <- yy ~ X1 + X2
  

  out <- agouti(formula = form, d,
                ID = ID, inner,
                outer,
                family, weights)
    
  
  expect_true(class(out) == 'agouti')

  
})

test_that('agouti function works with different link functions', {})

test_that('agouti function works with 1 covariate', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = 2)
  yy <- rnorm(N1)
  ID <- sample(letters[seq(N2)], N1, replace = TRUE)
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form <- yy ~ X1
  
  
  out <- agouti(formula = form, d,
                ID = ID, inner,
                outer,
                family, weights)
  
  
  expect_true(class(out) == 'agouti')
  expect_true(length(out$coefficients) == 2)
  
  
  
})

test_that('agouti function works with interactions, squared covs and factors', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = 2)
  yy <- rnorm(N1)
  ID <- sample(letters[seq(N2)], N1, replace = TRUE)
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form1 <- yy ~ X1 + I(X2^2)
  
  
  out1 <- agouti(formula = form1, d,
                ID = ID, inner,
                outer,
                family, weights)
  
  
  expect_true(class(out1) == 'agouti')
  expect_true(length(out1$coefficients) == 3)
  
  
  
  
  form2 <- yy ~ X1 * X2
  
  
  out2 <- agouti(formula = form2, d,
                 ID = ID, inner,
                 outer,
                 family, weights)
  
  
  expect_true(class(out2) == 'agouti')
  expect_true(length(out2$coefficients) == 3)
  
  d2 <- d
  d2$X2 <- factor(cut(d2$X2, c(-10, -1, 1, 10)))

  form <- yy ~ X1 + X2
  
  out3 <- agouti(formula = form, d2,
                 ID = ID, inner,
                 outer,
                 family, weights)
  
  
  expect_true(class(out3) == 'agouti')
  expect_true(length(out3$coefficients) == 4)
  
})


  




