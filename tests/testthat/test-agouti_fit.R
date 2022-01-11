context('Test the model fitting functions fully')

test_that('loglike works with Gaussian family.', {
  
  # nll <- function(beta, x, y, ID, inner,
  #                outer, likelihood_function, weights){
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
    
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  
  weights <- runif(N1)
  inner <- function(x) x
  outer <- function(x) x

  likelihood_function <- function(yhat, y, theta) dnorm(yhat, y, 3, log = TRUE)

  out <- loglike(beta, x, yy, ID, inner, outer, likelihood_function, weights)

  expect_true(all(!(is.na(out$loglike))))
  expect_true(all(!(is.nan(out$loglike))))
  

})



test_that('loglike works with 1 covariate.', {
  
  
  # nll <- function(beta, x, y, ID, inner,
  #                outer, likelihood_function, weights){
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 1
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  weights <- runif(N1)
  inner <- function(x) x
  outer <- function(x) x
  
  likelihood_function <- function(yhat, y, theta) dnorm(yhat, y, 3, log = TRUE)
  
  out <- loglike(beta, x, yy, ID, inner, outer, likelihood_function, weights)
  
  expect_true(all(!(is.na(out$loglike))))
  expect_true(all(!(is.nan(out$loglike))))    
    
    
    
})


test_that('Objective function works with gaussian.', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  weights <- runif(N1)
  inner <- function(x) x
  outer <- function(x) x
  
  likelihood_function <- function(yhat, y, theta) dnorm(yhat, y, 3, log = TRUE)
  ll_val <- objective(beta, x, yy, ID, inner, outer, likelihood_function, weights)
  
  
  expect_true(length(ll_val) == 1)
  expect_true(!is.na(ll_val))
  expect_true(is.numeric(ll_val))
  
  
})


test_that('agouti function works with gaussian.', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form <- yy ~ X1 + X2
  

  out <- agoutiGLM(formula = form, d,
                ID = ID, inner,
                outer,
                family, weights)
    
  
  expect_true(class(out) == 'agoutiGLM')

  
})

test_that('agouti function works with different link functions', {
  
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  weights <- runif(N1)
  
  
  family <- gaussian()
  
  form <- yy ~ X1 + X2
  
  yy <- exp(rnorm(N2)[order])
  d <- data.frame(x, yy, ID, weights)
  
  out1 <- agoutiGLM(formula = form, d,
                   ID = ID, inner_link = 'exp',
                   outer_link,
                   family, weights)
  
  yy <- plogis(rnorm(N2)[order])
  d <- data.frame(x, yy, ID, weights)
  
  out2 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'logit',
                    outer_link,
                    family, weights)
  
  
  out3 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'probit',
                    outer_link,
                    family, weights)
  
  
  out4 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'identity',
                    outer_link = 'exp',
                    family, weights)
  
  
  out5 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'identity',
                    outer_link = 'logit',
                    family, weights)
  
  
  out5 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'identity',
                    outer_link = 'probit',
                    family, weights)
  
  yy <- exp(rnorm(N2, 5)[order])
  d <- data.frame(x, yy, ID, weights)
  
  out6 <- agoutiGLM(formula = form, d,
                    ID = ID, inner_link = 'exp',
                    outer_link = 'exp',
                    family, weights)
  
  # Doesn't currently work but haven't really thought
  # about it if makes sense yet.
  # out7 <- agoutiGLM(formula = form, d,
  #                   ID = ID, inner_link = 'logit',
  #                   outer_link = 'exp',
  #                   family, weights)
  
  expect_true(class(out1) == 'agoutiGLM')
  expect_true(class(out2) == 'agoutiGLM')
  expect_true(class(out3) == 'agoutiGLM')
  expect_true(class(out4) == 'agoutiGLM')
  expect_true(class(out5) == 'agoutiGLM')
  expect_true(class(out6) == 'agoutiGLM')
  
  
  
})

test_that('agouti function works with 1 covariate', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form <- yy ~ X1
  
  
  out <- agoutiGLM(formula = form, d,
                ID = ID, inner,
                outer,
                family, weights)
  
  
  expect_true(class(out) == 'agoutiGLM')
  expect_true(length(out$coefficients) == 2)
  
  
  
})

test_that('agouti function works with interactions, squared covs and factors', {
  
  set.seed(1)
  N1 <- 50
  N2 <- 10
  ncovs <- 2
  
  beta <- c(0.1, -0.4)
  x <- matrix(rnorm(N1 * ncovs), ncol = ncovs)
  order <- sample(seq(N2), N1, replace = TRUE)
  ID <- letters[order]
  yy <- rnorm(N2)[order]
  weights <- runif(N1)
  
  d <- data.frame(x, yy, ID, weights)
  
  inner <- 'identity'
  outer <- 'identity'
  
  family <- gaussian()
  
  form1 <- yy ~ X1 + poly(X2, 2)
  
  
  out1 <- agoutiGLM(formula = form1, d,
                ID = ID, inner,
                outer,
                family, weights)
  
  
  expect_true(class(out1) == 'agoutiGLM')
  expect_true(length(out1$coefficients) == 4)
  
  
  
  
  form2 <- yy ~ X1 * X2
  
  
  out2 <- agoutiGLM(formula = form2, d,
                 ID = ID, inner,
                 outer,
                 family, weights)
  
  
  expect_true(class(out2) == 'agoutiGLM')
  expect_true(length(out2$coefficients) == 4)
  
  d2 <- d
  d2$X2 <- factor(cut(d2$X2, c(-10, -1, 1, 10)))

  form <- yy ~ X1 + X2
  
  out3 <- agoutiGLM(formula = form, d2,
                 ID = ID, inner,
                 outer,
                 family, weights)
  
  
  expect_true(class(out3) == 'agoutiGLM')
  expect_true(length(out3$coefficients) == 4)
  
})


  




