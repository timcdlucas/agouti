


agouti <- function(formula, data, 
                   ID, inner_link, 
                   outer_link,
                   family, weights, ...){
    
    
    if(family$family == 'gaussian'){
        densf <- dnorm
    } else if(family$family == 'poisson'){
        densf <- dpois
    } else if(family$family == 'binomial'){
        densf <- dbinom
    }
    

    
}


nll <- function(beta, x, y, xID, yID, inner, 
                outer, family, weights){
    
    linear_predictor <- t(beta %*% x)
    
    summand <- inner(linear_predictor) * weights
    
    d <- data.frame(xID, summand)
    dsum <-
        d %>% 
        group_by(xID) %>%
        summarise(summed = sum(summand),
                  yhat = outer(summed))
    
    ydf <- data.frame(y, yID)
    
    dsum <- 
        dsum %>%
            left_join(ydf, by = c('xID' = 'yID')) %>%
            mutate(nll = family(yhat, y, theta))

  return(dsum}
}


objective <- function(beta, x, y, xID, yID, inner, outer, family, weights){

   dsum <- nll(beta, x, y, xID, yID, inner, outer, family, weights)

  nll <- sum(dsum$nll)
 return(nll)
}


  



