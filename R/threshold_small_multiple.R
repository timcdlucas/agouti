
#' A plot that summarised the relationship between a variable and an aggregated
#' output that depends on it.
#' 
#' The plot is created by calculating, within each group, the proportion of 
#' observations that are above a threshold. A scatter plot with a linear model
#' is plotted of the relationship between this proportion and the aggregated output.
#' The threshold is varied and small multiples of the different values are plotted. 
#' 
#' 
#' @export
#' @example
#' data(madagascar_malaria)
#' thresh_sm(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)


thresh_sm <- function(formula, data, ID = ID, weights = weights, 
                      small_mult = 25,
                      lower_percentile = 0.05,
                      upper_percentile = 0.95){
    
    x <- as.character(formula[[3]])
    y <- as.character(formula[[2]])
        

    # If default weights value (weights) is used, and weights not in data,
    #   Add a column of 1s
    if(!('weights' %in% names(data))){
        data$weights <- 1
    }
    
    thresholds_sm <- quantile(data[, x, drop = TRUE], 
                              seq(lower_percentile, upper_percentile, length.out = small_mult))
    
    p1 <- 
    data %>% 
        group_by({{ID}}) %>% 
        summarise(prop = sapply(thresholds_sm, 
                                \(t) weighted.mean(.data[[x]] > t, w = {{weights}})),
                  threshold = round(thresholds_sm, 3),
                  response = mean(.data[[y]])) %>% 
        ggplot(aes(x = prop, y = response, colour = threshold)) + 
        geom_point() +
        geom_smooth(method = 'lm', colour = 'black') +
        facet_wrap(~ threshold)
    
    
    return(p1)   
}
