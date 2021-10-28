
#' A function that summarises the groups within an aggregate output dataset, 
#' by taking quantiles, and then plots these quantiles against the response data.
#' 
#' @export
#' @example
#' 
#' data(madagascar_malaria)
#' group_summary_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)


group_summary_plot <- function(formula, data, ID = ID, weights = weights){
    
    x <- as.character(formula[[3]])
    y <- as.character(formula[[2]])
    
    
    # If default weights value (weights) is used, and weights not in data,
    #   Add a column of 1s
    if(!('weights' %in% names(data))){
        data$weights <- 1
    }
    
    p1 <- 
    data %>% 
        group_by(ID) %>% 
        summarise(
            `0.05quant` = quantile(.data[[x]], 0.05),
            `0.25quant` = quantile(.data[[x]], 0.25),
            `0.5quant` = median(.data[[x]]),
            `0.75quant` = quantile(.data[[x]], 0.75),
            `0.95quant` = quantile(.data[[x]], 0.95),
            response = mean(.data[[y]])) %>% 
        ungroup %>% 
        select(-ID) %>%
        pivot_longer(-response) %>% 
        ggplot(aes(value, response)) + 
            facet_wrap(~ name, scale = 'free') +
            geom_point() + 
            geom_smooth()
    
    return(p1)   
}
