
#' A function that summarises the groups within an aggregate output dataset, 
#' by taking quantiles, and then plots these quantiles against the response data.
#' 
#' @export
#' @examples
#' @param formula A formula for the y and x variables.
#' @param data A data frame suitable for aggregate outputs models.
#' @param ID The column to use as the group ID column. Unquoted value.
#' @param weights Which column to use as within group weights such as 
#'   population or time.
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
        dplyr::group_by(ID) %>% 
        dplyr::summarise(
            `0.05quant` = quantile(.data[[x]], 0.05),
            `0.25quant` = quantile(.data[[x]], 0.25),
            `0.5quant` = median(.data[[x]]),
            `0.75quant` = quantile(.data[[x]], 0.75),
            `0.95quant` = quantile(.data[[x]], 0.95),
            response = mean(.data[[y]])) %>% 
        dplyr::ungroup %>% 
        dplyr::select(-.data$ID) %>%
        tidyr::pivot_longer(-.data$response) %>% 
        ggplot2::ggplot(ggplot2::aes(.data$value, .data$response)) + 
            ggplot2::facet_wrap(~ .data$name, scale = 'free') +
            ggplot2::geom_point() + 
            ggplot2::geom_smooth()
    
    return(p1)   
}
