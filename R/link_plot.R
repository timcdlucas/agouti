
#' A very basic plotting for aggregate data that plots a linked scatter plot.
#' 
#' This is a scatter plot which data from the aggregate unit "linked" by 
#' a line and colour coded to match.
#' 
#' @export
#' @example
#' data(madagascar_malaria)
#' link_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)

# Need to add a default for weights
link_plot <- function(formula, data, ID = ID, weights = weights){
    
    x <- as.character(formula[[3]])
    y <- as.character(formula[[2]])
    
    p <- 
        data %>% 
            ggplot2::ggplot(aes(y = .data[[y]], x = .data[[x]], 
                                colour = as.numeric({{ ID }}), 
                                group = factor({{ ID }}), alpha = {{ weights }})) +
                ggplot2::geom_point() +
                ggplot2::geom_path() + 
                ggplot2::scale_colour_viridis_c()
    
    return(p)
}





