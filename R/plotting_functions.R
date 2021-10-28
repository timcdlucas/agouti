
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




#' A grouped histogram plot.
#' 
#' 
#' @export
#' @examples
#' data(madagascar_malaria)
#' heat_hist(madagascar_malaria$EVI, madagascar_malaria$ID)

heat_hist <- function(x, ID, weights = 1, breaks = 50){
    
    ID <- factor(ID)
    ID <- as.numeric(fct_reorder(ID, x))
    
    d <- data.frame(x, ID = ID, weights = weights)
    
    p1 <- 
    ggplot(d, aes(x = x, y = ID, weights = weights)) +
        geom_bin_2d(
            binwidth = c(diff(range(x)) / breaks, 1)
        ) +
        scale_fill_viridis_c() + 
        theme(panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),
              axis.text.y=element_blank()
              )
        
    return(p1)
    
}

