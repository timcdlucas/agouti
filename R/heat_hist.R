
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