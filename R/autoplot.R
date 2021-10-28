


link_plot <- function(data, ID = 'ID'){
    
    madagascar_malaria %>% 
        ggplot(aes(y = case_rate, x = LSTmean, colour = as.numeric(ID), 
                   group = factor(ID), alpha = pop)) +
        geom_point() +
        geom_path() + 
        scale_colour_viridis_c()
    
}



