

#' Summarise a aggregated output dataset.
#' 
#' 
#' @export
#' @examples
#' data(madagascar_malaria)
#' agouti_summary(madagascar_malaria)

agouti_summary <- function(x, ID = ID){

    ID_char <- deparse(substitute(ID))
    
    n_groups <- length(unique(x[, ID_char, drop = TRUE]))
    n_rows <- nrow(x)
    
    max_j <- max(table(x[, ID_char, drop = TRUE]))
    min_j <- min(table(x[, ID_char, drop = TRUE]))
    median_j <- median(table(x[, ID_char, drop = TRUE]))
    
    

    # Find columns at aggregate level and remove.
    unique_vals <- 
        x %>% 
            sapply(function(col) length(unique(col)))
    not_agg_level <- which(!(unique_vals == n_groups))
    
    group_summary_table <-
        x %>% 
          group_by({{ID}}) %>% 
            select(all_of(not_agg_level)) %>% 
            select_if(is.numeric) %>%
            summarise_all(list(median = median, min = min, max = max)) %>% 
            ungroup %>% 
            select(- {{ID}}) %>% 
            summarise_all(median) %>% 
            pivot_longer(everything()) %>% 
            mutate(type = ifelse(grepl('median', name), 'Median of medians', 'other')) %>% 
            mutate(type = ifelse(grepl('min', name), 'Median of mins', type)) %>% 
            mutate(type = ifelse(grepl('max', name), 'Median of maxs', type)) %>% 
            mutate(name = gsub('_median|_min|_max', '', name)) %>% 
            pivot_wider(- name)
        
    print(paste("Aggregate outputs table with ID column:", ID_char))
    print(paste(n_rows, 'total rows in', n_groups, 'groups.'))
    print(paste0('Min group size: ', min_j, '. Median group size: ', median_j, '. Max group size: ', max_j, '.'))
    print(group_summary_table)
    
    return(invisible(group_summary_table))
}
