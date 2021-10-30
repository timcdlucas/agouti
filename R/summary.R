

#' Summarise a aggregated output dataset.
#' 
#' @importFrom magrittr %>% 
#' @importFrom stats median quantile
#' @importFrom rlang .data
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
          dplyr::group_by({{ID}}) %>% 
            dplyr::select(dplyr::all_of(not_agg_level)) %>% 
            dplyr::select_if(is.numeric) %>%
            dplyr::summarise_all(list(median = median, min = min, max = max)) %>% 
            dplyr::ungroup %>% 
            dplyr::select(- {{ID}}) %>% 
            dplyr::summarise_all(median) %>% 
            tidyr::pivot_longer(dplyr::everything()) %>% 
            dplyr::mutate(type = ifelse(grepl('median', .data$name), 'Median of medians', 'other')) %>% 
            dplyr::mutate(type = ifelse(grepl('min', .data$name), 'Median of mins', .data$type)) %>% 
            dplyr::mutate(type = ifelse(grepl('max', .data$name), 'Median of maxs', .data$type)) %>% 
            dplyr::mutate(name = gsub('_median|_min|_max', '', .data$name)) %>% 
            tidyr::pivot_wider(- .data$name)
        
    print(paste("Aggregate outputs table with ID column:", ID_char))
    print(paste(n_rows, 'total rows in', n_groups, 'groups.'))
    print(paste0('Min group size: ', min_j, '. Median group size: ', median_j, '. Max group size: ', max_j, '.'))
    print(group_summary_table)
    
    return(invisible(group_summary_table))
}
