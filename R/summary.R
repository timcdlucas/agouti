
#' Summarise a aggregated output dataset.
#'
#' @importFrom magrittr %>%
#' @importFrom stats median quantile
#' @importFrom rlang .data
#' @importFrom tidyr drop_na
#' @param x A data frame suitable for use as aggregate output data.
#' @param ID The column to use as the group ID column. Unquoted value.
#' @param high_res A string or vector of strings specifying a column or columns of high resolution variables to summarise.
#' default is NULL in which case the function will summarise all variables with more unique values than ID
#' @param na.rm default is false, if na.rm is not equal to false, NA's will be dropped
#' @export
#' @examples
#' data(madagascar_malaria)
#' agouti_summary(madagascar_malaria)
#' agouti_summary(madagascar_malaria, high_res=c("pop","EVI"))

agouti_summary <- function(x, ID = ID, high_res=NA, na.rm = FALSE){

  if(!(inherits(x, "as_disag")))
    warning("Using data not of class as_disag, we advise first using the
            function as_disag() to check and format your data")

  if(na.rm!=FALSE){
    x <- x %>% tidyr::drop_na()
  }

  ID_char <- deparse(substitute(ID))
  if(!(ID_char %in% names(x)))
    stop(paste0("Data does not contain the variable called ", ID_char))

  n_groups <- length(unique(x[, ID_char, drop = TRUE]))
  n_rows <- nrow(x)

  max_j <- max(table(x[, ID_char, drop = TRUE]))
  min_j <- min(table(x[, ID_char, drop = TRUE]))
  median_j <- median(table(x[, ID_char, drop = TRUE]))

  if(any(is.na(high_res))){
    # find cols at aggregate level and remove
    unique_vals <-
      x %>%
      sapply(function(col) length(unique(col)))
    not_agg_level <- which(!(unique_vals <= n_groups))
    if(length(not_agg_level)==0)
      stop("Not clear which variables are high resolution, please specify with the argument high_res")

  }else {

    if(!all(high_res %in% names(x)))
      stop("High resolution variables are not in data, please check spelling")

    not_agg_level <- as.numeric(which(names(x) %in% high_res))
  }

  number_vars <- x %>% dplyr::group_by({{ID}}) %>% dplyr::select(dplyr::any_of(not_agg_level)) %>% dplyr::select_if(is.numeric)
  # table doesn't make a single nice column when there is just one variable so addedd the below to handle this case
  if(ncol(number_vars) == 2 | length(high_res) == 1){

    col_name <- names(number_vars)[names(number_vars) != "ID"]
    group_summary_table <-
      number_vars %>%
      dplyr::group_by({{ID}}) %>%
      dplyr::select(dplyr::any_of(col_name)) %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::summarise(dplyr::across(c(dplyr::all_of(col_name)), list(median=median,min=min,max=max))) %>%
      dplyr::ungroup() %>%
      dplyr::select(- {{ID}}) %>%
      dplyr::summarise_all(median) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::mutate(type = ifelse(grepl('median', .data$name), 'Median of medians', 'other')) %>%
      dplyr::mutate(type = ifelse(grepl('min', .data$name), 'Median of mins', .data$type)) %>%
      dplyr::mutate(type = ifelse(grepl('max', .data$name), 'Median of maxs', .data$type)) %>%
      dplyr::mutate(name = gsub('_median|_min|_max', '', .data$name)) %>%
      tidyr::pivot_wider()
  }else{


    group_summary_table <-
      x %>%
      dplyr::group_by(ID) %>%
      dplyr::select(dplyr::any_of(not_agg_level)) %>%
      dplyr::select_if(is.numeric) %>%
      dplyr::summarise_all(list(median = median, min = min, max = max)) %>%
      dplyr::ungroup() %>%
      dplyr::select(- {{ID}}) %>%
      dplyr::summarise_all(median) %>%
      tidyr::pivot_longer(dplyr::everything()) %>%
      dplyr::mutate(type = ifelse(grepl('median', .data$name), 'Median of medians', 'other')) %>%
      dplyr::mutate(type = ifelse(grepl('min', .data$name), 'Median of mins', .data$type)) %>%
      dplyr::mutate(type = ifelse(grepl('max', .data$name), 'Median of maxs', .data$type)) %>%
      dplyr::mutate(name = gsub('_median|_min|_max', '', .data$name)) %>%
      tidyr::pivot_wider()
  }

  print(paste("Aggregate outputs table with ID column:", ID_char))
  print(paste(n_rows, 'total rows in', n_groups, 'groups.'))
  print(paste0('Min group size: ', min_j, '. Median group size: ', median_j, '. Max group size: ', max_j, '.'))

  print(group_summary_table)

  return(invisible(group_summary_table))
}
