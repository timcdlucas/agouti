
# can't remember how to define methods

## do i want data to be an object that contains at a minimum response and
## ID. - yes? response could be stock growth, number of malaria cases etc.
## this could also contain predictors but doesn't have to.
## output is a dataframe of class as_disag which can be used with all
## all other functions in this package?

# Do we also want to force them to provide an ID col?
as_disag <- function(data,...){
  UseMethod("as_disag")
}

# the default method for this generic function is just to check if that data is already in the correct format
## examples are the madagascar_malaria and stock_data datasets these are already in the correct format
# requires tidyverse
as_disag.default <- function(data,response="response",ID=ID){

  responsename <- response

  if(!(responsename %in% names(data)))
   stop(responsename, " variable not found in data, please check")


  if(!("ID" %in% names(data)))
    stop("ID varible not found in data, please check")

  ## check if every row with the same ID has the same response
  df <- data %>% group_by(ID) %>%
    mutate(unique_response = n_distinct(responsename))

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")


  print("Congrats, your data is in as_disag format")
  return(data)

}


as.disag.data.frame <- function (data, ...){

}

#' Create a data.frame suitable for disaggregation regression modelling from
#' a spatial polygons dataframe.
#' rstack,sp_df and data must contain an ID col to join between.

as.disag.SpatialPolygonsDataFrame <- function (data, rstack, sp_df, ...){

  ## first need some checks on each of the objects

  ## use the extract function from disagreggation package to extra data from raster stack at polygon level
  cl <- parallel::makeCluster(2)
  doParallel::registerDoParallel(cl)
  print("Extracting data from raster stack, this may take a while..")
  result <- disaggregation::parallelExtract(world_sub, shape, id="fipscode",fun=NULL)
  parallel::stopCluster(cl)

  data <- left_join(data, result, by = "ID")
  return(data)

}


#' Create a data.frame suitable for disaggregation regression modelling from
#' a time series object.
#'
#' An example time series object is daily stock price growth
#'
#' @section More stuff
#'
#' @export
#' @param data A vector of class time series representing a response e.g. in stock data the growth
#' @param lags An integer saying how many lags should be taken
#' @param ID Either the name of the grouping variable or "add" which means an ID variable will be added 1:nrow()
#' @return Nothing at the moment
#' @method as_disag class
#' @examples
#' disag_data <- as_disag.timeseries(stocks, lags=10, ID="add")

as_disag.timeseries <- function(data,lags=10, ID="add"){


  if(!(is.ts(data) | is.mts(data)))
     stop("Data object is not of class time-series")

  ## Just some formatting, probs a better way of doing this, basically force it to be a ts obj for no reason
  data <- as.vector(data2)
  data <- as.data.frame(data)
  data <- data %>% rename(response=data)

  ## Adding and checking the ID col
  if(any(ID == "add")){
    data <- cbind(data,ID=1:length(data))
    print("adding ID col")
  }else {
    data <- cbind(data, ID)
    if(nrow(data) != length(unique(data$ID)))
      stop("Error: There is not a unique ID for each observation, check your ID column")
  }

  # Function for calculating lags
  calculate_lags <- function(df, var, lags){
    map_lag <- lags %>% map(~partial(lag, n = .x))
    return(df %>% mutate(across(.cols = {{var}}, .fns = map_lag, .names = "lag{lags}")))
  }

  # Create lags - this is in wide format
  data_lags <- data %>% calculate_lags(response, 1:lags)
  # Grab the names of the lag columns so we can go from wide to long
  a <- names(data_lags)[grepl("lag",colnames(data_lags))]
  # Wide to long format
  data_long  <- data_lags %>% gather(lag, lagged_growth, all_of(a), factor_key=TRUE)

  return(data_long)

}


#' Create a data.frame suitable for disaggregation regression modelling from
#' a time series object.
#'
#' A series of lagged variables can be considered similar to disaggregation
#' regression if we think that the order of the lagged variables is unimportant.
#' For example, if we have millisecond resolution airpollution data relating to
#' daily health outcomes, it is unimportant whether an airpollution measurement
#' occured 2 miliseconds or 4 miliseconds before the health data cutoff time.
#'
#' @section More stuff
#'
#' @export
#' @param data A time series data vector representing response e.g. in stock data the growth
#' @param lags An integer list given which lags should be used.
#' @param ID Either the name of the grouping variable or "add" which means an ID variable will be added 1:nrow()
#' @return Nothing at the moment
#' @examples
#' disag_data <- disag_from_temporal_dataframe(stocks, lags=10, ID="add")

disag_from_temporal_dataframe <- function(data, lags, ID="add"){


}
