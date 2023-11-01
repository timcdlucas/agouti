
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

#' Checks if a given dataframe is in the correct format for agouti functions
#'
#' @export
#' @param data A dataframe containing a response variable, ID variable and any predictors
#' @param response The name of the variable containing the response data
#' @param single_df default is equal TRUE, if more than one dataframe single_df=FALSE
#' @return \code{as_disag.default} returns a dataframe that can be used with all agouti functions.
#' @method as_disag default
#' @export
#' @examples
#' disag_data <- as_disag(data=madagascar_malaria, response="case_rate")


as_disag.default <- function(data,response="response", single_df=TRUE){

  responsename <- response

  if(!(responsename %in% names(data)))
   stop(responsename, " variable not found in data, please check")


  if(!("ID" %in% names(data)))
    stop("ID varible not found in data, please check")

  ## check if every row with the same ID has the same response
  df <- data %>% dplyr::group_by(ID) %>%
    dplyr::mutate(unique_response = dplyr::n_distinct(responsename))

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")


  print("Congrats, your data is in as_disag format")
  class(data) <- append("as_disag", class(data))
  return(data)

}

#' Checks if a given dataframe is in the correct format for agouti functions
#'
#' @param data A dataframe containing a outcome variable and ID variable named ID
#' @param data2 A dataframe containing predictor variables at a different resolution to the outcome but with the same linking ID variable
#' @param outcome The name of the variable containing the response data
#' @param single_df default is equal to FALSE
#' @return \code{as_disag.data.frame} returns a dataframe that can be used with all agouti functions.
#' @method as_disag data.frame
#' @export
#' @examples
#' disag_data <- as_disag(data=temp_outcome, data2=temp_predictor, outcome="Death")
as_disag.data.frame <- function (data,data2,outcome,single_df=FALSE){

  if(!("ID" %in% names(data)))
    stop("ID varible not found in outcome data, please check")
  if(!("ID" %in% names(data2)))
    stop("ID varible not found in predictor data, please check")

  if(!(all(unique(data$ID) %in% unique(data2$ID))))
    stop("Some IDs in outcome data are not in predictor data")

  ## count how many rows of data we have for each ID. want this to be general in case people have groups of different sizes due to missing data
  data_summ <- dplyr::left_join(data2, data, by="ID")

  ## check if every row with the same ID has the same response
  df <- data_summ %>% dplyr::group_by(ID) %>%
    dplyr::mutate(unique_response = dplyr::n_distinct(outcome))

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")

  class(data_summ) <- append("as_disag", class(data_summ))
  return(data_summ)

}

#' Create a data.frame suitable for disaggregation regression modelling from
#' a spatial polygons dataframe.
#' @importFrom disaggregation parallelExtract
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @param data A spatial polygons data frame with a column named ID
#' @param rstack A raster stack of rasters to be summarised at ID level
#' @param response_df a dataframe containing the response data and an ID variable
#' @return \code{as_disag.SpatialPolygonsDataFrame} returns a dataframe that can be used with all agouti functions.
#' @method as_disag SpatialPolygonsDataFrame
#' @export
#' @examples
#' disag_data <- as_disag(data=shapefile, rstack=rstack, response=wnv)

as_disag.SpatialPolygonsDataFrame <- function (data, rstack, response_df){

  ## first need some checks on each of the objects
  if(!("ID" %in% names(data@data)))
    stop("ID varible not found in data, please check")
  if(!("ID" %in% names(response_df)))
    stop("ID varible not found in response data, please check")

  if(!(all(unique(response_df$ID) %in% unique(data@data$ID))))
    stop("Some IDs in response data are not in spatial polygons dataframe")

  ## use the extract function from disagreggation package to extract data from raster stack at polygon level
  nCores <- parallel::detectCores()-1
  cl <- parallel::makeCluster(nCores)
  doParallel::registerDoParallel(cl)
  print("Extracting data from raster stack, this may take a while...")
  result <- disaggregation::parallelExtract(rstack, data, id="ID",fun=NULL)
  parallel::stopCluster(cl)

  response_df <- dplyr::left_join(response_df, result, by = "ID")
  class(response_df) <- append("as_disag", class(response_df))
  return(response_df)


}


#' Create a data.frame suitable for disaggregation regression modelling from
#' a time series object.
#'
#' An example time series object is daily stock price growth
#'
#' @section More stuff
#' @importFrom stats is.ts is.mts lag
#' @param data A vector of class time series representing a response e.g. in stock data the growth
#' @param lags An integer saying how many lags should be taken
#' @param ID Either the name of the grouping variable or "add" which means an ID variable will be added 1:nrow()
#' @return \code{as_disag.ts} returns a dataframe that can be used with all agouti functions.
#' @method as_disag ts
#' @export
#' @examples
#' disag_data <- as_disag(data=stock_vector)

as_disag.ts <- function(data,lags=10, ID="add"){


  if(!(stats::is.ts(data) | stats::is.mts(data)))
     stop("Data object is not of class time-series")

  data <- as.vector(data)
  data <- as.data.frame(data)
  data <- data %>% dplyr::rename(response=data)

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
    map_lag <- lags %>% purr::map(~partial(lag, n = .x))
    return(df %>% dplyr::mutate(dplyr::across(.cols = {{var}}, .fns = map_lag, .names = "lag{lags}")))
  }

  # Create lags - this is in wide format
  data_lags <- data %>% calculate_lags(response, 1:lags)
  # Grab the names of the lag columns so we can go from wide to long
  a <- names(data_lags)[grepl("lag",colnames(data_lags))]
  # Wide to long format
  data_long  <- data_lags %>% tidyr::gather(lag, lagged_growth, tidyselect::all_of(a), factor_key=TRUE)

  class(data_long) <- append("as_disag", class(data_long))
  return(data_long)

}


#' Create a data.frame suitable for disaggregation regression modelling from
#' a temporal dataframe object.
#'
#' A series of lagged variables can be considered similar to disaggregation
#' regression if we think that the order of the lagged variables is unimportant.
#' For example, if we have millisecond resolution airpollution data relating to
#' daily health outcomes, it is unimportant whether an airpollution measurement
#' occured 2 miliseconds or 4 miliseconds before the health data cutoff time.
#'
#' @section More stuff
#' @importFrom lubridate is.POSIXt is.Date
#' @param data a column of a dataframe that is in datetime format
#' @param time_group what is the format of the datetime variable at the level of the response in date-time format
#' @param ID Either the name of the grouping variable or "add" which means an ID variable will be added 1:nrow()
#' @param response_df a dataframe containing at least data,ID and outcome
#' @param outcome A character string naming the variable which stores the outcome data
#' @return \code{as_disag.POSIXt} returns a dataframe that can be used with all agouti functions
#' @method as_disag POSIXt
#' @export
#' @examples
#' data(mortality_temporal)
#' disag_data <- as_disag.POSIXt(data=mortality_temporal$Datetime, response=mortality_temporal, outcome="Death")

as_disag.POSIXt <- function(data = data$Datetime, time_group="%Y%m%d", ID="add",response_df, outcome="Death"){

  if(!(lubridate::is.POSIXt(data) | lubridate::is.Date(data)))
    stop("Data object is not of class date-time")

  if(ID=="add"){
    data2 <- response_df %>%
      dplyr::mutate(ID=format(Datetime, time_group)) %>%
      dplyr::group_by(ID) %>%
      dplyr::mutate(outcome=sum(get(outcome),na.rm=TRUE))

  }

  ## check if every row with the same ID has the same response
  df <- data2 %>% dplyr::group_by(ID) %>%
    dplyr::mutate(unique_response = n_distinct(outcome))

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")

  class(data2) <- append("as_disag", class(data2))
  return(data2)

}
