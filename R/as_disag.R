
# can't remember how to define methods

as.disag <- function(data, ...){

}


as.disag.data.frame <- function (data, ...){

}


as.disag.SpatialPolygonsDataFrame <- function (data, ...){


}



as.disag.TimeSeries <- function(data, ...){

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
#' @param data A time series data object
#' @param lags An integer list given which lags should be used.
#' @return Nothing at the moment
#' @examples
#' disag_data <- disag_from_temporal_dataframe()

disag_from_temporal_dataframe <- function(data, lags){


}
