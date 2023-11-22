
#' as_disag Generic function
#'
#' @param data A dataframe containing at least an outcome variable and ID variable named ID
#' @param ... any other arguments
#' @export
as_disag <- function(data,...){
  UseMethod("as_disag")
}

#' Checks if a given dataframe is in the correct format for agouti functions
#'
#' @param data A dataframe containing a outcome variable and ID variable named ID
#' @param data2 A dataframe default is NULL in which case we check the data is already in the correct format. If not null,
#'  the second data frame should contain predictor variables at a different resolution to the outcome but with the same linking ID variable
#' @param response_var The name of the variable containing the response data
#' @param ... any other arguments
#' @return \code{as_disag.data.frame} returns a dataframe that can be used with all agouti functions.
#' @method as_disag data.frame
#' @export
#' @examples
#' \dontrun{
#' disag_data <- as_disag(data=df_outcome, data2=df_predictor, outcome="Death")
#' }
#' data(madagascar_malaria)
#' disag_data <- as_disag(data=madagascar_malaria,response_var="case_rate")
as_disag.data.frame <- function (data,data2=NULL,response_var,...){

  if(is.null(data2)){
    responsename <- response_var

    if(!(responsename %in% names(data)))
      stop(responsename, " variable not found in data, please check")
    if(!("ID" %in% names(data)))
      stop("ID varible not found in data, please check")


    if(any(is.na(data[,{{responsename}}]))){
      check <- data %>%
        dplyr::group_by(.data$ID) %>%
        dplyr::mutate(unique_response = dplyr::n_distinct({{responsename}}))
      if(any(check$unique_response >1))
        stop("Response variable contains more than one unique value for a given ID")
      data <- data %>%
        dplyr::group_by(.data$ID) %>%
        dplyr::mutate(response=sum(get(responsename),na.rm=TRUE))
      print(paste0("NOTE: ",response_var, " variable has been repeated for all rows with the same ID and stored in a new variable named response"))
    }

    data_tidy <- data

  }else{

  if(!("ID" %in% names(data)))
    stop("ID varible not found in outcome data, please check")
  if(!("ID" %in% names(data2)))
    stop("ID varible not found in predictor data, please check")

  if(!(all(unique(data$ID) %in% unique(data2$ID))))
    stop("Some IDs in outcome data are not in predictor data")

  ## count how many rows of data we have for each ID. want this to be general in case people have groups of different sizes due to missing data
  data_tidy <- dplyr::left_join(data2, data, by="ID")
  }

  ## check if every row with the same ID has the same response
  if("response" %in% names(data_tidy)){
  df <- data_tidy %>% dplyr::group_by(.data$ID) %>%
    dplyr::mutate(unique_response = dplyr::n_distinct(response))
  }else{
    df <- data_tidy %>% dplyr::group_by(.data$ID) %>%
      dplyr::mutate(unique_response = dplyr::n_distinct({{responsename}}))
  }

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")

  class(data_tidy) <- append("as_disag", class(data_tidy))
  return(data_tidy)

}

#' Create a data.frame suitable for disaggregation regression modelling from
#' a sf object.
#' @importFrom terra extract match
#' @param data An object of class sf containing polygons that covariates should be aggregated to and including the response variable and ID
#' @param rstack A raster stack of class SpatRaster to be summarised at ID level
#' @param response_var Name of the response variable found in data
#' @param ... any other arguments
#' @return \code{as_disag.sf} returns a dataframe that can be used with all agouti functions.
#' @method as_disag sf
#' @export
#' @examples
#' data("NYleukemia")
#' polygons <- sf::st_as_sf(NYleukemia$spatial.polygon)
#' df <- cbind(polygons, NYleukemia$data)
#' names(df)[1] <-"ID"
#' covariate <- terra::rast("vignettes/annual_mean_temp_newyork.tif")
#' disag_data <- as_disag(data=df, rstack=covariate, response_var="cases")
#'
as_disag.sf <- function (data, rstack, response_var="response",...){

  ## first need some checks on each of the objects
  if(!("ID" %in% names(data)))
    stop("ID varible not found in data, please check")
  if(!(response_var %in% names(data)))
    stop("ID varible not found in response data, please check")

  df_summ <- prepare_data(polygon_shapefile =data, covariate_rasters = rstack, id_var="ID",
                        response_var=response_var,na.action=TRUE)


  class(df_summ) <- append("as_disag", class(df_summ))
  return(df_summ)


}

#' Function to Extract polygon id and response data into a data.frame from a sf object
#'  Taken from disaggregation package
#' Returns a data.frame with a row for each polygon in the sf object and columns: area_id, response and N, containing the id of the
#' polygon, the values of the response for that polygon, and the sample size respectively. If the data is not survey data (the sample size does
#' not exist), this column will contain NAs.
#'
#' @param shape A sf object containing response data.
#' @param id_var Name of column in shape object with the polygon id. Default 'area_id'.
#' @param response_var Name of column in shape object with the response data. Default 'response'.
#' @param sample_size_var For survey data, name of column in sf object (if it exists) with the sample size data. Default NULL.
#'
#' @return A data.frame with a row for each polygon in the sf object and columns: area_id, response and N, containing the id of the
#' polygon, the values of the response for that polygon, and the sample size respectively. If the data is not survey data (the sample size does
#' not exist), this column will contain NAs.
#'
#'
#'
getPolygonData <- function(shape, id_var = 'area_id', response_var = 'response', sample_size_var = NULL) {

  if(is.null(sample_size_var)) {
    polygon_df <- shape[, c(id_var, response_var), drop = TRUE]
    polygon_df$N <- rep(NA, nrow(polygon_df))
  } else {
    polygon_df <- shape[, c(id_var, response_var, sample_size_var), drop = TRUE]
  }

  names(polygon_df) <- c('area_id', 'response', 'N')

  return(polygon_df)
}

#' Function to extract values from high-resolution covariates and join to low-resolution
#' polygon data
#' This function is mostly taken from the disaggregation package
#'
#' @param polygon_shapefile sf object containing at least three columns: one with the geometried, one with the id for the polygons (\emph{id_var}) and one with the response data (\emph{response_var}); for binomial data, i.e survey data, it can also contain a sample size column (\emph{sample_size_var}).
#' @param covariate_rasters SpatRaster of covariate rasters to be used in the model.
#' @param aggregation_raster SpatRaster to aggregate pixel level predictions to polygon level e.g. population to aggregate prevalence. If this is not supplied a uniform raster will be used.
#' @param id_var Name of column in sf object with the polygon id.
#' @param response_var Name of column in sf object with the response data.
#' @param sample_size_var For survey data, name of column in sf object (if it exists) with the sample size data.
#' @param na.action logical. If TRUE, NAs in response will be removed, covariate NAs will be given the median value, aggregation NAs will be set to zero. Default FALSE (NAs in response or covariate data within the polygons will give errors).
#' @return Returns a dataframe containing the values of covariates for each pixel with the associated polygon ID

prepare_data <- function(polygon_shapefile,
                         covariate_rasters,
                         aggregation_raster = NULL,
                         id_var = 'area_id',
                         response_var = 'response',
                         sample_size_var = NULL,
                         na.action = FALSE) {

  stopifnot(inherits(polygon_shapefile, 'sf'))
  stopifnot(inherits(covariate_rasters, 'SpatRaster'))
  if(!is.null(aggregation_raster)) stopifnot(inherits(aggregation_raster, 'SpatRaster'))
  stopifnot(inherits(id_var, 'character'))
  stopifnot(inherits(response_var, 'character'))

  # Check for NAs in response data
  na_rows <- is.na(polygon_shapefile[, response_var, drop = TRUE])
  if(sum(na_rows) != 0) {
    if(na.action) {
      polygon_shapefile <- polygon_shapefile[!na_rows, ]
    } else {
      stop('There are NAs in the response data. Please deal with these, or set na.action = TRUE')
    }
  }


  polygon_data <- getPolygonData(polygon_shapefile, id_var, response_var, sample_size_var)

  # Save raster layer names so we can reassign it to make sure names don't change.
  cov_names <- names(covariate_rasters)

  # If no aggregation raster is given, use a 'unity' raster
  if(is.null(aggregation_raster)) {
    aggregation_raster <- covariate_rasters[[1]]
    terra::values(aggregation_raster) <- rep(1, terra::ncell(aggregation_raster))
  }
  names(aggregation_raster) <- 'aggregation_raster'


  covariate_rasters <- c(covariate_rasters, aggregation_raster)
  covariate_data <- terra::extract(covariate_rasters, terra::vect(polygon_shapefile), cells=TRUE, na.rm=TRUE, ID=TRUE)
  #merge to transfer area_id and then tidy up
  polygon_data$area_n <- 1:nrow(polygon_data)
  covariate_data <- merge(covariate_data, polygon_data, by.x = "ID", by.y = "area_n")
  covariate_data <- covariate_data[ , !(names(covariate_data) %in% c("ID", "response", "N"))]
  colnames(covariate_data )[colnames(covariate_data ) == "area_id"] <- id_var
  polygon_data <- polygon_data[ , !(names(polygon_data) %in% c("area_n"))]

  # Remove the aggregation raster
  cov_filter <- !(names(covariate_data) %in% c('aggregation_raster'))
  covariate_rasters <- covariate_rasters[[cov_filter]]
  names(covariate_rasters) <- cov_names

  agg_filter <- names(covariate_data) %in% c('aggregation_raster')
  aggregation_pixels <- as.numeric(covariate_data[ , agg_filter])
  covariate_data <- covariate_data[, !agg_filter]

  # Check for NAs in population data
  if(sum(is.na(aggregation_pixels)) != 0) {
    if(na.action) {
      aggregation_pixels[is.na(aggregation_pixels)] <- 0
    } else {
      stop('There are NAs in the aggregation rasters within polygons. Please deal with these, or set na.action = TRUE')
    }
  }

  # Check for NAs in covariate data
  if(sum(is.na(covariate_data)) != 0) {
    if(na.action) {
      cov_filter <- !(names(covariate_data) %in% c(id_var,'cell'))
      covariate_data[ , cov_filter] <- sapply(covariate_data[ , cov_filter], function(x) { x[is.na(x)] <- stats::median(x, na.rm = T); return(x) })
    } else {
      stop('There are NAs in the covariate rasters within polygons. Please deal with these, or set na.action = TRUE')
    }
  }

  disag_data <- covariate_data
  disag_data <- disag_data %>% dplyr::left_join(polygon_shapefile, by = "ID")

  return(disag_data)

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
#' @param ... any other arguments
#' @return \code{as_disag.ts} returns a dataframe that can be used with all agouti functions.
#' @method as_disag ts
#' @export
#' @examples
#' data(stock_vector)
#' disag_data <- as_disag(data=stock_vector)


as_disag.ts <- function(data,lags=10, ID="add",...){


  if(!(stats::is.ts(data) | stats::is.mts(data)))
     stop("Data object is not of class time-series")

  data_ts <- as.data.frame(as.numeric(data))
  names(data_ts)[1] <- "outcome"

  ## Adding and checking the ID col
  if(any(ID == "add")){
    data_ts <- cbind(data_ts,ID=1:nrow(data_ts))
    print("adding ID col")
  }else {
    data_ts <- cbind(data, ID)
    if(nrow(data_ts) != length(unique(data_ts$ID)))
      stop("Error: There is not a unique ID for each observation, check your ID column")
  }

  # Function for calculating lags
  calculate_lags <- function(df, var, lags){
    map_lag <- lags %>% purrr::map(~purrr::partial(dplyr::lag, n = .x))
    return(df %>% dplyr::mutate(dplyr::across(.cols = {{var}}, .fns = map_lag, .names = "lag{lags}")))
  }

  # Create lags - this is in wide format
  data_lags <- data_ts %>% calculate_lags("outcome", 1:lags)
  #data_lags <- cbind(data_lags, data_ts)
  # Grab the names of the lag columns so we can go from wide to long
  a <- names(data_lags)[grepl("lag",colnames(data_lags))]
  # Wide to long format
  data_long  <- data_lags %>% tidyr::gather(lag, "covariate", tidyselect::all_of(a), factor_key=TRUE)

  class(data_long) <- append("as_disag", class(data_long))
  return(data_long)

}
