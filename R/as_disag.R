
#' as_dsiag Generic function
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
#' @param outcome The name of the variable containing the response data
#' @param ... any other arguments
#' @return \code{as_disag.data.frame} returns a dataframe that can be used with all agouti functions.
#' @method as_disag data.frame
#' @export
#' @examples
#' \dontrun{
#' disag_data <- as_disag(data=df_outcome, data2=df_predictor, outcome="Death")
#' }
#' data(madagascar_malaria)
#' disag_data <- as_disag(data=madagascar_malaria,outcome="case_rate")
as_disag.data.frame <- function (data,data2=NULL,outcome,...){

  if(is.null(data2)){
    outcomename <- outcome

    if(!(outcomename %in% names(data)))
      stop(outcomename, " variable not found in data, please check")
    if(!("ID" %in% names(data)))
      stop("ID varible not found in data, please check")


    if(any(is.na(data[,{{outcomename}}]))){
      check <- data %>%
        dplyr::group_by(.data$ID) %>%
        dplyr::mutate(unique_response = dplyr::n_distinct({{outcomename}}))
      if(any(check$unique_response >1))
        stop("Outcome variable contains more than one unique value for a given ID")
      data <- data %>%
        dplyr::group_by(.data$ID) %>%
        dplyr::mutate(outcome=sum(get(outcomename),na.rm=TRUE))
      print("NOTE: Outcome variable has been repeated for all rows with the same ID and stored in a new variable called outcome")
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
  df <- data_tidy %>% dplyr::group_by(.data$ID) %>%
    dplyr::mutate(unique_response = dplyr::n_distinct(outcome))

  if(any(df$unique_response > 1))
    stop("Different responses found within the same ID group")

  class(data_tidy) <- append("as_disag", class(data_tidy))
  return(data_tidy)

}

#' Create a data.frame suitable for disaggregation regression modelling from
#' a spatial polygons dataframe.
#' @importFrom parallel detectCores makeCluster stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom raster extract
#' @importFrom foreach foreach
#' @param data A spatial polygons data frame with a column named ID
#' @param rstack A raster stack of rasters to be summarised at ID level
#' @param response_df a dataframe containing the response data and an ID variable
#' @param ... any other arguments
#' @return \code{as_disag.SpatialPolygonsDataFrame} returns a dataframe that can be used with all agouti functions.
#' @method as_disag SpatialPolygonsDataFrame
#' @export
#' @examples
#' \dontrun{
#' disag_data <- as_disag(data=shapefile, rstack=rstack, response=wnv)
#' }
as_disag.SpatialPolygonsDataFrame <- function (data, rstack, response_df,...){

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
  parallelExtract <- function(raster, shape, fun = mean, id = 'OBJECTID',  ...){

    if (!requireNamespace("foreach", quietly = TRUE)) {
      stop("foreach needed for this function to work. Please install it.",
           call. = FALSE)
    }

    shape@data[, id] <- as.character(shape@data[, id])

    i <- NULL
    # Run extract in parallel.
    values <- foreach::foreach(i = seq_along(shape)) %dopar% {
      raster::extract(raster, shape[i, ], fun = fun, na.rm = TRUE, cellnumbers = TRUE, ...)
    }
    if(!is.null(fun)){
      # If a summary function was given, just bind everything together and add ID column
      df <- data.frame(do.call(rbind, values))
      if(inherits(shape, 'SpatialPolygonsDataFrame')){
        df <- cbind(ID = as.data.frame(shape)[, id], df)
      } else{
        df <- cbind(ID = names(shape), df)
        id <- 'id'
      }

      names(df) <- c(id, names(raster))

      return(df)
    } else {
      # If no summary was given we get a list of length n.shapes
      #   each entry in the list is a dataframe with n.covariates columns
      #   Want to make covariates columns, rbind shapes, and add shape and cell id columns.

      # list of vectors, one for each covariate
      values_id <- lapply(seq_along(values), function(x) data.frame(shape@data[, id][x], values[[x]][[1]]))


      df <- do.call(rbind, values_id)
      names(df) <- c(id, 'cellid', names(raster))

      return(df)
    }

  }
  result <- parallelExtract(rstack, data, id="ID",fun=NULL)
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
