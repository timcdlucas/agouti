
#' as_disag Generic function
#'
#' @param data A dataframe containing at least an outcome variable and ID variable named ID
#' @param ... any other arguments
#' @method
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
#' \dontrun{
#' disag_data <- as_disag(data=shapefile, rstack=rstack, response_var=Human.cases)
#' }
as_disag.sf <- function (data, rstack, response_var="response",...){

  ## first need some checks on each of the objects
  if(!("ID" %in% names(data)))
    stop("ID varible not found in data, please check")
  if(!(response_var %in% names(data)))
    stop("ID varible not found in response data, please check")

  prepare_data <- function(polygon_shapefile,
                           covariate_rasters,
                           aggregation_raster = NULL,
                           id_var = 'area_id',
                           response_var = 'response',
                           sample_size_var = NULL,
                           # mesh.args = NULL,
                           na.action = FALSE) {
    # makeMesh = TRUE,
    #ncores = NULL) {

    # if (!missing("ncores"))
    #   warning("The ncores argument has been deprecated")

    stopifnot(inherits(polygon_shapefile, 'sf'))
    stopifnot(inherits(covariate_rasters, 'SpatRaster'))
    if(!is.null(aggregation_raster)) stopifnot(inherits(aggregation_raster, 'SpatRaster'))
    stopifnot(inherits(id_var, 'character'))
    stopifnot(inherits(response_var, 'character'))
    # if(!is.null(mesh.args)) stopifnot(inherits(mesh.args, 'list'))

    # Check for NAs in response data
    na_rows <- is.na(polygon_shapefile[, response_var, drop = TRUE])
    if(sum(na_rows) != 0) {
      if(na.action) {
        polygon_shapefile <- polygon_shapefile[!na_rows, ]
      } else {
        stop('There are NAs in the response data. Please deal with these, or set na.action = TRUE')
      }
    }
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
    # extractCoordsForMesh <- function(cov_rasters, selectIds = NULL) {
    #
    #   stopifnot(inherits(cov_rasters, 'SpatRaster'))
    #   if(!is.null(selectIds)) stopifnot(inherits(selectIds, 'numeric'))
    #
    #   points_raster <- cov_rasters[[1]]
    #   points_raster[is.na(terra::values(points_raster, mat = FALSE))] <- -9999
    #   raster_pts <- terra::as.points(points_raster)
    #   coords <- terra::crds(raster_pts)
    #
    #   # If specified, only retain certain pixel ids
    #   if(!is.null(selectIds)) {
    #     coords <- coords[selectIds, ]
    #   }
    #
    #   return(coords)
    #
    # }
    # coordsForFit <- extractCoordsForMesh(covariate_rasters, selectIds = covariate_data$cell)
    #
    # coordsForPrediction <- extractCoordsForMesh(covariate_rasters)
    getStartendindex <- function(covariates, polygon_data, id_var = 'area_id') {

      stopifnot(ncol(polygon_data) == 3)
      stopifnot(ncol(covariates) >= 2)
      stopifnot(nrow(covariates) > nrow(polygon_data))
      stopifnot(sum(polygon_data$area_id %in% covariates[, id_var]) == nrow(polygon_data))

      # Create  startendindex matrix
      # This defines which pixels in the matrix are associated with which polygon.
      startendindex <- lapply(unique(covariates[, id_var]), function(x) range(which(covariates[, id_var] == x)))

      startendindex <- do.call(rbind, startendindex)

      whichindices <- terra::match(polygon_data$area_id, unique(covariates[, id_var]))

      # c++ is zero indexed.
      startendindex <- startendindex[whichindices, ] - 1L

      return(startendindex)
    }
    startendindex <- getStartendindex(covariate_data, polygon_data, id_var = id_var)

    # if(makeMesh) {
    #   mesh <- build_mesh(polygon_shapefile, mesh.args)
    # } else {
    #   mesh <- NULL
    #   message("A mesh is not being built. You will not be able to run a spatial model without a mesh.")
    # }

    disag_data <- list(polygon_shapefile = polygon_shapefile,
                       shapefile_names = list(id_var = id_var, response_var = response_var),
                       covariate_rasters = covariate_rasters,
                       polygon_data = polygon_data,
                       covariate_data = covariate_data,
                       aggregation_pixels = aggregation_pixels,
                       #coordsForFit = coordsForFit,
                       #coordsForPrediction = coordsForPrediction,
                       startendindex = startendindex)
    #mesh = mesh)

    disag_data <- disag_data$covariate_data
    disag_data <- disag_data %>% dplyr::left_join(polygon_shapefile, by = "ID")

    return(disag_data)

  }

  df_summ <- prepare_data(polygon_shapefile =data, covariate_rasters = rstack, id_var="ID",
                        response_var=response_var,na.action=TRUE)


  class(df_summ) <- append("as_disag", class(df_summ))
  return(df_summ)


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
#' \dontrun{
#' data(stock_vector)
#' disag_data <- as_disag(data=stock_vector)
#' }

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
