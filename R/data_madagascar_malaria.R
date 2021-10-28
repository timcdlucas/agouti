#' Madagascar Malaria data
#'
#' This data includes areal counts of malaria cases in madagascar, and high 
#' resolution covariates and population counts.
#' The format is one row per high resolution pixel, with the values for the areal
#' data being repeated. And ID column unambiguously links the two scales.
#'
#' @seealso The script to create this data set: \url{}
#'
#' @format A data.frame with x observations and y variables:
#' \describe{
#' \item{ID}{An ID column for the areal data with}
#' \item{Temp, NVDI}{Temperature and vegeation index covariates}
#' }
#' @examples
#' madagascar_malaria
"madagascar_malaria"
