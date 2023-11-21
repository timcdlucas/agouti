#' Stock vector
#'
#' This data contains stock data for the companys in the S&P500.
#' The response variable, growth, is the a stocks closing price, divded by it's
#' closing price the previous day, minus 1 (i.e. daily percentage change)
#'
#'
#'
#'
#' @seealso The script to create this data set: \url{}
#'
#' @format A ts vector with x observations:
#' \describe{
#' \item{growth}{A vector containing daily stock growths}
#' }
#' @usage data(stock_vector)
#' @examples
#' data(stock_vector)
"stock_vector"
