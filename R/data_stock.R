#' Stock data
#'
#' This data contains stock data for the companys in the S&P500.
#' The response variable, growth, is the a stocks closing price, divded by it's
#' closing price the previous day, minus 1 (i.e. daily percentage change). The
#' main covariate is the lagged growth rate and here we consider 50 lags. 
#' However, instead of treating these 50 lags as 50 variables as in normal
#' time series modelling, we treat them all as 1 variable and therefore each
#' datapoint has 50 disaggregated rows of covariate data. The only other
#' covariate is the company (symbol).
#' 
#' We collected data 2017 to 2013, but then only used data where the date of 
#' the response data was the 10th of the month. This reduces the data size
#' and avoid having too many of the same covariate value occur multiple times as
#' it corresponds to different lags.
#' 
#' 
#'
#' @seealso The script to create this data set: \url{}
#'
#' @format A data.frame with x observations and y variables:
#' \describe{
#' \item{ID}{An ID column for each day of response data.}
#' \item{growth}{Daily percentage change in the value of the stock market.}
#' \item{lagged_growth}{The growth of the stock in the preceeding 1 to 50 days.}
#' \item{lag}{Which lag this row corresponds to.}
#' \item{symbol}{The stock ticker.}
#' \item{date}{The date of response data.}
#' }
#' @export
#' @examples
#' data(stock_data)
"stock_data"
