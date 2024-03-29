test_that('Heat_hist makes a plot of class gg', {

  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, response_var="case_rate")
  pp <- heat_hist(disag_data$EVI, disag_data$ID)
  data("stock_data")
  p <- heat_hist(stock_data$lagged_growth, stock_data$ID)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('group summary plot makes a plot of class gg', {

  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, response_var="case_rate")
  pp <- group_summary_plot(case_rate ~ LSTmean, data = disag_data, ID = ID, weights = pop)
  data("stock_data")
  p <- group_summary_plot(growth ~ lagged_growth, data = stock_data, ID = ID, weights=1)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('threshold small multiple plot makes a plot of class gg', {

  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, response_var="case_rate")
  pp <- thresh_sm(case_rate ~ LSTmean, data = disag_data, ID = ID, weights = pop)
  data("stock_data")
  p <- thresh_sm(growth ~ lagged_growth, data = stock_data, ID = ID)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('Link plot makes a plot of class gg', {

  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, response_var="case_rate")
  pp <- link_plot(case_rate ~ LSTmean, data = disag_data, ID = ID, weights = pop)
  data("stock_data")
  p <- link_plot(growth ~ lagged_growth, data = stock_data, ID = ID, weights = 1)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('A warning message is printed when data to be plotted has NAs', {

  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, response_var="case_rate")
  expect_no_warning(thresh_sm(case_rate ~ LSTmean, data = disag_data, ID = ID, weights = pop))
  data("stock_vector")
  disag_ts <- as_disag(stock_vector)
  expect_warning(thresh_sm(response ~covariate, data = disag_ts, ID = ID), "NA have been removed")

})
