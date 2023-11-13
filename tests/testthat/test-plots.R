test_that('Heat_hist makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- heat_hist(madagascar_malaria$EVI, madagascar_malaria$ID)
  data("stock_data")
  p <- heat_hist(stock_data$lagged_growth, stock_data$ID)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('group summary plot makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- group_summary_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  data("stock_data")
  p <- group_summary_plot(growth ~ lagged_growth, data = stock_data, ID = ID, weights=1)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('threshold small multiple plot makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- thresh_sm(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  data("stock_data")
  p <- thresh_sm(growth ~ lagged_growth, data = stock_data, ID = ID)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})

test_that('Link plot makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- link_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  data("stock_data")
  p <- link_plot(growth ~ lagged_growth, data = stock_data, ID = ID, weights = 1)
  expect_true(inherits(pp, "gg"))
  expect_true(inherits(p, "gg"))
})
