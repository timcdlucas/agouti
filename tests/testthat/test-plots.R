test_that('Heat_hist makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- heat_hist(madagascar_malaria$EVI, madagascar_malaria$ID)
  expect_true(inherits(pp, "gg"))
})

test_that('group summary plot makes a plot of class gg', {

  data("madagascar_malaria")
  pp <- group_summary_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  expect_true(inherits(pp, "gg"))
})

test_that('threshold small multiple plot makes a plot of class gg', {

  data(madagascar_malaria)
  pp <- thresh_sm(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  expect_true(inherits(pp, "gg"))
})

test_that('Link plot makes a plot of class gg', {

  data(madagascar_malaria)
  pp <- link_plot(case_rate ~ LSTmean, data = madagascar_malaria, ID = ID, weights = pop)
  expect_true(inherits(pp, "gg"))
})
