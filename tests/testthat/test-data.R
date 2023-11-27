test_that("Data is of expected class", {
  data(stock_data)
  expect_true(inherits(stock_data,"as_disag"))
  data("madagascar_malaria")
  expect_false(inherits(madagascar_malaria,"as_disag"))
  data("stock_vector")
  expect_false(inherits(stock_vector,"as_disag"))

})
