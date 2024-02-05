test_that("Internal data is of expected class", {
  data(stock_data)
  expect_true(inherits(stock_data,"disag"))
  data("madagascar_malaria")
  expect_false(inherits(madagascar_malaria,"disag"))
  data("stock_vector")
  expect_false(inherits(stock_vector,"disag"))

})
