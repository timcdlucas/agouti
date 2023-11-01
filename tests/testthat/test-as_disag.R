test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("as_disag returns a dataframe in the correct format", {

  data("madagascar_malaria")
  expect_equal(class(as_disag(madagascar_malaria, outcome="case_rate"))[1], "as_disag")

  data("stock_vector")
  expect_equal(class(as_disag(stock_vector))[1], "as_disag")

  data("mortality_temporal")
  expect_equal(class(as_disag(mortality_temporal$Datetime, response_df=mortality_temporal))[1], "as_disag")

  ## add examples for the other methods

})
