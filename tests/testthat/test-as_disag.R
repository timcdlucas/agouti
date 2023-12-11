## Testing as_disag function

test_that("as_disag returns a dataframe in the correct format", {

  data("madagascar_malaria")
  expect_true(inherits(as_disag(madagascar_malaria, response_var="case_rate"),"disag"))

  data("stock_vector")
  expect_true(inherits(as_disag(stock_vector), "disag"))

  ## these tests so far only consider three of the methods
})

test_that("as_disag returns a dataframe containing an ID column", {

  data("madagascar_malaria")
  expect_true("ID" %in% names(as_disag(madagascar_malaria, response_var="case_rate")))

  data("stock_vector")
  expect_true("ID" %in% names(as_disag(stock_vector)))

})

test_that("as_disag returns a dataframe where all rows with the same ID have the same outcome value", {

  data("madagascar_malaria")
  df <- as_disag(madagascar_malaria, response_var="case_rate") %>% dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(case_rate))
  expect_true(all(df$unique_response==1))

  data("stock_vector")
  df <- as_disag(stock_vector) %>% dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(response))
  expect_true(all(df$unique_response==1))


})
