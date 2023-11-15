## Testing as_disag function

test_that("as_disag returns a dataframe in the correct format", {

  data("madagascar_malaria")
  expect_true(inherits(as_disag(madagascar_malaria, outcome="case_rate"),"as_disag"))

  data("stock_vector")
  expect_true(inherits(as_disag(stock_vector), "as_disag"))

  data("mortality_temporal")
  expect_true(inherits(as_disag(mortality_temporal$Datetime, response_df=mortality_temporal), "as_disag"))

  ## these tests so far only consider three of the methods
})

test_that("as_disag returns a dataframe containing an ID column", {

  data("madagascar_malaria")
  expect_true("ID" %in% names(as_disag(madagascar_malaria, outcome="case_rate")))

  data("stock_vector")
  expect_true("ID" %in% names(as_disag(stock_vector)))

  data("mortality_temporal")
  expect_true("ID" %in% names(as_disag(mortality_temporal$Datetime, response_df=mortality_temporal)))

})

test_that("as_disag returns a dataframe where all rows with the same ID have the same outcome value", {

  data("madagascar_malaria")
  df <- as_disag(madagascar_malaria, outcome="case_rate") %>% dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(case_rate))
  expect_true(all(df$unique_response==1))

  data("stock_vector")
  df <- as_disag(stock_vector) %>% dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(outcome))
  expect_true(all(df$unique_response==1))

  data("mortality_temporal")
  df <- as_disag(mortality_temporal$Datetime, response_df=mortality_temporal) %>%
    dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(outcome))
  expect_true(all(df$unique_response==1))

})
