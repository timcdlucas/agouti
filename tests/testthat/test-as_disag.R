## Testing as_disag function

test_that("as_disag returns a dataframe in the correct format", {

  data("madagascar_malaria")
  expect_true(inherits(as_disag(madagascar_malaria, outcome="case_rate"),"as_disag"))

  data("stock_vector")
  expect_true(inherits(as_disag(stock_vector), "as_disag"))

  ## sf example
  polygons <- sf::st_as_sf(NYleukemia$spatial.polygon)
  df <- cbind(polygons, NYleukemia$data)
  names(df)[1] <-"ID"
  covariate <- terra::rast("vignettes/annual_mean_temp_newyork.tif")
  expect_true(inherits(as_disag(data=df,rstack=covariate, response_var="cases")))

  ## these tests so far only consider three of the methods
})

test_that("as_disag returns a dataframe containing an ID column", {

  data("madagascar_malaria")
  expect_true("ID" %in% names(as_disag(madagascar_malaria, outcome="case_rate")))

  data("stock_vector")
  expect_true("ID" %in% names(as_disag(stock_vector)))

  ## sf example
  polygons <- sf::st_as_sf(NYleukemia$spatial.polygon)
  df <- cbind(polygons, NYleukemia$data)
  names(df)[1] <-"ID"
  covariate <- terra::rast("vignettes/annual_mean_temp_newyork.tif")
  expect_true("ID" %in% names(as_disag(data=df,rstack=covariate, response_var="cases")))

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

  ## sf example
  polygons <- sf::st_as_sf(NYleukemia$spatial.polygon)
  df <- cbind(polygons, NYleukemia$data)
  names(df)[1] <-"ID"
  covariate <- terra::rast("vignettes/annual_mean_temp_newyork.tif")
  df2 <- as_disag(data=df,rstack=covariate, response_var="cases") %>% dplyr::group_by(.data$ID)%>%
    dplyr::mutate(unique_response=dplyr::n_distinct(cases))
  expect_true(all(df2$unique_response==1))


})
