test_that("Summary fails if names of variables mis-spelt", {
  data("madagascar_malaria")
  expect_error(agouti_summary(madagascar_malaria, high_res="POP"))
})


test_that("Summary fails if named ID variable is not present", {
  data("madagascar_malaria")
  expect_error(agouti_summary(madagascar_malaria, ID="id"))
})


test_that("Summary creates a table in the correct format", {
  data("madagascar_malaria")
  expect_true(nrow(agouti_summary(madagascar_malaria, high_res="pop"))==3)
  expect_true(ncol(agouti_summary(madagascar_malaria, high_res="pop"))>=1)

  data("mortality_temporal")
  disag_data <- as_disag(mortality_temporal$Datetime, response_df=mortality_temporal,time_group="%Y%m%d",outcome="Death")
  expect_true(nrow(agouti_summary(disag_data))==3)
  expect_true(ncol(agouti_summary(disag_data))>=1)
})
