test_that("Summary fails if names of variables mis-spelt", {
  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, outcome="case_rate")
  expect_error(agouti_summary(disag_data, high_res="POP"), "check spelling")

})


test_that("Summary fails if named ID variable is not present", {
  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, outcome="case_rate")
  expect_error(agouti_summary(disag_data, ID="id"), "Data does not contain the variable")
})


test_that("Summary creates a table in the correct format", {
  data("madagascar_malaria")
  disag_data <- as_disag(madagascar_malaria, outcome="case_rate")
  expect_true(nrow(agouti_summary(disag_data, high_res="pop"))==3)
  expect_true(ncol(agouti_summary(disag_data, high_res="pop"))>=1)

})
