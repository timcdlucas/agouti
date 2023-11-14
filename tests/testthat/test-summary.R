test_that("Summary fails if names of variables mis-spelt", {
  data("madagascar_malaria")
  expect_error(agouti_summary(madagascar_malaria, high_res="POP"))
})
