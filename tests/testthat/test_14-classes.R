context("classes")

test_that("mf_seed", {
  ## make sure we can only use integer or numeric
  
  test14_int <- new("mf", seed = as.integer(123))
  test14_num <- new("mf", seed = as.numeric(123))
  
  
  expect_identical(test14_int$seed, as.integer(123))
  expect_identical(test14_num$seed, as.numeric(123))
  
  # reject character, logical, list, data.frame
  expect_error(new("mf", seed = "123"))
  expect_error(new("mf", seed = TRUE))
  expect_error(new("mf", seed = as.list(123)))
  expect_error(new("mf", seed = as.data.frame(123)))
})
