context("test-utils")

test_that("expect_ages_similar works as intended", {
  withr::with_seed(4819, {
    expect_silent(expect_ages_similar(1:10 + runif(10, -1, 1), 1:10, max_delta = 1))
    expect_error(expect_ages_similar(1:10 + runif(10, -1, 1), 1:10, max_delta = 0.1))
  })
})

test_that("expect_ages_similar failes when there are zero values", {
  expect_silent(expect_ages_similar(1:10, 1:10, age_range = 10:12))
  expect_error(expect_ages_similar(1:10, 1:10, age_range = 11:12))
})

test_that("expect_ages_similar failes when there are zero finite values", {
  expect_silent(expect_ages_similar(c(NA_real_, 2:10), 1:10, na.rm = TRUE))
  expect_error(expect_ages_similar(c(NA_real_, 2:10), 1:10, na.rm = FALSE), "isn't true")
  expect_error(expect_ages_similar(NA_real_, 1:10, na.rm = TRUE), "Zero ages")
})
