context("test-utils")

test_that("pb210_core_area() defaults to 1 m^2", {
  expect_equal(pb210_core_area(), 1)
})

test_that("pb210_slice_volume() defaults to 1m^3", {
  expect_equal(pb210_slice_volume(1), 1)
})

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
  expect_error(expect_ages_similar(c(NA_real_, 2:10), 1:10, na.rm = FALSE), class = "expectation_failure")
  expect_error(expect_ages_similar(NA_real_, 1:10, na.rm = TRUE), "Zero ages")
})

test_that("with_errors() warns when multiple error types are specified", {
  expect_silent(with_errors(1, 1))
  expect_silent(with_errors(set_errors(1, 1), NA_real_))
  expect_equal(
    with_errors(1, 1),
    with_errors(set_errors(1, 1), NA_real_)
  )
  expect_warning(with_errors(set_errors(1, 1), 1), "Two errors")
})

test_that("extract_errors() warns when multiple error types are specified", {
  expect_silent(extract_errors(1, 1))
  expect_silent(extract_errors(set_errors(1, 1), NA_real_))
  expect_equal(
    extract_errors(1, 1),
    extract_errors(set_errors(1, 1), NA_real_)
  )
  expect_warning(extract_errors(set_errors(1, 1), 1), "Two errors")
})
