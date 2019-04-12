context("test-utils")

test_that("time function calculates times in seconds correctly", {
  expect_equal(pb210_time_seconds(), 0)
  expect_equal(pb210_time_seconds(days = 1), pb210_time_seconds(hours = 24))
  expect_equal(pb210_time_seconds(hours = 1), pb210_time_seconds(minutes = 60))
  expect_equal(pb210_time_seconds(minutes = 1), pb210_time_seconds(seconds = 60))
})
