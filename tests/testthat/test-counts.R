context("test-counts")

test_that("pb210_count/activity functions are inverses", {
  mass <- 0.5 / 1000
  time <- lubridate::ddays(1)
  specific_activities <- c(500, 100, 12)
  expect_identical(
    specific_activities %>%
      pb210_counts_from_activity(mass, time) %>%
      pb210_activity_from_counts(mass, time),
    specific_activities
  )
})

test_that("pb210_error functions give consistent information", {
  mass <- 0.5 / 1000
  time <- lubridate::ddays(1)
  specific_activities <- c(500, 100, 12)
  expect_identical(
    specific_activities %>%
      pb210_counts_from_activity(mass, time) %>%
      pb210_error_from_counts(mass, time),
    specific_activities %>%
      pb210_error_from_activity(mass, time)
  )
})

test_that("pb210 count functions accept difftime, raw seconds, and duration objects", {
  seconds <- c(1485810, 10493, 10)
  expect_identical(count_time_seconds(seconds), seconds)
  expect_identical(count_time_seconds(lubridate::dseconds(seconds)), seconds)
  expect_identical(count_time_seconds(as.difftime(seconds, units = "secs")), seconds)
})
