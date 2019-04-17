context("test-physical_parameters")

test_that("cumulative_mass function works", {
  masses <- withr::with_seed(39, runif(10))
  expect_identical(pb210_cumulative_mass(masses, position = 1), cumsum(masses))
  expect_identical(pb210_cumulative_mass(masses, position = 0), c(0, cumsum(masses[-1])))
})
