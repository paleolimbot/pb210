context("test-age")

test_that("CIC model works on simulated core data", {
  accumulation <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant())
  core <- withr::with_seed(4817, {
    accumulation <- accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  accumulation$cumulative_dry_mass <- (cumsum(accumulation$slice_mass) + c(0, cumsum(accumulation$slice_mass[-1]))) / 2
  core$cumulative_dry_mass <- (cumsum(core$slice_mass) + c(0, cumsum(core$slice_mass[-1]))) / 2

  cic_model_exact <- pb210_age_cic(
    cumulative_dry_mass = accumulation$cumulative_dry_mass,
    excess_pb210 = accumulation$pb210_specific_activity
  )
  expect_ages_similar(cic_model_exact$age, accumulation$age, 0.1)

  cic_model <- pb210_age_cic(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  )

  # not quite within 1 year for all samples
  expect_ages_similar(cic_model$age, core$age, max_delta = 2)

  # CRS model is also valid here
  crs_model <- pb210_age_crs(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  )

  # CRS model does quite well here
  expect_ages_similar(crs_model$age, core$age, max_delta = 0.8)
})

test_that("CRS model works on simulated core data", {

  # this simulation is a wildly varying sedimentation rate
  accumulation <- withr::with_seed(283, {
    pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_rlnorm(sd = 1))
  })
  core <- withr::with_seed(4817, {
    accumulation <- accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  # even in a perfect world, the best I can get is 3 years of accuracy in the last 100 years
  accumulation$cumulative_dry_mass <- (cumsum(accumulation$slice_mass) + c(0, cumsum(accumulation$slice_mass[-1]))) / 2
  accumulation$inventory <- rev(cumsum(rev(accumulation$pb210_specific_activity * accumulation$slice_mass)))

  crs_model_exact <- pb210_age_crs(
    cumulative_dry_mass = accumulation$cumulative_dry_mass,
    excess_pb210 = accumulation$pb210_specific_activity,
    inventory = accumulation$inventory
  )
  expect_ages_similar(crs_model_exact$age, accumulation$age, max_delta = 3)

  # a less perfect world: a core with varying sedimentation rate
  # the best this gets is 12 years in the last 100 (with the defaults)
  core$cumulative_dry_mass <- (cumsum(core$slice_mass) + c(0, cumsum(core$slice_mass[-1]))) / 2
  crs_model <- pb210_age_crs(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  )
  expect_ages_similar(crs_model$age, core$age, max_delta = 12)
})

test_that("inventory calculation works", {
  withr::with_seed(29, {
    fake_mass <- 0:10
    fake_pb210 <- exp(5 - fake_mass) + rnorm(11, sd = 0.005)
    known_coeffs <- c(m = -1, b = 5)
    known_inventory <- unname(exp(known_coeffs["m"] * fake_mass  + known_coeffs["b"]) / -known_coeffs["m"])
    calc_inventory <- pb210_inventory(fake_mass, fake_pb210) %>% drop_errors()

    expect_true(all(is.finite(calc_inventory)))
    expect_true(
      all(abs(log(calc_inventory) - log(known_inventory)) < 0.3)
    )
  })
})

test_that("inventory calculation works with wildly varying sedimentation rates", {
  core <- withr::with_seed(4817, {
    accumulation <- pb210_simulate_accumulation(pb210_mass_accumulation_rlnorm(sd = 1)) %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  core$cumulative_dry_mass <- (cumsum(core$slice_mass) + c(0, cumsum(core$slice_mass[-1]))) / 2

  inventory <- pb210_inventory(
    core$cumulative_dry_mass,
    core$pb210_specific_activity_estimate,
    core$pb210_specific_activity_se
  )

  # must be decreasing everywhere
  expect_true(all(diff(drop_errors(inventory)) < 0))
})

test_that("excess function works", {
  expect_equal(as.numeric(pb210_excess(2:11)), 2:11)
  expect_equal(as.numeric(pb210_excess(2:11, 1)), 1:10)
})

test_that("excess function propogates error", {
  expect_is(pb210_excess(2:11), "errors")
  expect_equal(errors(pb210_excess(3:12, 1, 1, 1)), rep(sqrt(2), 10))
})

test_that("excess function accepts error objects", {
  expect_equal(
    pb210_excess(2:11, 1, 1, 1),
    pb210_excess(set_errors(2:11, 1), set_errors(1, 1))
  )
})

test_that("cumulative mass and excess_pb210 assumptions are checked", {
  mass <- 0:2
  pb210 <- c(10, 1, 0.1)
  expect_silent(check_mass_and_activity(mass, pb210))
  # no non-finite values
  expect_error(check_mass_and_activity(c(0, 1, NA), pb210), "is not TRUE")
  # negative excess values
  expect_error(check_mass_and_activity(mass, c(10, 1, -1)), "is not TRUE")
  # not enough finite values
  expect_error(check_mass_and_activity(mass, c(10, 1, NA)), "is not TRUE")
  expect_error(check_mass_and_activity(mass, c(10, 1, 0)), "is not TRUE")
  # inconsistent lengths
  expect_error(check_mass_and_activity(0:3, pb210), "is not TRUE")
  expect_error(check_mass_and_activity(mass, c(pb210, 0.01)), "is not TRUE")
  expect_error(check_mass_and_activity(mass, pb210, c(1, 2)), "is not TRUE")
})
