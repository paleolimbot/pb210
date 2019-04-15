context("test-age")

test_that("CIC model works on simulated core data", {
  core <- withr::with_seed(4817, {
    pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant()) %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  cic_model <- pb210_age_cic(
    depth = core$depth,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se,
    calc_excess_pb210_surface = pb210_surface_estimate
  )

  # not quite within 1 year for all samples
  expect_ages_similar(cic_model$age, core$age, max_delta = 1.32)

  crs_model <- pb210_age_crs(
    depth = core$depth,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se,
    sample_mass = core$slice_mass,
    core_area = 1,
    calc_excess_pb210_surface = pb210_surface_estimate,
    calc_inventory_surface = pb210_surface_estimate,
    calc_inventory_below = pb210_deep_inventory_estimate
  )

  expect_ages_similar(crs_model$age, core$age)
})

test_that("CRS model works on simulated core data", {
  core <- withr::with_seed(4817, {
    pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_rlnorm(sd = 1)) %>%
      pb210_simulate_core() %>%
      pb210_simulate_counting()
  })

  crs_model <- pb210_age_crs(
    depth = core$depth,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se,
    sample_mass = core$slice_mass,
    core_area = 1,
    calc_excess_pb210_surface = pb210_surface_estimate,
    calc_inventory_surface = pb210_surface_estimate,
    calc_inventory_below = pb210_deep_inventory_estimate
  )

  expect_ages_similar(crs_model$age, core$age)
})

test_that("CRS model works on Alta Lake data", {

  real_pb210 <- alta_lake_210Pb[is.finite(alta_lake_210Pb$depth), ]
  real_ages <- pb210_age_crs(
    real_pb210$depth,
    real_pb210$excess_210Pb_Bq_g,
    sample_mass = real_pb210$slice_mass_g,
    excess_pb210_sd = real_pb210$excess_210Pb_sd_Bq,
    calc_inventory_surface = pb210_surface_min_depth,
    calc_inventory_below = pb210_deep_inventory_zero,
    decay_constant = 0.03108,
    core_area = pb210_core_area(0.063)
  )

  real_ages$age_compare <- 2014.60215053763 - real_pb210$crs_age_section_top_ad
  real_ages$sd_compare <- real_pb210$crs_age_sd_yr

  expect_true(all(abs(real_ages$age - real_ages$age_compare) < 0.0000001, na.rm = TRUE))
  expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 0.000001, na.rm = TRUE))
})

test_that("exponential surface estimation works", {
  withr::with_seed(287, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
    expect_true(abs(pb210_surface_estimate_loglinear(fake_depth, fake_pb210) - fake_pb210[1]) < 20)
    expect_true(abs(pb210_surface_estimate(fake_depth, fake_pb210) - fake_pb210[1]) < 0.01)
  })
})

test_that("min depth surface estimation works", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  expect_identical(pb210_surface_min_depth(fake_depth, fake_pb210), fake_pb210[1])
})

test_that("deep inventory estimation works", {
  withr::with_seed(583, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)

    # here m = -1 and b = 5, and we want cumulative area under the curve from 10 to infinity
    # integrated, it's exp(m*x + b) / m, and because at x = infinity the integral is 0
    # the definite integral from [background] to infinity is exp(m * [background] + b) / -m
    known_background <- exp(-1 * 10 + 5) / 1
    calc_background_loglin <- pb210_deep_inventory_estimate_loglinear(fake_depth, fake_pb210)
    calc_background_exp <- pb210_deep_inventory_estimate(fake_depth, fake_pb210)
    expect_true(abs(calc_background_loglin - known_background) < 0.01)
    expect_true(abs(calc_background_exp - known_background) < 0.00001)
  })
})
