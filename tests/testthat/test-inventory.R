
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

  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass, 0.5)

  inventory <- pb210_inventory(
    core$cumulative_dry_mass,
    core$pb210_specific_activity_estimate,
    core$pb210_specific_activity_se
  )

  # must be decreasing everywhere
  expect_true(all(diff(drop_errors(inventory)) < 0))
})

test_that("lazy inventory calculator and inventory function defaults are identical", {

  core <- withr::with_seed(4817, {
    accumulation <- pb210_simulate_accumulation(pb210_mass_accumulation_rlnorm(sd = 1)) %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass, 0.5)

  calc <- pb210_inventory_calculator()
  expect_is(calc, "inventory_calculator")
  expect_is(calc$model_top, "pb210_fit_lazy")
  expect_is(calc$model_bottom, "pb210_fit_lazy")

  inventory <- pb210_inventory(
    core$cumulative_dry_mass,
    core$pb210_specific_activity_estimate,
    core$pb210_specific_activity_se
  )

  inventory_lazy <- pb210_inventory_calculator() %>%
    predict(
      core$cumulative_dry_mass,
      core$pb210_specific_activity_estimate,
      core$pb210_specific_activity_se
    )

  expect_equal(inventory, inventory_lazy)

  crs <- pb210_crs(
    core$cumulative_dry_mass,
    core$pb210_specific_activity_estimate,
    core$pb210_specific_activity_se,
    inventory = inventory
  ) %>%
    predict()

  crs_lazy <- pb210_crs(
    core$cumulative_dry_mass,
    core$pb210_specific_activity_estimate,
    core$pb210_specific_activity_se,
    inventory = pb210_inventory_calculator()
  ) %>%
    predict()

  expect_equal(crs, crs_lazy)
})
