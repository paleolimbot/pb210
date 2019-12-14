
test_that("pb210_sample_norm() works with and without errors", {
  expect_identical(pb210_sample_norm(1:10), 1:10)
  expect_length(pb210_sample_norm(set_errors(1:10, 1)), 10)
  expect_is(pb210_sample_norm(set_errors(1:10, 1)), "numeric")
})

test_that("MC works for CIC fit", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)


  cic_mc <- withr::with_seed(483, {
    pb210_cic_monte_carlo(
      core$cumulative_dry_mass,
      set_errors(core$activity, core$activity_se),
      n = 50
    )
  })

  cic_mc_ages <- predict(cic_mc)
  cic_ages <- predict(cic_mc$fit_base)
  expect_ages_similar(cic_mc_ages$age[1:40], cic_ages$age[1:40], max_delta = 0.4)
})

test_that("MC works for CRS fit", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)


  crs_mc <- withr::with_seed(483, {
    pb210_crs_monte_carlo(
      core$cumulative_dry_mass,
      set_errors(core$activity, core$activity_se),
      n = 50
    )
  })

  crs_mc_ages <- predict(crs_mc)
  crs_ages <- predict(crs_mc$fit_base)
  expect_ages_similar(crs_mc_ages$age[1:40], crs_ages$age[1:40], max_delta = 0.4)
})