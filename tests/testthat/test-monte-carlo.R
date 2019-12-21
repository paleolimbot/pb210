
test_that("pb210_sample_norm() works with and without errors", {
  expect_identical(pb210_sample_norm(1:10), 1:10)
  expect_length(pb210_sample_norm(set_errors(1:10, 1)), 10)
  expect_is(pb210_sample_norm(set_errors(1:10, 1)), "numeric")
})

test_that("NA fits work correctly", {
  expect_identical(
    predict(pb210_age_depth_na(), out_length = 1, out_names = "age"),
    tibble::tibble(age = NA_real_)
  )
})

test_that("monte-carlo fits with errors can be predict()ed", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)

  bad_fits <- expect_warning(
    fit_many(
      set_errors(core$activity, core$activity_se),
      background = 0,
      fit_fun = function(ex, dc, ...) {
        if(inherits(ex, "errors")) {
          pb210_cic(core$cumulative_dry_mass, ex, ..., decay_constant = dc)
        } else {
          rlang::abort("fail!")
        }
      },
      n = 3
    ),
    "failed to fit"
  )

  expect_equal(nrow(predict.pb210_fit_cic_monte_carlo(bad_fits)), nrow(core))
  expect_equal(nrow(predict.pb210_fit_crs_monte_carlo(bad_fits)), nrow(core))
})

test_that("pb210_sample_norm() is quiet when there are NA values in x", {
  expect_silent(pb210_sample_norm(NA_real_))
  expect_identical(pb210_sample_norm(NA_real_), NA_real_)

  expect_silent(pb210_sample_norm(set_errors(NA_real_, 0)))
  expect_identical(pb210_sample_norm(set_errors(NA_real_, 0)),NA_real_)
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
