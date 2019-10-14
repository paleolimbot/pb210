context("test-age")

test_that("CIC model works on simulated core data", {
  accumulation <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant())
  core <- withr::with_seed(4817, {
    accumulation <- accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  accumulation$cumulative_dry_mass <- pb210_cumulative_mass(accumulation$slice_mass, 0.5)
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass, 0.5)

  cic_model_exact <- pb210_cic(
    cumulative_dry_mass = accumulation$cumulative_dry_mass,
    excess_pb210 = accumulation$pb210_specific_activity
  ) %>%
    predict()

  expect_ages_similar(cic_model_exact$age, accumulation$age, 0.1)

  cic_model <- pb210_cic(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  ) %>%
    predict()

  # not quite within 1 year for all samples
  expect_ages_similar(cic_model$age, core$age, max_delta = 2)

  # CRS model is also valid here
  crs_model <- pb210_crs(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  ) %>%
    predict()

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
  accumulation$cumulative_dry_mass <- pb210_cumulative_mass(accumulation$slice_mass, 0.5)
  accumulation$inventory <- rev(cumsum(rev(accumulation$pb210_specific_activity * accumulation$slice_mass)))

  crs_model_exact <- pb210_crs(
    cumulative_dry_mass = accumulation$cumulative_dry_mass,
    excess_pb210 = accumulation$pb210_specific_activity,
    inventory = accumulation$inventory
  ) %>%
    predict()

  expect_ages_similar(crs_model_exact$age, accumulation$age, max_delta = 3)

  # a less perfect world: a core with varying sedimentation rate
  # the best this gets is 12 years in the last 100 (with the defaults)
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass, 0.5)
  crs_model <- pb210_crs(
    cumulative_dry_mass = core$cumulative_dry_mass,
    excess_pb210 = core$pb210_specific_activity_estimate,
    excess_pb210_sd = core$pb210_specific_activity_se
  ) %>%
    predict()

  expect_ages_similar(crs_model$age, core$age, max_delta = 12)
})

test_that("CRS calculations for real core data do not change", {
  df <- alta_lake_pb210
  core_area <- pb210_core_area(0.063)

  df_background <- df %>%
    dplyr::filter(depth_cm > 8) %>%
    dplyr::summarise(
      background = mean(total_pb210_Bq_kg, na.rm = TRUE),
      background_sd = sd(total_pb210_Bq_kg, na.rm = TRUE)
    )

  df$excess_pb210 <- pb210_excess(
    set_errors(df$total_pb210_Bq_kg, df$total_pb210_sd),
    set_errors(df_background$background, df_background$background_sd)
  )

  df$excess_pb210[df$depth_cm > 8] <- NA

  # cumulative dry mass on a per-core area basis are most useful
  df$cumulative_dry_mass <- pb210_cumulative_mass(df$slice_mass_g / 1000 / core_area)
  df$inventory <- pb210_inventory(
    df$cumulative_dry_mass,
    df$excess_pb210,
    model_bottom = 0
  )

  ages <- pb210_crs(
    df$cumulative_dry_mass,
    df$excess_pb210,
    inventory = df$inventory
  ) %>%
    predict()

  expect_identical(
    is.na(ages$age),
    is.na(
      c(1.7997, 4.45888, 8.1515, 12.48042, 25.59803, 41.63099, 61.26054,
        83.31264, 127.34372, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  )

  expect_ages_similar(
    ages$age[1:9],
    c(1.74603, 4.57826, 8.09783, 12.42675, 25.54436, 41.57732, 61.20688, 83.25898, 127.29005),
    max_delta = 0.0001
  )

  expect_identical(
    is.na(ages$age_sd),
    is.na(
      c(1.8076, NA, 1.77354, 1.71348, 1.81861, 2.49783, 3.71956, 4.75733,
        10.86162, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  )

  expect_ages_similar(
    ages$age_sd[c(1, 3:9)],
    c(1.8076, 1.77354, 1.71348, 1.81861, 2.49783, 3.71956, 4.75733, 10.86162),
    max_delta = 0.0001
  )
})
