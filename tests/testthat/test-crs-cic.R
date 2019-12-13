context("test-age")

test_that("CIC model works on simulated core data", {
  accumulation <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant())
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  accumulation$cumulative_dry_mass <- pb210_cumulative_mass(accumulation$slice_mass)
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)

  cic_model_exact <- pb210_cic(
    accumulation$cumulative_dry_mass,
    accumulation$activity
  ) %>%
    predict()

  expect_ages_similar(cic_model_exact$age, accumulation$age, 0.1)

  cic_model <- pb210_cic(
    core$cumulative_dry_mass,
    set_errors(
      core$activity_estimate,
      core$activity_se
    )
  ) %>%
    predict()

  # not quite within 1 year for all samples
  expect_ages_similar(cic_model$age, core$age, max_delta = 2)

  # CRS model is also valid here
  crs_model <- pb210_crs(
    core$cumulative_dry_mass,
    set_errors(
      core$activity_estimate,
      core$activity_se
    )
  )

  crs_ages <- predict(crs_model)

  # CRS model does quite well here
  expect_ages_similar(crs_ages$age, core$age, max_delta = 0.8)
})

test_that("CIC/CRS calculations are identical with and without error", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)

  cic_fit_with_error <-  pb210_cic(
    core$cumulative_dry_mass,
    set_errors(core$activity, core$activity_se)
  )
  cic_model_with_error <- predict(cic_fit_with_error)

  cic_fit_without_error <- pb210_cic(
    core$cumulative_dry_mass,
    core$activity
  )

  cic_model_without_error <- predict(cic_fit_without_error)

  expect_equal(cic_model_with_error$age, cic_model_without_error$age)
  expect_true(any(is.finite(cic_model_with_error$age_sd)))
  expect_false(any(is.finite(cic_model_without_error$age_sd)))
  expect_true(cic_fit_with_error$use_errors)
  expect_false(cic_fit_without_error$use_errors)

  crs_fit_with_error <-  pb210_crs(
    core$cumulative_dry_mass,
    set_errors(core$activity, core$activity_se)
  )
  crs_model_with_error <- predict(crs_fit_with_error)

  crs_fit_without_error <- pb210_crs(
    core$cumulative_dry_mass,
    core$activity
  )
  crs_model_without_error <- predict(crs_fit_without_error)

  expect_equal(crs_model_with_error$age, crs_model_without_error$age)
  expect_equal(crs_model_with_error$mar, crs_model_without_error$mar)
  expect_equal(crs_model_with_error$inventory, crs_model_without_error$inventory)
  expect_true(crs_fit_with_error$use_errors)
  expect_true(crs_fit_with_error$fit_cic$use_errors)
  expect_false(crs_fit_without_error$use_errors)
  expect_false(crs_fit_without_error$fit_cic$use_errors)

  expect_true(any(is.finite(crs_model_with_error$age_sd)))
  expect_false(any(is.finite(crs_model_without_error$age_sd)))
  expect_true(any(is.finite(crs_model_with_error$mar_sd)))
  expect_false(any(is.finite(crs_model_without_error$mar_sd)))
  expect_true(any(is.finite(crs_model_with_error$inventory_sd)))
  expect_false(any(is.finite(crs_model_without_error$inventory_sd)))
})

test_that("CIC/CRS calculations are faster without error", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)

  # for CIC
  predict_error <- function() {
    predict.pb210_fit_cic(
      pb210_cic(
        core$cumulative_dry_mass,
        set_errors(core$activity, core$activity_se)
      )
    )$age
  }

  predict_no_error <- function() {
    predict.pb210_fit_cic(
      pb210_cic(
        core$cumulative_dry_mass,
        core$activity
      )
    )$age
  }

  expect_identical(predict_error(), predict_no_error())

  error_yes <- median(sapply(1:10, function(x) system.time(predict_error())[1]))
  error_no <- median(sapply(1:10, function(x) system.time(predict_no_error())[1]))
  expect_true(error_yes > error_no)

  # for CRS
  predict_error <- function() {
    predict.pb210_fit_crs(
      pb210_crs(
        core$cumulative_dry_mass,
        set_errors(core$activity, core$activity_se)
      )
    )$age
  }

  predict_no_error <- function() {
    predict.pb210_fit_crs(
      pb210_crs(
        core$cumulative_dry_mass,
        core$activity
      )
    )$age
  }

  expect_identical(predict_error(), predict_no_error())

  error_yes <- median(sapply(1:10, function(x) system.time(predict_error())[1]))
  error_no <- median(sapply(1:10, function(x) system.time(predict_no_error())[1]))
  expect_true(error_yes > error_no)
})

test_that("predict methods can accept any number of inputs", {
  accumulation <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant()
  )
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)

  cic <- pb210_cic(core$cumulative_dry_mass, core$activity)

  expect_identical(
    predict(cic, numeric(0)),
    tibble::tibble(age = numeric(0), age_sd = numeric(0))
  )
  expect_identical(nrow(predict(cic, 0)), 1L)
  expect_identical(nrow(predict(cic, 1:5)), 5L)

  # make sure order doesn't have to be increasing
  expect_identical(
    predict(cic, 1:10)$age,
    rev(predict(cic, 10:1)$age)
  )

  crs <- pb210_cic(core$cumulative_dry_mass, core$activity)

  expect_identical(
    predict(crs, numeric(0)),
    tibble::tibble(age = numeric(0), age_sd = numeric(0))
  )
  expect_identical(nrow(predict(crs, 0)), 1L)
  expect_identical(nrow(predict(crs, 1:5)), 5L)

  # make sure order doesn't have to be increasing
  expect_identical(
    predict(crs, 1:10)$age,
    rev(predict(crs, 10:1)$age)
  )
})

test_that("CRS model works on simulated core data", {

  # this simulation is a wildly varying sedimentation rate
  accumulation <- withr::with_seed(283, {
    pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_rlnorm(sd = 1))
  })
  core <- withr::with_seed(4817, {
    accumulation %>%
      pb210_simulate_core(core_area = 1) %>%
      pb210_simulate_counting()
  })

  # even in a perfect world, the best I can get is 3 years of accuracy in the last 100 years
  accumulation$cumulative_dry_mass <- pb210_cumulative_mass(accumulation$slice_mass)
  accumulation$inventory <- rev(cumsum(rev(accumulation$activity * accumulation$slice_mass)))

  crs_model_exact <- pb210_crs(
    accumulation$cumulative_dry_mass,
    accumulation$activity,
    inventory = accumulation$inventory
  ) %>%
    predict()

  expect_ages_similar(crs_model_exact$age, accumulation$age, max_delta = 3)

  # a less perfect world: a core with varying sedimentation rate
  # the best this gets is 12 years in the last 100 (with the defaults)
  core$cumulative_dry_mass <- pb210_cumulative_mass(core$slice_mass)
  crs_model <- pb210_crs(
    core$cumulative_dry_mass,
    set_errors(
      core$activity_estimate,
      core$activity_se
    )
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
  df$cumulative_dry_mass <- pb210_cumulative_mass(df$slice_mass_g / 1000 / core_area, 0.5)

  ages <- pb210_crs(
    df$cumulative_dry_mass,
    df$excess_pb210,
    inventory = pb210_inventory_calculator(model_bottom = 0)
  ) %>%
    predict()

  expect_identical(
    is.na(ages$age),
    is.na(
      c(1.67829, 4.51052, 8.03009, 12.35901, 25.47662, 41.50958, 61.13913,
        83.19123, 127.22231, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  )

  expect_ages_similar(
    ages$age[1:9],
    c(1.64582, 4.47804, 7.99762, 12.32654, 25.44415, 41.47711, 61.10666, 83.15876, 127.18984),
    max_delta = 0.0001
  )

  expect_identical(
    is.na(ages$age_sd),
    is.na(
      c(1.80809, NA, 1.77502, 1.71571, 1.82269, 2.50257, 3.7242, 4.76223,
        10.86489, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
        NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    )
  )

  expect_ages_similar(
    ages$age_sd[c(1, 3:9)],
    c(1.80759, 1.77348, 1.7134, 1.81844, 2.49763, 3.71937, 4.75712, 10.86148),
    max_delta = 0.0001
  )
})

test_that("dating works on real lead-210 data", {

  # from counts data
  # precision_sd <- 13.382
  precision_sd <- 0

  # load core data
  al_core <- alta_lake_pb210[alta_lake_pb210$depth_cm < 17, ]
  al_core$cumulative_dry_mass <- pb210_cumulative_mass(al_core$slice_mass_g) / 1000

  # plot(al_core$depth_cm, al_core$total_pb210_Bq_kg, type = "l")

  # assign background
  al_core$is_background <- al_core$depth_cm > 10
  background <- mean(al_core$total_pb210_Bq_kg[al_core$is_background])
  background_sd <- sd(al_core$total_pb210_Bq_kg[al_core$is_background])

  al_core$excess_pb210 <- pb210_excess(
    set_errors(al_core$total_pb210_Bq_kg, al_core$total_pb210_sd) + set_errors(0, precision_sd),
    set_errors(background, background_sd)
  )

  # plot(al_core$depth_cm, al_core$excess_pb210, type = "l")

  # the published dating of this core assumed no deep inventory
  # and top inventory at the top of the core
  alta_lake_inv <- pb210_inventory(
    al_core$cumulative_dry_mass,
    al_core$excess_pb210,
    model_bottom = pb210_fit_exponential_zero()
  )

  # plot(al_core$depth_cm, alta_lake_inv, type = "l")
  # lines(al_core$depth_cm, drop_errors(alta_lake_inv) + errors(alta_lake_inv), col = "red")
  # lines(al_core$depth_cm, drop_errors(alta_lake_inv) - errors(alta_lake_inv), col = "red")

  al_crs <- pb210_crs(
    al_core$cumulative_dry_mass,
    al_core$excess_pb210,
    inventory = alta_lake_inv
  ) %>%
    predict()

  # plot(al_core$depth_cm, al_crs$age, type = "l")
  # lines(al_core$depth_cm, al_crs$age + al_crs$age_sd, col = "red")
  # lines(al_core$depth_cm, al_crs$age - al_crs$age_sd, col = "red")

  # at the moment, within 7 years is where we're at
  expect_ages_similar(al_crs$age, al_core$published_age_yr, 7, na.rm = TRUE)
})
