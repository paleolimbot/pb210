context("test-real_cores")

test_that("dating works on real lead-210 data", {
  withr::with_par(list(mfrow = c(2, 2)), {

    # from counts data
    # precision_sd <- 13.382
    precision_sd <- 0

    # load core data
    al_core <- alta_lake_pb210[alta_lake_pb210$depth_cm < 17, ]
    al_core$cumulative_dry_mass <- pb210_cumulative_mass(al_core$slice_mass_g) / 1000

    plot(al_core$depth_cm, al_core$total_pb210_Bq_kg, type = "l")

    # assign background
    al_core$is_background <- al_core$depth_cm > 8
    background <- mean(al_core$total_pb210_Bq_kg[al_core$is_background])
    background_sd <- sd(al_core$total_pb210_Bq_kg[al_core$is_background])

    al_core$excess_pb210 <- pb210_excess(
      set_errors(al_core$total_pb210_Bq_kg, al_core$total_pb210_sd) + set_errors(0, precision_sd),
      set_errors(background, background_sd)
    )

    plot(al_core$depth_cm, al_core$excess_pb210, type = "l")

    # the published dating of this core assumed no deep inventory
    # and top inventory at the top of the core
    alta_lake_inv <- pb210_inventory(
      al_core$cumulative_dry_mass,
      al_core$excess_pb210,
      model_bottom = pb210_fit_exponential_zero()
    )

    plot(al_core$depth_cm, alta_lake_inv, type = "l")
    lines(al_core$depth_cm, drop_errors(alta_lake_inv) + errors(alta_lake_inv), col = "red")
    lines(al_core$depth_cm, drop_errors(alta_lake_inv) - errors(alta_lake_inv), col = "red")

    al_crs <- pb210_age_crs(
      al_core$cumulative_dry_mass,
      al_core$excess_pb210,
      inventory = alta_lake_inv,
      model_top = max(alta_lake_inv, na.rm = TRUE)
    )

    plot(al_core$depth_cm, al_crs$age, type = "l")
    lines(al_core$depth_cm, al_crs$age + al_crs$age_sd, col = "red")
    lines(al_core$depth_cm, al_crs$age - al_crs$age_sd, col = "red")

    # at the moment, within 8 years is where we're at
    expect_ages_similar(al_crs$age, al_core$published_age_yr, 8, na.rm = TRUE)
    plot(al_core$depth_cm, al_crs$age - al_core$published_age_yr)
  })
})
