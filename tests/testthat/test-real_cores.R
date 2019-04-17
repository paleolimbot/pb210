context("test-real_cores")

test_that("dating works on real lead-210 data", {
  withr::with_par(list(mfrow = c(2, 2)), {

    al_standards <- alta_lake_210Pb[!is.finite(alta_lake_210Pb$slice_mass_g), ]

    # calculate pb210 precision
    precision_sd <- sd(al_standards$activity_210Pb_Bq_g * 1000)

    # load core data
    al_core <- alta_lake_210Pb[is.finite(alta_lake_210Pb$slice_mass_g), ]
    al_core$cumulative_dry_mass <- pb210_cumulative_mass(al_core$slice_mass_g) / 1000
    al_core$activity_210Pb_Bq_kg <- al_core$activity_210Pb_Bq_g * 1000
    al_core$activity_210Pb_sd <- al_core$activity_210Pb_Bq_kg * al_core$activity_210Pb_sd_percent / 100

    plot(al_core$cumulative_dry_mass, al_core$activity_210Pb_Bq_kg, type = "l")

    # assign background
    al_core$is_background <- al_core$depth > 8
    background <- mean(al_core$activity_210Pb_Bq_kg[al_core$is_background])
    background_sd <- sd(al_core$activity_210Pb_Bq_kg[al_core$is_background])

    al_core$excess_pb210 <- pb210_excess(
      set_errors(al_core$activity_210Pb_Bq_kg, al_core$activity_210Pb_sd) + set_errors(0, precision_sd),
      set_errors(background, background_sd)
    )

    plot(al_core$depth, al_core$excess_pb210, type = "l")

    # the published dating of this core assumed no deep inventory
    # and top inventory at the top of the core
    alta_lake_inv <- pb210_inventory(
      al_core$cumulative_dry_mass,
      al_core$excess_pb210,
      model_bottom = pb210_fit_exponential_zero()
    )

    plot(al_core$depth, alta_lake_inv, type = "l")
    lines(al_core$depth, drop_errors(alta_lake_inv) + errors(alta_lake_inv), col = "red")
    lines(al_core$depth, drop_errors(alta_lake_inv) - errors(alta_lake_inv), col = "red")

    al_crs <- pb210_age_crs(
      al_core$cumulative_dry_mass,
      al_core$excess_pb210,
      inventory = alta_lake_inv,
      model_top = max(alta_lake_inv, na.rm = TRUE)
    )

    plot(al_core$depth, al_crs$age, type = "l")
    lines(al_core$depth, al_crs$age + al_crs$age_sd, col = "red")
    lines(al_core$depth, al_crs$age - al_crs$age_sd, col = "red")
  })
})
