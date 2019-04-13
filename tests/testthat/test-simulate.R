context("test-simulate")

test_that("simulators returns tibbles", {
  expect_is(pb210_simulate_accumulation(), "tbl_df")
  expect_is(pb210_simulate_core(), "tbl_df")
})

test_that("age and depth steps are contiguous for pb210_simulate_accumulation", {
  accumulation_sim <- pb210_simulate_accumulation(
    max_age = 100, time_step = 2,
    compressibility = ~0, initial_density = ~150, mass_accumulation = ~0.15
  )
  expect_true(all(diff(accumulation_sim$age_top) == 2))
  expect_true(all(diff(accumulation_sim$age_bottom) == 2))
  expect_true(all(diff(accumulation_sim$age) == 2))
  expect_equal(max(accumulation_sim$age_bottom), 100)
  expect_equal(min(accumulation_sim$age_top), 0)
  expect_equal(
    accumulation_sim$age_bottom[-nrow(accumulation_sim)],
    accumulation_sim$age_top[-1]
  )

  expect_true(all(abs(diff(accumulation_sim$depth_top) - 0.2) < 1e-9))
  expect_true(all(abs(diff(accumulation_sim$depth_bottom) - 0.2) < 1e-9))
  expect_true(all(abs(diff(accumulation_sim$depth) - 0.2) < 1e-9))
  expect_equal(min(accumulation_sim$depth_top), 0)
  expect_equal(
    accumulation_sim$depth_bottom[-nrow(accumulation_sim)],
    accumulation_sim$depth_top[-1]
  )

})

test_that("age and depth steps are contiguous for pb210_simulate_core (no compression)", {
  core_sim <- pb210_simulate_core(
    pb210_simulate_accumulation(compressibility = ~0, initial_density = ~150, mass_accumulation = ~0.15),
    depth_step = rep(1, 30)
  )

  expect_true(all(diff(core_sim$age_top) == 10))
  expect_true(all(diff(core_sim$age_bottom) == 10))
  expect_true(all(abs(diff(core_sim$age) - 10) < 1e-9))
  expect_equal(min(core_sim$age_top), 0)
  expect_equal(
    core_sim$age_bottom[-nrow(core_sim)],
    core_sim$age_top[-1]
  )

  expect_true(all(diff(core_sim$depth_top) == 1))
  expect_true(all(diff(core_sim$depth_bottom) == 1))
  expect_equal(max(core_sim$depth_bottom), 30)
  expect_equal(min(core_sim$depth_top), 0)

  expect_equal(
    core_sim$depth_bottom[-nrow(core_sim)],
    core_sim$depth_top[-1]
  )
})

test_that("age and depth steps are contiguous for pb210_simulate_core (with compression)", {
  core_sim <- pb210_simulate_core(
    pb210_simulate_accumulation(compressibility = ~1e-3, initial_density = ~150, mass_accumulation = ~0.15),
    depth_step = rep(1, 30)
  )

  expect_equal(
    core_sim$age_bottom[-nrow(core_sim)],
    core_sim$age_top[-1]
  )

  expect_equal(
    core_sim$depth_bottom[-nrow(core_sim)],
    core_sim$depth_top[-1]
  )
})

test_that("accumulation simulation for constant rate of supply works", {
  crs_sim <- withr::with_seed(433, {
    pb210_simulate_accumulation(
      mass_accumulation = pb210_mass_accumulation_rlnorm(sd = 0.1)
    )
  })

  # the CRS model should work here for the last 150 years
  crs_sim$inventory <- rev(cumsum(rev(crs_sim$pb210_specific_activity * crs_sim$slice_mass)))
  inventory_surface <- max(crs_sim$inventory)
  crs_sim$crs_age_top <- 1 / pb210_decay_constant() * log(inventory_surface / crs_sim$inventory)
  expect_true(
    all(abs(crs_sim$crs_age_top[crs_sim$age < 100] - crs_sim$age_top[crs_sim$age < 100]) < 1)
  )
})

test_that("accumulation simulation for constant initial concentration works", {
  # params chosen such that the surface activity is 1000 Bq/kg
  cic_sim <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant(0.10),
    supply = pb210_supply_constant(100),
    compressibility = ~0
  )

  # the CIC model to calculate ages should work exactly
  expect_equal(
    1 / pb210_decay_constant() * log(1000 / cic_sim$pb210_specific_activity),
    cic_sim$age
  )

  # the CRS model should approximately work here for the last 100 years
  cic_sim$inventory <- rev(cumsum(rev(cic_sim$pb210_specific_activity * cic_sim$slice_mass)))
  inventory_surface <- max(cic_sim$inventory)
  cic_sim$crs_age_top <- 1 / pb210_decay_constant() * log(inventory_surface / cic_sim$inventory)

  expect_true(
    all(abs(cic_sim$crs_age_top[cic_sim$age < 100] - cic_sim$age_top[cic_sim$age < 100]) < 1e-3)
  )
})

test_that("core simulation for constant rate of supply works", {
  crs_sim <- withr::with_seed(433, {
    pb210_simulate_accumulation(
      mass_accumulation = pb210_mass_accumulation_rlnorm(sd = 0.1)
    ) %>%
      pb210_simulate_core()
  })

  # the CRS model should work here for the last 100 years
  crs_sim$inventory <- rev(cumsum(rev(crs_sim$pb210_specific_activity * crs_sim$slice_mass)))
  inventory_surface <- max(crs_sim$inventory)
  crs_sim$crs_age_top <- 1 / pb210_decay_constant() * log(inventory_surface / crs_sim$inventory)
  expect_true(
    all(abs(crs_sim$crs_age_top[crs_sim$age < 100] - crs_sim$age_top[crs_sim$age < 100]) < 0.01)
  )
})

test_that("core simulation for constant initial concentration works", {
  # params chosen such that the surface activity is 1000 Bq/kg
  cic_sim <- pb210_simulate_accumulation(
    mass_accumulation = pb210_mass_accumulation_constant(),
    compressibility = ~0
  ) %>%
    pb210_simulate_core()

  # the CIC model to calculate ages should work exactly for the last 100 years
  pb210_surface <- max(cic_sim$pb210_specific_activity)
  cic_sim$cic_age_top <- 1 / pb210_decay_constant() * log(pb210_surface / cic_sim$pb210_specific_activity)
  expect_equal(
    cic_sim$cic_age_top[cic_sim$age < 100],
    cic_sim$age_top[cic_sim$age < 100]
  )

  # the CRS model should approximately work here for the last 100 years
  cic_sim$inventory <- rev(cumsum(rev(cic_sim$pb210_specific_activity * cic_sim$slice_mass)))
  inventory_surface <- max(cic_sim$inventory)
  cic_sim$crs_age_top <- 1 / pb210_decay_constant() * log(inventory_surface / cic_sim$inventory)

  expect_true(
    all(abs(cic_sim$crs_age_top[cic_sim$age < 100] - cic_sim$age_top[cic_sim$age < 100]) < 1)
  )
})

test_that("core simulation calculates identical values when slices are identical to simulation", {
  # a 1 g per year with 100 kg / m^3^ density and 0 compressibility means
  # 1 cm per year
  sim <- pb210_simulate_accumulation(
    compressibility = ~0, mass_accumulation = ~1, initial_density = ~100,
    max_age = 300, time_step = 1
  )

  # this core is designed to sample sim at exactly the same intervals that it
  # already contains, with the same core area
  core_sim <- pb210_simulate_core(sim, depth_step = rep(1, 300), core_area = 1)

  expect_identical(
    sim[order(sim$depth), ],
    core_sim[order(core_sim$depth), ],
  )
})

test_that("core simulation works with default parameters", {
  core_sim <- pb210_simulate_core()
  expect_equal(
    core_sim$age_bottom[-nrow(core_sim)],
    core_sim$age_top[-1]
  )

  expect_equal(
    core_sim$depth_bottom[-nrow(core_sim)],
    core_sim$depth_top[-1]
  )
})

test_that("parameter generators return functions that are length stable", {

  test_parameter_generator <- function(gen, ages = 300:0) {
    gen_label <- deparse(substitute(gen))
    gen <- rlang::as_function(gen)
    expect_is(gen(), "function", info = gen_label)
    expect_true(length(gen()(ages)) == length(ages) || length(gen()(ages)) == 1, info = gen_label)
  }

  test_parameter_generator(pb210_supply_constant)

  test_parameter_generator(pb210_mass_accumulation_constant)
  test_parameter_generator(pb210_mass_accumulation_rlnorm)

  test_parameter_generator(pb210_density_constant)

  test_parameter_generator(pb210_compressibility_constant)
})

test_that("rlang lambda syntax is accepted for all functional arguments", {
  expect_silent(
    pb210_simulate_accumulation(
      mass_accumulation = ~1,
      supply = ~1,
      compressibility = ~1,
      initial_density = ~1
    )
  )
})
