context("test-physical_parameters")

test_that("cumulative_mass function works", {
  masses <- withr::with_seed(39, runif(10))
  expect_identical(pb210_cumulative_mass(masses, position = 1), cumsum(masses))
  expect_identical(pb210_cumulative_mass(masses, position = 0), c(0, cumsum(masses[-1])))
})

test_that("density and porosity estimation are correct", {

  # use all known parameters with a toy example

  # 2 cm x 8 cm diameter
  thickness <- 0.02
  core_area <- pb210_core_area(0.08)
  total_volume <- pb210_slice_volume(thickness, core_area)
  porosity <- 0.75

  # use reasonable but non-default densities
  inorganic_density <- 2800
  organic_density <- 1500
  water_density <- 1100

  organic_content <- 0.5
  solid_density <- (inorganic_density + organic_density) / 2

  volume_water <- total_volume * porosity
  volume_solid <- total_volume - volume_water
  mass_water <- water_density * volume_water
  mass_solid <- solid_density * volume_solid

  mass_total <- mass_water + mass_solid

  water_content <- mass_water / mass_total
  bulk_density <- mass_solid / total_volume

  expect_equal(
    pb210_bulk_density_estimate(
      water_content = water_content,
      organic_content = organic_content,
      density_inorganic = inorganic_density,
      density_organic = organic_density,
      density_water = water_density
    ),
    bulk_density
  )

  expect_equal(
    pb210_porosity(
      water_content = water_content,
      organic_content = organic_content,
      density_organic = organic_density,
      density_inorganic = inorganic_density,
      density_water = water_density
    ),
    porosity
  )

  expect_equal(
    pb210_slice_mass(bulk_density = bulk_density, thickness = thickness, core_area = core_area),
    mass_solid
  )

  expect_equal(
    pb210_bulk_density(
      slice_mass = mass_solid,
      thickness = thickness,
      core_area = core_area
    ),
    bulk_density
  )
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
