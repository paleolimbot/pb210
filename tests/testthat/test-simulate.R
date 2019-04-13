context("test-simulate")

test_that("simulators returns tibbles", {
  expect_is(pb210_simulate_accumulation(), "tbl_df")
  expect_is(pb210_simulate_core(), "tbl_df")
})

test_that("age and depth steps are contiguous", {
  accumulation_sim <- pb210_simulate_accumulation(max_age = 100, time_step = 2)
  expect_true(all(diff(accumulation_sim$age_top) == 2))
  expect_true(all(diff(accumulation_sim$age_bottom) == 2))
  expect_true(all(diff(accumulation_sim$age) == 2))
  expect_equal(max(accumulation_sim$age_bottom), 100)
  expect_equal(min(accumulation_sim$age_top), 0)

  core_sim <- pb210_simulate_core(depth_step = rep(1, 30))
  expect_true(all(diff(core_sim$depth_top) == 1))
  expect_true(all(diff(core_sim$depth_bottom) == 1))
  expect_equal(max(core_sim$depth_bottom), 30)
  expect_equal(min(core_sim$depth_top), 0)
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

test_that("accumulation simulation for constant rate of supply works", {
  crs_sim <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_rnorm_trend())

})

test_that("accumulation simulation for constant initial concentration works", {
  cic_sim <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant())
})

test_that("core simulation works with parameters identical to simulation", {
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

test_that("core simulation works with parameters not identical to simulation", {
  # a 1 g per year with 100 kg / m^3^ density and 0 compressibility means
  # 1 cm per year
  sim <- pb210_simulate_accumulation()
  core_sim <- pb210_simulate_core(sim, depth_step = rep(1, 30))

  # top and bottom ages need to be contiguous
  expect_true(all.equal(core_sim$age_bottom[-nrow(core_sim)], core_sim$age_top[-1]))
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
  test_parameter_generator(pb210_mass_accumulation_rnorm)
  test_parameter_generator(pb210_mass_accumulation_rnorm_trend)

  test_parameter_generator(pb210_density_constant)

  test_parameter_generator(pb210_compressibility_constant)
})
