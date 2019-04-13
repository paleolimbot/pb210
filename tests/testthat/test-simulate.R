context("test-simulate")

test_that("simulators returns tibbles", {
  expect_is(pb210_simulate_accumulation(), "tbl_df")
  expect_is(pb210_simulate_core(), "tbl_df")
})

test_that("age and depth steps are contiguous", {
  accumulation_sim <- pb210_simulate_accumulation(max_age = 100, time_step = 2)
  expect_true(all(diff(accumulation_sim$age_top) == -2))
  expect_true(all(diff(accumulation_sim$age_bottom) == -2))
  expect_true(all(diff(accumulation_sim$age) == -2))
  expect_equal(max(accumulation_sim$age_bottom), 100)
  expect_equal(min(accumulation_sim$age_top), 0)

  core_sim <- pb210_simulate_core(depth_step = rep(1, 30))
  expect_true(all(diff(core_sim$depth_top) == 1))
  expect_true(all(diff(core_sim$depth_bottom) == 1))
  expect_equal(max(core_sim$depth_bottom), 30)
  expect_equal(min(core_sim$depth_top), 0)
})

test_that("accumulation simulation for constant rate of supply works", {
  crs_sim <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_rnorm_trend())

})

test_that("accumulation simulation for constant initial concentration works", {
  cic_sim <- pb210_simulate_accumulation(mass_accumulation = pb210_mass_accumulation_constant())
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
