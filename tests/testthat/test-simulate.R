context("test-simulate")

test_that("parameter generators return functions that are length stable", {

  test_parameter_generator <- function(gen, ages = 300:0) {
    gen_label <- deparse(substitute(gen))
    gen <- rlang::as_function(gen)
    expect_is(gen(), "function", info = gen_label)
    expect_true(length(gen()(ages)) == length(ages) || length(gen()(ages)) == 1, info = gen_label)
  }

  test_parameter_generator(pb210_mass_accumulation_constant)
  test_parameter_generator(pb210_mass_accumulation_rnorm)
  test_parameter_generator(pb210_mass_accumulation_rnorm_trend)

  test_parameter_generator(pb210_water_content_constant)

  test_parameter_generator(pb210_density_constant)

  test_parameter_generator(pb210_compressibility_constant)
})
