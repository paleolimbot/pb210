context("test-age")

test_that("CRS model works on Alta Lake data", {

  real_pb210 <- alta_lake_210Pb[is.finite(alta_lake_210Pb$depth), ]
  real_ages <- pb210_age_crs(
    real_pb210$depth,
    real_pb210$excess_210Pb_Bq_g,
    sample_mass = real_pb210$slice_mass_g,
    excess_pb210_sd = real_pb210$excess_210Pb_sd_Bq,
    calc_inventory_surface = pb210_surface_min_depth
  )

  real_ages$age_compare <- 2014.60215053763 - real_pb210$crs_age_section_top_ad
  real_ages$sd_compare <- real_pb210$crs_age_sd_yr

  expect_true(all(abs(real_ages$age - real_ages$age_compare) < 0.0000001, na.rm = TRUE))
  expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 0.000001, na.rm = TRUE))
})

# have no idea how Blais lab does its Pb210 estimation
# test_that("CRS model works on Blais lab data", {
#
#   real_pb210 <- tibble::tibble(
#     depth = c(0.25, 1.25, 2.25, 3.25, 4.25, 6.25, 8.25, 10.25, 12.25, 16.25, 18.25, 22.25),
#     cumulative_mass = c(0.0116, 0.0652, 0.1242, 0.1883, 0.2595, 0.401, 0.5463, 0.7034, 0.8666, 1.1974, 1.3652, 1.7113),
#     mass = c(cumulative_mass[1], diff(cumulative_mass)),
#     unsupported_pb210 = c(2929.1891, 2519.4502, 3095.9563, 2171.7853, 1566.7996,
#                          1293.5157, 1063.123, 733.824, 444.6746, 69.9617, 0, 42.1252),
#     unsupported_pb210_error = c(302.078, 238.1527, 290.3227, 213.7138, 170.0907, 150.8338,
#                     147.3795, 127.9393, 117.9695, 94.0914, 88.434, 84.9105),
#     supported_pb210 = c(58.3385, 31.5886, 0, 49.976, 37.8864, 7.9291, 60.8241,
#                         55.5902, 35.5882, 38.1487, 50.4169, 47.5013),
#     supported_pb210_error = c(58.3385, 31.5886, 0, 49.976, 37.8864, 7.9291, 60.8241, 55.5902,
#                               35.5882, 38.1487, 50.4169, 47.5013),
#     age_crs = c(0.8776, 4.6767, 9.6092, 15.5623, 21.1803, 32.2094, 45.8894, 64.3832, 88.6593, NA, NA, NA)
#   )
#
#   real_ages <- pb210_age_crs(
#     real_pb210$cumulative_mass,
#     real_pb210$unsupported_pb210,
#     sample_mass = mass,
#     calc_excess_pb210_surface = pb210_surface_min_depth
#   )
#
#   real_ages$age_compare <- 2014.60215053763 - real_pb210$crs_age_section_top_ad
#   real_ages$sd_compare <- real_pb210$crs_age_sd_yr
#
#   expect_true(all(abs(real_ages$age - real_ages$age_compare) < 0.0000001, na.rm = TRUE))
#   expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 0.000001, na.rm = TRUE))
# })

test_that("exponential surface estimation works", {
  withr::with_seed(287, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.01)
    expect_true(abs(pb210_surface_estimate(fake_depth, fake_pb210) - fake_pb210[1]) < 0.1)
  })
})

test_that("min depth surface estimation works", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  expect_identical(pb210_surface_min_depth(fake_depth, fake_pb210), fake_pb210[1])
})

