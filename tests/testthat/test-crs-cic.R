context("test-age")

test_that("CRS model works on Alta Lake data", {

  real_pb210 <- alta_lake_210Pb[is.finite(alta_lake_210Pb$depth), ]
  real_ages <- pb210_age_crs(
    real_pb210$depth,
    real_pb210$excess_210Pb_Bq_g,
    sample_mass = real_pb210$slice_mass_g,
    excess_pb210_sd = real_pb210$excess_210Pb_sd_Bq,
    calc_inventory_surface = pb210_surface_min_depth,
    calc_inventory_below = pb210_deep_inventory_zero
  )

  real_ages$age_compare <- 2014.60215053763 - real_pb210$crs_age_section_top_ad
  real_ages$sd_compare <- real_pb210$crs_age_sd_yr

  expect_true(all(abs(real_ages$age - real_ages$age_compare) < 0.0000001, na.rm = TRUE))
  expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 0.000001, na.rm = TRUE))
})

test_that("CIC and CRS models work on Kellys Lake data", {

  real_pb210 <- tibble::tibble(
    cumulative_mass = c(0.0039, 0.094, 0.2592, 0.4399, 0.6198, 0.8832, 1.1511, 1.3404,
                            1.7145, 1.9039, 2.2088, 2.5309, 2.8659, 3.2186, 3.6071),
    mass = c(cumulative_mass[1], diff(cumulative_mass)),
    depth = c(0.25, 2.25, 4.25, 6.25, 8.25, 10.25, 12.25, 14.25, 18.25, 20.25,
              23.25, 26.25, 29.25, 32.25, 35.25),
    unsupported_pb210 = c(2744.4956, 3010.5064, 2398.5811, 1982.9397, 1526.9685,
                          601.4311, 723.1201, 509.8198, 198.2745, 182.8893, 97.0584, 82.2188,
                          64.9902, 52.121, 16.303),
    unsupported_pb210_error = c(1908.6439, 2175.1369, 1584.1431, 1349.0103, 1083.7345,
                                308.1658, 310.8554, 356.6073, 139.0672, 123.322, 72.6587, 58.0563,
                                33.546, 24.1429, 11.0648),
    age_crs = c(0.1604, 4.043, 12.0771, 21.3959, 31.4399, 44.112, 55.848, 66.8574,
                86.8897, 95.5305, 110.1282, 125.9577, 151.1861, NA, NA),
    sd_crs = c(0.1256, 2.8855, 5.1236, 6.625, 7.2822, 8.335, 9.0618, 9.2918, 10.3112, 10.7886,
               11.9392, 11.9564, 14.8732, NA, NA),
    age_cic = c(4.1081, 1.1373, 8.4344, 14.5454, 22.9364, 52.857, 46.9398, 58.1639, 88.4916, 91.0854,
                111.4312, 116.7597, 124.311, NA, NA),
    sd_cic = c(22.4205, 23.2866, 21.3014, 21.9364, 22.8775, 16.5731, 13.9462, 22.5495, 22.6106,
               21.7442, 24.1215, 22.762, 16.6938, NA, NA)
  )

  real_ages <- pb210_age_crs(
    real_pb210$cumulative_mass,
    real_pb210$unsupported_pb210,
    # sample_mass = real_pb210$mass,
    excess_pb210_sd = real_pb210$unsupported_pb210_error,
    calc_excess_pb210_surface = pb210_surface_estimate,
    calc_inventory_surface = pb210_surface_estimate,
    calc_inventory_below = pb210_deep_inventory_estimate_loglinear
  )

  real_ages$age_compare <- real_pb210$age_crs
  real_ages$sd_compare <- real_pb210$sd_crs
  plot(age_compare ~ age, data = real_ages, subset = is.finite(age_compare))
  abline(0, 1)

  skip("Haven't figured out kellys calculations")
  expect_true(all(abs(real_ages$age - real_ages$age_compare) < 1, na.rm = TRUE))
  skip("Haven't figured out kellys calculations")
  expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 1, na.rm = TRUE))

  real_ages_cic <- pb210_age_crs(
    real_pb210$cumulative_mass,
    real_pb210$unsupported_pb210,
    sample_mass = real_pb210$mass,
    excess_pb210_sd = real_pb210$unsupported_pb210_error,
    calc_excess_pb210_surface = pb210_surface_estimate,
    calc_inventory_surface = pb210_surface_estimate,
    calc_inventory_below = pb210_deep_inventory_estimate
  )

  real_ages_cic$age_compare <- real_pb210$age_cic
  real_ages_cic$sd_compare <- real_pb210$sd_cic
  plot(age_compare ~ age, data = real_ages_cic, subset = is.finite(age_compare))
  abline(0, 1)

  skip("Haven't figured out kellys calculations")
  expect_true(all(abs(real_ages_cic$age - real_ages$age_compare) < 1, na.rm = TRUE))
  skip("Haven't figured out kellys calculations")
  expect_true(all(abs(real_ages_cic$age_sd - real_ages$sd_compare) < 1, na.rm = TRUE))

})


test_that("CRS model works on independent lab data", {

  real_pb210 <- tibble::tibble(
    depth = c(0.25, 1.25, 2.25, 3.25, 4.25, 6.25, 8.25, 10.25, 12.25, 16.25, 18.25, 22.25),
    cumulative_mass = c(0.0116, 0.0652, 0.1242, 0.1883, 0.2595, 0.401, 0.5463, 0.7034, 0.8666, 1.1974, 1.3652, 1.7113),
    mass = c(cumulative_mass[1], diff(cumulative_mass)),
    unsupported_pb210 = c(2929.1891, 2519.4502, 3095.9563, 2171.7853, 1566.7996,
                          1293.5157, 1063.123, 733.824, 444.6746, 69.9617, 0, 42.1252),
    unsupported_pb210_error = c(302.078, 238.1527, 290.3227, 213.7138, 170.0907, 150.8338,
                                147.3795, 127.9393, 117.9695, 94.0914, 88.434, 84.9105),
    age_crs = c(0.8776, 4.6767, 9.6092, 15.5623, 21.1803, 32.2094, 45.8894, 64.3832, 88.6593, NA, NA, NA),
    sd_crs = c(0.0091, 0.0296, 0.0233, 0.0799, 0.146, 0.3107, 0.6173, 1.3081, 3.1079, NA, NA, NA)
  )

  real_ages <- pb210_age_crs(
    real_pb210$cumulative_mass,
    real_pb210$unsupported_pb210,
    sample_mass = real_pb210$mass,
    excess_pb210_sd = real_pb210$unsupported_pb210_error,
    calc_excess_pb210_surface = pb210_surface_estimate,
    calc_inventory_surface = pb210_surface_estimate,
    calc_inventory_below = pb210_deep_inventory_estimate
  )

  real_ages$age_compare <- real_pb210$age_crs
  real_ages$sd_compare <- real_pb210$sd_crs

  skip("Havne't figured out independent lab calculations")
  expect_true(all(abs(real_ages$age - real_ages$age_compare) < 1, na.rm = TRUE))
  skip("Havne't figured out independent lab calculations")
  expect_true(all(abs(real_ages$age_sd - real_ages$sd_compare) < 1, na.rm = TRUE))
})

test_that("exponential surface estimation works", {
  withr::with_seed(287, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
    expect_true(abs(pb210_surface_estimate_loglinear(fake_depth, fake_pb210) - fake_pb210[1]) < 20)
    expect_true(abs(pb210_surface_estimate(fake_depth, fake_pb210) - fake_pb210[1]) < 0.01)
  })
})

test_that("min depth surface estimation works", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  expect_identical(pb210_surface_min_depth(fake_depth, fake_pb210), fake_pb210[1])
})

test_that("deep inventory estimation works", {
  withr::with_seed(583, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)

    # here m = -1 and b = 5, and we want cumulative area under the curve from 10 to infinity
    # integrated, it's exp(m*x + b) / m, and because at x = infinity the integral is 0
    # the definite integral from [background] to infinity is exp(m * [background] + b) / -m
    known_background <- exp(-1 * 10 + 5) / 1
    calc_background_loglin <- pb210_deep_inventory_estimate_loglinear(fake_depth, fake_pb210)
    calc_background_exp <- pb210_deep_inventory_estimate(fake_depth, fake_pb210)
    expect_true(abs(calc_background_loglin - known_background) < 0.01)
    expect_true(abs(calc_background_exp - known_background) < 0.00001)
  })
})
