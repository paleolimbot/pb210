context("test-models")

test_that("loglinear model fits", {
  withr::with_seed(287, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
    fit <- pb210_fit_loglinear(fake_depth, fake_pb210)

    # loglinear fit has a different class and a different predict method
    expect_is(fit, "lm")
    expect_is(fit, "lm_loglinear")

    # make sure these are close! without the custom predict method,
    # it predicts the log(y) value rather than y.
    fitted_pb210 <- predict(fit, newdata = tibble::tibble(x = fake_depth))
    expect_true(all(abs(log(fitted_pb210) - log(fake_pb210)) < 0.25))

    # coefficients should be "b" = 5 and "m" = -1
    expect_identical(names(coefficients(fit)), c("b", "m"))
    expect_true(all(abs(coefficients(fit) - c(5, -1)) < 0.07))
  })
})

test_that("exponential model fits", {
  withr::with_seed(287, {
    fake_depth <- 0:10
    fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
    fit <- pb210_fit_exponential(fake_depth, fake_pb210)

    fitted_pb210 <- predict(fit, newdata = tibble::tibble(x = fake_depth))
    expect_true(all(abs(log(fitted_pb210) - log(fake_pb210)) < 0.4))

    # coefficients should be "b" = 5 and "m" = -1
    expect_identical(names(coefficients(fit)), c("b", "m"))
    expect_true(all(abs(coefficients(fit) - c(5, -1)) < 0.001))
  })
})

test_that("manual model fits", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  fit <- pb210_fit_exponential_manual(-1, 5)
  expect_identical(predict(fit, tibble::tibble(x = fake_depth)), fake_pb210)
  expect_identical(coefficients(fit), c("b" = 5, "m" = -1))
})

test_that("zero model is zero always", {
  fit <- pb210_fit_exponential_zero()
  expect_equal(
    predict(fit, tibble::tibble(x = seq(-100, 100))),
    rep(0, 201)
  )
})

test_that("constant model is constant always", {
  fit <- pb210_fit_exponential_constant(4)
  expect_equal(
    predict(fit, tibble::tibble(x = seq(-100, 100))),
    rep(4, 201)
  )
})

test_that("fit coersion works as expected", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
  fit <- pb210_fit_exponential(fake_depth, fake_pb210)
  expect_identical(pb210_as_fit(fit), fit)
  expect_identical(pb210_as_fit(0), pb210_fit_exponential_zero())
  expect_identical(pb210_as_fit(4), pb210_fit_exponential_constant(4))
  expect_error(pb210_as_fit("I'm not a fit"), "method for object")
})

test_that("linear interpolator works as intended", {
  known_x <- c(0, 1, 2)
  known_y <- c(10, 12, 16)
  interp_fit <- pb210_fit_interpolator_linear(known_x, known_y)

  expect_identical(predict(interp_fit, tibble::tibble(x = known_x)), known_y)
  expect_identical(predict(interp_fit, tibble::tibble(x = c(0.5, 1.5))), c(11, 14))
})
