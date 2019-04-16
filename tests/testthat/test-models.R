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
    expect_true(all(abs(log(fitted_pb210) - log(fake_pb210)) < 0.3))

    # coefficients should be "b" = 5 and "m" = -1
    expect_identical(names(coefficients(fit)), c("b", "m"))
    expect_true(all(abs(coefficients(fit) - c(5, -1)) < 0.01))
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
