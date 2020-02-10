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

test_that("exponential model fits even with perfect data", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  expect_is(pb210_fit_exponential(fake_depth, fake_pb210), "lm_loglinear")
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

test_that("as_fit works as intended with lazy fits", {
  expect_is(pb210_as_fit(~max(..1)), "pb210_fit_lazy")
  expect_is(pb210_as_fit(max), "pb210_fit_lazy")
  expect_is(pb210_as_fit(pb210_as_fit(max)), "pb210_fit_lazy")
})

test_that("lazy fits work as intended", {
  fake_depth <- 0:10
  fake_pb210 <- exp(5 - fake_depth)
  expect_is(pb210_fit_exponential(fake_depth, fake_pb210), "lm_loglinear")

  # referring to objects in the environment
  lazy_fit <- pb210_fit_lazy(~pb210_fit_exponential(fake_depth, fake_pb210))
  expect_is(lazy_fit, "pb210_fit_lazy")
  expect_is(pb210_as_fit(lazy_fit, data = list()), "lm_loglinear")

  lazy_fit2 <- pb210_fit_lazy(~pb210_fit_exponential(.x, .y))
  expect_is(lazy_fit2, "pb210_fit_lazy")
  expect_is(pb210_as_fit(lazy_fit2), "pb210_fit_lazy")
  expect_error(predict(pb210_as_fit(lazy_fit2)), "Lazy fit needs to be resolved")
  expect_is(
    pb210_as_fit(
      lazy_fit2,
      data = tibble::tibble(cumulative_dry_mass = fake_depth, excess_pb210 = fake_pb210)
    ),
    "lm_loglinear"
  )
})

test_that("finite head and tail functions work as expected", {
  x <- 1:5
  y <- c(1, 2, NA, NA, 5)

  expect_identical(finite_tail(x, y), is.finite(y))
  expect_identical(finite_tail(x, y, 0), c(F, F, F, F, F))
  expect_identical(finite_tail(x, y, 1), c(F, F, F, F, T))
  expect_identical(finite_tail(x, y, 2), c(F, T, F, F, T))
  expect_identical(finite_tail(x, y, 3), c(T, T, F, F, T))
  expect_identical(finite_tail(x, y, 4), c(T, T, F, F, T))

  expect_identical(finite_tail_prop(x, y), finite_tail(x, y))
  expect_identical(finite_tail_prop(x, y, 0.5), finite_tail(x, y, 2))

  expect_identical(finite_head(x, y), is.finite(y))
  expect_identical(finite_head(x, y, 0), c(F, F, F, F, F))
  expect_identical(finite_head(x, y, 1), c(T, F, F, F, F))
  expect_identical(finite_head(x, y, 2), c(T, T, F, F, F))
  expect_identical(finite_head(x, y, 3), c(T, T, F, F, T))
  expect_identical(finite_head(x, y, 4), c(T, T, F, F, T))

  expect_identical(finite_head_prop(x, y), finite_head(x, y))
  expect_identical(finite_head_prop(x, y, 0.5), finite_head(x, y, 2))
})

test_that("subset application works as expected", {
  x <- 1:10
  y <- c(1, 2, NA, NA, 5, 6, NA, 8, 9, NA)

  expect_identical(apply_subset(x, y), tibble::tibble(x, y))
  expect_identical(apply_subset(x, y, ~NULL), tibble::tibble(x, y))
  expect_identical(apply_subset(x, y, function(x, y) NULL), tibble::tibble(x, y))
  expect_identical(apply_subset(x, y, 1:10), tibble::tibble(x, y))
  expect_identical(apply_subset(x, y, as.numeric(1:10)), tibble::tibble(x, y))


  expect_identical(apply_subset(x, y, 5:7), tibble::tibble(x, y)[5:7, ])
  expect_identical(apply_subset(x, y, (1:10) %in% (5:7)), tibble::tibble(x, y)[5:7, ])

  # closer to actual usage
  expect_identical(
    apply_subset(x, y, ~finite_tail(..1, ..2, 3)),
    tibble::tibble(x, y)[c(6, 8, 9), ]
  )
})
