
#' Fit an exponential model
#'
#' Fits the exponential model (`y ~ exp(m * x + b)`), estimating parameters `m`
#' and `b` using [stats::nls()]. The log-linear version fits the model `log(y) ~ x`
#' using [stats::lm()], where the y-intercept is `b` and the slope of the
#' line is `m`. The log-linear version overestimates the importance of small
#' values but is widely used. NA and zero values are removed observation-wise
#' prior to fitting.
#'
#' @param x An independent variable like depth or cumulative dry mass.
#' @param y A dependent variable that responds exponentially to `x`.
#' @param newdata A tibble with a column `x`.
#' @param object A model fit object.
#' @param ... Not used.
#' @param m,b Directly specify coefficients for a manual fit.
#'
#' @return A model object like that returned by [stats::nls()], with a
#'   [stats::predict()] method.
#' @export
#'
#' @examples
#' fake_depth <- 0:10
#' fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
#'
#' fit_exp <- pb210_fit_exponential(fake_depth, fake_pb210)
#' fit_loglinear <- pb210_fit_loglinear(fake_depth, fake_pb210)
#'
#' coefficients(fit_exp)
#' coefficients(fit_loglinear)
#'
#' tibble::tibble(
#'   new_depth = 0:5,
#'   fitted_exp = predict(fit_exp, newdata = tibble::tibble(x = new_depth)),
#'   fitted_loglin = predict(fit_loglinear, newdata = tibble::tibble(x = new_depth))
#' )
#'
pb210_fit_exponential <- function(x, y) {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  # the nonlinear least squares function often fails when initial
  # coefficients do not make sense
  # using a loglinear fit to find the starting place is one way around this
  fit_loglinear <- pb210_fit_loglinear(x, y)
  coeffs_loglinear <- stats::coefficients(fit_loglinear)

  stats::nls(
    y ~ exp(m * x + b),
    start = as.list(coeffs_loglinear),
    na.action = stats::na.omit
  )
}

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_loglinear <- function(x, y) {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  fit <- stats::lm(log(y) ~ x, na.action = stats::na.omit)

  class(fit) <- c("lm_loglinear", class(fit))
  fit
}

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_exponential_manual <- function(m, b) {
  stopifnot(
    is.numeric(m), length(m) == 1,
    is.numeric(b), length(b) == 1
  )

  structure(list(coef = c("b" = unname(b), "m" = unname(m))), class = "exponential_manual")
}

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_exponential_zero <- function() {
  pb210_fit_exponential_manual(1, -Inf)
}

#' @importFrom stats predict
#' @rdname pb210_fit_exponential
#' @export
predict.exponential_manual <- function(object, newdata, ...) {
  stopifnot(
    is.data.frame(newdata),
    "x" %in% colnames(newdata),
    is.numeric(newdata$x)
  )

  coeffs <- stats::coefficients(object)
  exp(coeffs["m"] * newdata$x + coeffs["b"])
}

#' @importFrom stats coef
#' @rdname pb210_fit_exponential
#' @export
coef.exponential_manual <- function(object, ...) {
  object$coef
}

#' @importFrom stats predict
#' @rdname pb210_fit_exponential
#' @export
predict.lm_loglinear <- function(object, newdata, ...) {
  stopifnot(
    is.data.frame(newdata),
    "x" %in% colnames(newdata),
    is.numeric(newdata$x)
  )

  class(object) <- "lm"
  exp(unname(stats::predict(object, newdata)))
}

#' @importFrom stats coef
#' @rdname pb210_fit_exponential
#' @export
coef.lm_loglinear <- function(object, ...) {
  lm_coeffs <- NextMethod()
  names(lm_coeffs) <- c("b", "m")
  lm_coeffs
}


#' Fit an interpolator
#'
#' @param x,y Numeric vectors of known values, where `x` is the
#'   "known" variable and `y` is the variable that must be interpolated
#'   (between unknown `x` values).
#'
#' @return An object of class "interpolator".
#' @export
#'
#' @examples
#' fake_depth <- 0:10
#' fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
#' fit_interp <- pb210_fit_interpolator_linear(fake_depth, fake_pb210)
#' predict(fit_interp, newdata = tibble::tibble(x = seq(-1, 11, by = 0.5)))
#'
pb210_fit_interpolator_linear <- function(x, y) {
  stopifnot(
    is.numeric(x),
    is.numeric(y),
    sum(is.finite(x) & is.finite(y)) >= 2
  )

  tbl <- tibble::tibble(x, y)
  tbl <- tbl[is.finite(x) & is.finite(y), ]
  tbl <- tbl[order(tbl$x), ]

  structure(list(coords = tbl), class = "linear_interpolator")
}

#' @importFrom stats predict
#' @rdname pb210_fit_exponential
#' @export
predict.linear_interpolator <- function(object, newdata, ...) {
  stopifnot(
    is.data.frame(newdata),
    "x" %in% colnames(newdata),
    is.numeric(newdata$x)
  )

  stats::approx(object$coords, xout = newdata$x)$y
}
