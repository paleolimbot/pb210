
#' Fit an exponential model
#'
#' Fits the exponential model (`y ~ exp(m * x + b)`), estimating parameters `m` and `b`
#' using [stats::nls()].
#' The log-linear version fits the model `log(y) ~ x` using [stats::lm()],
#'  where the y-intercept is `b`
#' and the slope of the line is `m`. The log-linear version overestimates
#' the importance of small values but is widely used. NA and zero values
#' are removed observation-wise prior to fitting.
#'
#' @param x An independent variable like depth or cumulative dry mass.
#' @param y A dependent variable that responds exponentially to `x`.
#' @param newdata A tibble with a column `x`.
#' @param object A model fit object.
#' @param ... Not used.
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
#' tibble::tibble(
#'   new_depth = 0:5,
#'   fitted_exp = predict(fit_exp, newdata = tibble::tibble(x = new_depth)),
#'   fitted_loglin = predict(fit_loglinear, newdata = tibble::tibble(x = new_depth))
#' )
#'
pb210_fit_exponential <- function(x, y) {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  # the nonlinear least squares function often fails when coefficients do not make sense
  # using a loglinear fit to find the starting place is one way around this
  fit_loglinear <- pb210_fit_loglinear(x, y)
  coeffs_loglinear <- stats::coefficients(fit_loglinear)
  names(coeffs_loglinear) <- c("b", "m")

  stats::nls(y ~ exp(m * x + b), start = as.list(coeffs_loglinear), na.action = stats::na.omit)
}

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_loglinear <- function(x, y) {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  fit <- stats::lm(log(y) ~ x, na.action = stats::na.omit)
  coeffs <- stats::coefficients(fit)
  names(coeffs) <- c("b", "m")
  coeffs

  class(fit) <- c("lm_loglinear", class(fit))
  fit
}

#' @importFrom stats predict
#' @rdname pb210_fit_exponential
#' @export
predict.lm_loglinear <- function(object, newdata, ...) {
  class(object) <- "lm"
  stopifnot(is.data.frame(newdata), "x" %in% colnames(newdata))
  exp(unname(stats::predict(object, newdata)))
}
