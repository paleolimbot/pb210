
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
#' @param subset `NULL` to do no subsetting, a logical or numeric
#'   vector to subset manually, or a function of two variables
#'   (`x` and `y`) that returns `NULL`, a logical vector, or
#'   a numeric vector that will be used to subset the data.
#'   This is particularly useful in conjunction with
#'   [finite_head()] or [finite_tail()].
#' @param ... Not used.
#' @param m,b,value Directly specify coefficients for a manual fit.
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
pb210_fit_exponential <- function(x, y, subset = NULL) {
  # these functions do not consider error
  x <- without_errors(x)
  y <- without_errors(y)

  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  # apply subset
  data <- apply_subset(x, y, subset)

  # the nonlinear least squares function often fails when initial
  # coefficients do not make sense
  # using a loglinear fit to find the starting place is one way around this
  fit_loglinear <- pb210_fit_loglinear(data$x, data$y)
  coeffs_loglinear <- stats::coefficients(fit_loglinear)

  # the nls() function also fails when the initial coeficients are
  # perfect, which is the case here with a (nearly) perfect fit
  if(is_perfect_lm_fit(fit_loglinear)) {
    return(fit_loglinear)
  }

  stats::nls(
    y ~ exp(m * x + b),
    start = as.list(coeffs_loglinear),
    na.action = stats::na.omit,
    data = data
  )
}

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_loglinear <- function(x, y, subset = NULL) {
  # these functions do not consider error
  x <- without_errors(x)
  y <- without_errors(y)

  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  # apply subset
  data <- apply_subset(x, y, subset)

  fit <- stats::lm(log(y) ~ x, na.action = stats::na.omit, data = data)

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

#' @rdname pb210_fit_exponential
#' @export
pb210_fit_exponential_constant <- function(value) {
  stopifnot(is.numeric(value), length(value) == 1)
  if(!is.na(value) && value == 0) {
    pb210_fit_exponential_zero()
  } else {
    pb210_fit_exponential_manual(m = 0, b = log(value))
  }
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

is_perfect_lm_fit <- function(fit_obj, epsilon = .Machine$double.eps^0.5) {
  all(abs(stats::residuals(fit_obj)) < epsilon, na.rm = TRUE)
}


#' Coerce objects to exponential fits
#'
#' This is used to sanitize input for [pb210_cic()] and [pb210_crs()],
#' where fit objects are required but where it is anticipated that
#' numeric constants will be common as input.
#'
#' @param x An object
#' @param data For lazy fits, this object is a tibble with
#'   `cumulative_dry_mass` and `excess`.
#' @param ... Unused.
#'
#' @return An object with a [stats::predict()] method.
#' @export
#'
#' @examples
#' fake_depth <- 0:10
#' fake_pb210 <- exp(5 - fake_depth) + rnorm(11, sd = 0.005)
#'
#' pb210_as_fit(pb210_fit_exponential(fake_depth, fake_pb210))
#' pb210_as_fit(1)
#' pb210_as_fit(0)
#'
pb210_as_fit <- function(x, ...) {
  UseMethod("pb210_as_fit")
}

#' @rdname pb210_as_fit
#' @export
pb210_as_fit.default <- function(x, ...) {
  # any S3 with a predict method is OK
  predict <- try(utils::getS3method("predict", class(x), optional = FALSE), silent = TRUE)
  if(inherits(predict, "try-error")) {
    abort(
      paste0(
        "No stats::predict() method for object inheriting ",
        paste(class(x), collapse = " / ")
      )
    )
  }

  x
}

#' @rdname pb210_as_fit
#' @export
pb210_as_fit.numeric <- function(x, ...) {
  pb210_fit_exponential_constant(x)
}

#' @rdname pb210_as_fit
#' @export
pb210_fit_lazy <- function(x) {
  x <- rlang::as_function(x, env = rlang::caller_env())
  structure(list(fun = x), class = "pb210_fit_lazy")
}

#' @rdname pb210_as_fit
#' @export
pb210_as_fit.function <- function(x, ...) {
  pb210_fit_lazy(x)
}

#' @rdname pb210_as_fit
#' @export
pb210_as_fit.formula <- function(x, ...) {
  pb210_fit_lazy(x)
}

#' @rdname pb210_as_fit
#' @export
pb210_as_fit.pb210_fit_lazy <- function(x, data = NULL, ...) {
  if (is.null(data)) {
    x
  } else {
    pb210_as_fit(rlang::exec(x$fun, !!!data))
  }
}

#' @export
predict.pb210_fit_lazy <- function(object, ...) {
  abort("Lazy fit needs to be resolved by calling pb210_as_fit(x, data = ...)")
}

#' Subset generators
#'
#' @inheritParams pb210_fit_exponential
#' @param prop_tail,prop_head A minimum proportion of finite values
#'   that should be included in the output
#' @param n_tail,n_head A minimum number of finite values
#'   that should be included in the output
#'
#' @return A logical vector that can be used to subset `x` and `y`.
#' @export
#'
#' @examples
#' x <- 1:5
#' y <- c(1, 2, NA, NA, 5)
#'
#' finite_head(x, y, 2)
#' finite_tail(x, y, 2)
#' finite_head_prop(x, y, 0.5)
#' finite_tail_prop(x, y, 0.5)
#'
finite_head_prop <- function(x, y, prop_tail = 1) {
  finite <- is.finite(x) & is.finite(y)
  finite_head(x, y, ceiling(sum(finite) * prop_tail))
}

#' @rdname finite_head_prop
#' @export
finite_tail_prop <- function(x, y, prop_head = 1) {
  finite <- is.finite(x) & is.finite(y)
  finite_tail(x, y, ceiling(sum(finite) * prop_head))
}

#' @rdname finite_head_prop
#' @export
finite_tail <- function(x, y, n_tail = sum(is.finite(x) & is.finite(y))) {
  finite <- is.finite(x) & is.finite(y)
  n_finite <- sum(finite)
  first_finite <- n_finite - n_tail + 1

  (cumsum(finite) >= first_finite) & finite
}

#' @rdname finite_head_prop
#' @export
finite_head <- function(x, y, n_head = sum(is.finite(x) & is.finite(y))) {
  rev(finite_tail(rev(x), rev(y), n_head))
}

apply_subset <- function(x, y, subset = NULL) {
  data <- tibble::tibble(x, y)
  if (is.null(subset)) {
    data
  } else if(is.logical(subset) || is.numeric(subset)) {
    data[subset, ]
  } else {
    subset <- rlang::as_function(subset)
    apply_subset(x, y, subset(x, y))
  }
}
