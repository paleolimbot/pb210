
#' Apply the Constant Rate of Supply or Constant Initial Concentration model
#'
#' @param depth The representative depth of each sample. Must be greater than 0.
#' @param sample_mass The mass of each sample.
#' @param excess_pb210 Excess (non-erosional) lead-210 activity. Can be NA for samples on which
#'   lead-210 was not measured.
#' @param excess_pb210_sd The standard deviation of the excess lead-210 value. Can be NA if error is not known.
#' @param calc_excess_pb210_surface,calc_inventory_surface,calc_inventory_below The surface
#'   inventory and deep inventory (CRS) and surface lead-210 activity (CIC)
#'   are important factors in the models that may need to be estimated. The default is to use the value
#'   at the minimum depth value (closest to surface). purrr-style one-sided formulas are
#'   supported.
#' @param core_area The internal diameter of the corer, in cm^2
#' @param decay_constant The decay contstant for lead-210, in 1/yr. This is an argument
#'   because different spreadsheets use different decay constants (!!), and we would like
#'   these functions to reproduce most existing age-depth models. Ideally this value should
#'   be ln(2) / 22.26 (the default), given that the half-life of lead-210 is 22.26 years.
#'
#' @references
#' Appleby, P.G., and Oldfield, F. 1983. The assessment of 210Pb data from sites with
#' varying sediment accumulation rates.
#' Hydrobiologia, 103: 29–35. doi:10.1007/BF00028424.
#'
#' @return A table with (at least) components `age` and `age_sd`. CRS model
#'   output also contains `mar` and `mar_sd`.
#' @export
#'
pb210_age_cic <- function(depth, excess_pb210, excess_pb210_sd = NA_real_,
                          calc_excess_pb210_surface = pb210_surface_min_depth,
                          decay_constant = pb210_decay_constant()) {
  stopifnot(
    is.numeric(depth), length(depth) >= 3, all(is.finite(depth)),
    # no repeated depths or depth == 0
    length(depth) == length(unique(depth)), all(depth != 0),
    is.numeric(excess_pb210),
    is.numeric(excess_pb210_sd)
  )

  calc_excess_pb210_surface <- rlang::as_function(calc_excess_pb210_surface)

  # also checks consistent length
  tbl <- tibble::tibble(.id = seq_along(depth), depth, excess_pb210, excess_pb210_sd)
  tbl <- tbl[order(tbl$depth), ]

  # estimate the surface pb210 value (needed for interpolation in some cases)
  surface_excess_pb210 <- calc_excess_pb210_surface(tbl$depth, tbl$excess_pb210)

  # all finite values are needed for excess_pb210; some must be interpolated
  finite_depth <- c(0, tbl$depth[is.finite(tbl$excess_pb210)])
  finite_excess_pb210 <- c(surface_excess_pb210, tbl$excess_pb210[is.finite(tbl$excess_pb210)])
  tbl$excess_pb210_interp <- stats::approx(finite_depth, finite_excess_pb210, depth)$y

  # NA values in the interpolated version are now at the very bottom (no excess pb210)
  tbl$excess_pb210_interp[is.na(tbl$excess_pb210_interp)] <- 0

  # excess pb210 values must be positive, as they get logged to calculate the age
  if(any(tbl$excess_pb210_interp < 0)) {
    warning(sum(tbl$excess_pb210_interp < 0), " negative excess lead-210 activity(ies) clamped to zero")
    tbl$excess_pb210_interp[tbl$excess_pb210_interp < 0] <- 0
  }

  # TODO: I don't understand this error propogation, particularly why we add 2% extra error
  # to the MAR
  tbl$age <- 1 / decay_constant * log(surface_excess_pb210 / tbl$excess_pb210_interp)

  pb210_relative_error <- tbl$excess_pb210_sd / tbl$excess_pb210
  mar_relative_error <- sqrt(pb210_relative_error^2 + pb210_relative_error^2 + 0.02^2)
  tbl$age_sd <- tbl$age * mar_relative_error

  # return the tbl in the original order it was specified
  tbl_out <- tbl[order(tbl$.id), ]
  tbl_out$.id <- NULL

  tbl_out
}

#' @rdname pb210_age_cic
#' @export
pb210_age_crs <- function(depth, excess_pb210, sample_mass, excess_pb210_sd = NA_real_,
                          calc_excess_pb210_surface = pb210_surface_min_depth,
                          calc_inventory_surface = pb210_surface_min_depth,
                          calc_inventory_below = pb210_deep_inventory_zero,
                          core_area = pi * (6.3 / 2)^2, decay_constant = pb210_decay_constant()) {

  stopifnot(
    is.numeric(sample_mass),
    length(core_area) == 1, is.numeric(core_area)
  )

  calc_inventory_surface <- rlang::as_function(calc_inventory_surface)
  calc_inventory_below <- rlang::as_function(calc_inventory_below)

  tbl <- pb210_age_cic(
    depth = depth,
    excess_pb210 = excess_pb210,
    excess_pb210_sd = excess_pb210_sd,
    calc_excess_pb210_surface = calc_excess_pb210_surface,
    decay_constant = decay_constant
  )

  # these calculations depend on depth-ordering
  tbl$.id <- seq_along(tbl$depth)
  tbl <- tbl[order(tbl$depth), ]

  # the MAR relative error was calculated above in pb210_age_cic()
  mar_relative_error <- tbl$age_sd / tbl$age

  # the cumulative inventory prior to any measured pb210 must be calculated
  deep_inventory <- calc_inventory_below(tbl$depth, tbl$excess_pb210)

  # the CRS is based on similar math but on the total lead-210
  # in the core starting at background
  tbl$excess_pb210_in_core <- tbl$excess_pb210_interp * sample_mass
  tbl$inventory <- deep_inventory + rev(cumsum(rev(tbl$excess_pb210_in_core)))
  surface_inventory <- calc_inventory_surface(tbl$depth, tbl$inventory)

  # age calculations
  tbl$age <- 1 / decay_constant * log(surface_inventory / tbl$inventory)
  tbl$mar <- decay_constant * tbl$inventory / tbl$excess_pb210_interp / core_area

  tbl$age_sd <- tbl$age * mar_relative_error
  tbl$mar_sd <- tbl$mar * mar_relative_error

  # return the tbl in the original order it was specified
  tbl_out <- tbl[order(tbl$.id), ]
  tbl_out$.id <- NULL

  tbl_out
}

#' Estimate surface Lead-210 activity
#'
#' Estimates surface lead-210 activity using an exponential fit of `pb210`
#' vs. `-depth`, or using the lead-210 activity at the  minimum `depth` value.
#'
#' @param depth Depth of sample
#' @param pb210 Activity of lead-210 or inventory
#'
#' @return A plausible surface value given the inputs
#' @export
#'
pb210_surface_min_depth <- function(depth, pb210) {
  pb210[which.min(depth)]
}

#' @rdname pb210_surface_min_depth
#' @export
pb210_surface_estimate_loglinear <- function(depth, pb210) {
  coeffs <- fit_loglinear_model(depth, pb210, "surface value")
  unname(exp(coeffs["b"]))
}

#' @rdname pb210_surface_min_depth
#' @export
pb210_surface_estimate <- function(depth, pb210) {
  coeffs <- fit_exponential_model(depth, pb210, "surface value")
  unname(exp(coeffs["b"]))
}

#' @rdname pb210_surface_min_depth
#' @export
pb210_deep_inventory_zero <- function(depth, pb210) {
  0
}

#' @rdname pb210_surface_min_depth
#' @export
pb210_deep_inventory_estimate <- function(depth, pb210) {
  pb210_deep_inventory_estimate_base(
    depth, pb210,
    fit_exponential_model(depth, pb210, "deep inventory")
  )
}

#' @rdname pb210_surface_min_depth
#' @export
pb210_deep_inventory_estimate_loglinear <- function(depth, pb210) {
  pb210_deep_inventory_estimate_base(
    depth, pb210,
    fit_loglinear_model(depth, pb210, "deep inventory")
  )
}

pb210_deep_inventory_estimate_base <- function(depth, pb210, coeffs) {

  # find the place below which we need the integral to Inf
  max_depth <- max(depth[is.finite(pb210)])

  # integrated, this is exp(m*x + b) / m, and because at x = infinity the integral is 0
  # the definite integral from [background] to infinity is exp(m * [background] + b) / -m
  unname(exp(coeffs["m"] * max_depth  + coeffs["b"]) / -coeffs["m"])
}

fit_loglinear_model <- function(x, y, estimate_context = "a parameter") {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  fit <- try(stats::lm(log(y) ~ x, na.action = stats::na.omit), silent = TRUE)
  if(inherits(fit, "try-error")) {
    stop("Could not estimate ", estimate_context, " using a log-transform linear regression")
  }
  coeffs <- stats::coefficients(fit)
  names(coeffs) <- c("b", "m")
  coeffs
}

fit_exponential_model <- function(x, y, estimate_context = "a parameter") {
  # y values that are 0 or less will cause the model not to fit
  y[y <= 0] <- NA_real_

  # the nonlinear least squares function often fails when coefficients do not make sense
  # using a loglinear fit to find the starting place is one way around this
  coeffs_loglin <- fit_loglinear_model(x, y, sprintf("exponential model start parameters (%s)", estimate_context))

  fit <- try(stats::nls(y ~ exp(m * x + b), start = as.list(coeffs_loglin)), silent = TRUE)
  if(inherits(fit, "try-error")) {
    stop("Could not estimate ", estimate_context, " using a non-linear least squares exponential model")
  }

  stats::coefficients(fit)
}
