
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
#' @param core_area The internal diameter of the corer, in units of length^2.
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
                          core_area = pb210_core_area(),
                          decay_constant = pb210_decay_constant()) {

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


#' Calculate cumulative lead-210 activity
#'
#' The cumulative content of lead-210 from the bottom of the core is the basis for the
#' constant rate of supply model, and is a required input to [pb210_age_crs()]. There are
#' as many ways to calculate this as there are people who interpret lead-210 activities.
#' This function models the bottom (below measured lead-210), middle (between measured
#' values of lead-210), and top (above measured lead-210) with separate models to
#' accomodate the variety of methods. By default, lead-210 activity is estimated
#' for samples in which it was not measured by an exponential fit of lead-210
#' activity vs. depth (top and bottom), and by linear interpolation between
#' values (middle).
#'
#' @param cumulative_dry_mass The cumulative dry mass of the core in kilograms, starting at depth
#'   0 and including all samples in the core. These must be positive and in increasing order.
#' @param excess_pb210_specific_activity An excess lead-210 activity for samples where this was
#'   measured, and NA where lead-210 was not measured. NA values will be estimated using
#'   `model_top`, `model_middle`, and `model_bottom`.
#' @param model_bottom A fit object that will be used to model activities below
#'   the last positive finite lead-210 activity. This must be created using
#'   [pb210_fit_exponential()] in that its `m` and `b` coefficients are used to calculate
#'   the integrated activity below the last positive finite `excess_pb210_specific_activity`.
#' @param model_middle A fit object that will be used to model activities between
#'   measured finite lead-210 activities.
#' @param n_segments The number of tiny rectangles used to approximate the cumulative
#'   activity between the first and last positive finite lead-210 measurement.
#'
#' @return A vector of cumulative lead-210 activities for each sample in Bq.
#' @export
#'
#' @examples
#' fake_mass <- 1:10
#' fake_pb210 <- exp(5 - fake_mass) + rnorm(10, sd = 0.005)
#' pb210_calculate_inventory(fake_mass, fake_pb210)
#'
#' # compare with known inventory from integrating
#' # exp(5 - fake_mass) to +Inf
#' exp(-1 * fake_mass  + 5) / -(-1)
#'
pb210_calculate_inventory <- function(
  cumulative_dry_mass, excess_pb210_specific_activity,
  model_bottom = pb210_fit_exponential(cumulative_dry_mass, excess_pb210_specific_activity),
  model_middle = pb210_fit_interpolator_linear(cumulative_dry_mass, excess_pb210_specific_activity),
  n_segments = 200L
) {
  stopifnot(
    is.numeric(cumulative_dry_mass),
    all(is.finite(cumulative_dry_mass)),
    all(cumulative_dry_mass > 0),
    all(diff(cumulative_dry_mass) > 0),
    is.numeric(excess_pb210_specific_activity),
    length(cumulative_dry_mass) == length(excess_pb210_specific_activity),
    sum(is.finite(excess_pb210_specific_activity) & (excess_pb210_specific_activity > 0)) >= 3,
    is.integer(n_segments)
  )

  finite_pb210_indices <- which(
    is.finite(excess_pb210_specific_activity) &
      (excess_pb210_specific_activity > 0)
  )

  first_finite_mass <- min(finite_pb210_indices) # kg
  last_finite_mass <- max(finite_pb210_indices) # kg

  # the model exp(m*x + b),
  # integrated, is exp(m*x + b) / m, and because at x = infinity the integral is 0
  # the definite integral from [background] to infinity is exp(m * [background] + b) / -m
  coeffs <- stats::coefficients(model_bottom)
  deep_pb210 <- function(mass) unname(exp(coeffs["m"] * mass  + coeffs["b"]) / -coeffs["m"])

  # in the middle, approximate the cumulative sum with a bunch of tiny rectangles
  mass_step <- (last_finite_mass - first_finite_mass) / n_segments # kg
  mass_points <- seq(
    first_finite_mass,
    last_finite_mass - mass_step / 2,
    length.out = n_segments - 1
  ) # kg
  pb210_modeled <- stats::predict(model_middle, tibble::tibble(x = mass_points)) # Bq / kg
  inventory_middle <- rev(cumsum(rev(pb210_modeled))) * mass_step # Bq
  inventory_middle_interp <- pb210_fit_interpolator_linear(
    c(mass_points, last_finite_mass),
    deep_pb210(last_finite_mass) + c(inventory_middle, 0)
  )

  # combine the two methods
  ifelse(
    cumulative_dry_mass >= last_finite_mass,
    predict(inventory_middle_interp, tibble::tibble(x = cumulative_dry_mass)),
    deep_pb210(cumulative_dry_mass)
  )
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
