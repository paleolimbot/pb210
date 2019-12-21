
#' Run a Monte-Carlo simulation on a CRS or CIC fit
#'
#' These functions run many simulations on randomly-sampled activity values
#' constrained by the measured activity and estimated background. Excess
#' is calculated by [pb210_excess()] for each simulation. Prediction
#' results are presented as the median result and are constrained by
#' min (5th percentile) and max (95th percentile) values (instead of
#' quadrature-propagated error like [pb210_cic()] and [pb210_crs()]). Note
#' that this may take 10 seconds per 1,000 iterations (depending on
#' hardware).
#'
#' @inheritParams pb210_cic
#' @inheritParams pb210_excess
#' @param n The number of permutations. The default is 1,000, as
#'   Sanchez-Cabeza et al. (2014) found that this was the minimum
#'   number of iterations needed for Monte-Carlo uncertainty to
#'   converge on the quadrature-propagated uncertainty. In general,
#'   Sanchez-Cabeza et al. (2014) used n values from 1,000 to 4,000.
#' @param sample_activity,sample_background,sample_decay_constant Random
#'   sampler functions such as [pb210_sample_norm()] that are called
#'   `n` times for the appropriate argument.
#'
#' @export
#'
#' @references
#' Binford, M.W. 1990. Calculation and uncertainty analysis of ^210^Pb dates for
#' PIRLA project lake sediment cores. Journal of Paleolimnology, 3: 253–267.
#' <https://doi.org/10.1007/BF00219461>
#'
#' Sanchez-Cabeza, J.-A., Ruiz-Fernández, A.C., Ontiveros-Cuadras, J.F.,
#' Pérez Bernal, L.H., and Olid, C. 2014. Monte Carlo uncertainty calculation of ^210^Pb
#' chronologies and accumulation rates of sediments and peat bogs.
#' Quaternary Geochronology, 23: 80–93. <https://doi.org/10.1016/j.quageo.2014.06.002>
#'
#' @examples
#' # simulate a core
#' core <- pb210_simulate_core() %>%
#'   pb210_simulate_counting()
#'
#' # calculate ages using the CRS model
#' crs <- pb210_crs_monte_carlo(
#'   pb210_cumulative_mass(core$slice_mass),
#'   set_errors(
#'     core$activity_estimate,
#'     core$activity_se
#'   ),
#'   n = 100
#' )
#'
#' predict(crs)
#'
pb210_cic_monte_carlo <- function(cumulative_dry_mass, activity, background = 0,
                                  model_top = ~pb210_fit_exponential(..1, ..2),
                                  decay_constant = pb210_decay_constant(),
                                  n = 1000,
                                  sample_activity = pb210_sample_norm,
                                  sample_background = pb210_sample_norm,
                                  sample_decay_constant = pb210_sample_norm) {
  force(cumulative_dry_mass)
  force(model_top)

  fit_results <- fit_many(
    activity = activity,
    background = background,
    fit_fun = function(excess, decay_constant) {
      pb210_cic(
        cumulative_dry_mass, excess,
        model_top = model_top, decay_constant = decay_constant
      )
    },
    decay_constant = decay_constant,
    n = n,
    sample_activity = sample_activity,
    sample_background = sample_background,
    sample_decay_constant = sample_decay_constant
  )

  structure(
    fit_results,
    class = c("pb210_fit_cic_monte_carlo", "pb210_fit_mc", "pb210_fit")
  )
}

#' @rdname pb210_cic_monte_carlo
#' @export
pb210_crs_monte_carlo <- function(cumulative_dry_mass, activity, background = 0,
                                  inventory = pb210_inventory_calculator(),
                                  core_area = pb210_core_area(),
                                  decay_constant = pb210_decay_constant(),
                                  n = 1000,
                                  sample_activity = pb210_sample_norm,
                                  sample_background = pb210_sample_norm,
                                  sample_decay_constant = pb210_sample_norm) {

  stopifnot(
    # only works with inventories that are calculated
    inherits(inventory, "inventory_calculator")
  )

  force(cumulative_dry_mass)
  force(inventory)
  force(core_area)

  fit_results <- fit_many(
    activity = activity,
    background = background,
    fit_fun = function(excess, decay_constant) {
      pb210_crs(
        cumulative_dry_mass, excess,
        inventory = inventory,
        core_area = core_area,
        decay_constant = decay_constant
      )
    },
    decay_constant = decay_constant,
    n = n,
    sample_activity = sample_activity,
    sample_background = sample_background,
    sample_decay_constant = sample_decay_constant
  )

  structure(
    fit_results,
    class = c("pb210_fit_cic_monte_carlo", "pb210_fit_mc", "pb210_fit")
  )
}

#' @rdname pb210_cic_monte_carlo
#' @export
predict.pb210_fit_cic_monte_carlo <- function(object, cumulative_dry_mass = NULL, ...) {
  if (is.null(cumulative_dry_mass)) {
    cumulative_dry_mass <- object$fit_base$data$cumulative_dry_mass
  }

  n_samples <- length(cumulative_dry_mass)

  prediction_results <- purrr::map(
    object$fits,
    stats::predict,
    cumulative_dry_mass = cumulative_dry_mass,
    # for null fits
    out_length = n_samples,
    out_names = c("age", "excess", "activity", "background")
  )

  inputs <- purrr::transpose(object$inputs)

  vctrs::vec_cbind(
    predict_many(prediction_results, "age", len = n_samples),
    predict_many(inputs, "excess", len = n_samples),
    predict_many(inputs, "activity", len = n_samples),
    predict_many(inputs, "background", len = n_samples)
  )
}

#' @rdname pb210_cic_monte_carlo
#' @export
predict.pb210_fit_crs_monte_carlo <- function(object, cumulative_dry_mass = NULL, ...) {
  if (is.null(cumulative_dry_mass)) {
    cumulative_dry_mass <- object$fit_base$data$cumulative_dry_mass
  }

  n_samples <- length(cumulative_dry_mass)

  prediction_results <- purrr::map(
    object$fits,
    stats::predict,
    cumulative_dry_mass = cumulative_dry_mass,
    # for null fits
    out_length = n_samples,
    out_names = c("age", "mar", "inventory", "excess", "activity", "background")
  )

  inputs <- purrr::transpose(object$inputs)

  vctrs::vec_cbind(
    # predict_many(object$fits, c("data", "excess"), len = n_samples),
    predict_many(prediction_results, "age", len = n_samples),
    predict_many(prediction_results, "mar", len = n_samples),
    predict_many(prediction_results, "inventory", len = n_samples),
    predict_many(inputs, "excess", len = n_samples),
    predict_many(inputs, "activity", len = n_samples),
    predict_many(inputs, "background", len = n_samples)
  )
}

fit_many <- function(activity, background,
                     fit_fun,
                     decay_constant = pb210_decay_constant(),
                     n = 1000,
                     sample_activity = pb210_sample_norm,
                     sample_background = pb210_sample_norm,
                     sample_decay_constant = pb210_sample_norm) {
  stopifnot(n >= 2)
  sample_activity <- rlang::as_function(sample_activity)
  sample_background <- rlang::as_function(sample_background)
  sample_decay_constant <- rlang::as_function(sample_decay_constant)

  # check plausible base fit (also checks args)
  fit_base <- fit_fun(pb210_excess(activity, background), decay_constant)

  inputs <- tibble::tibble(
    activity = purrr::map(seq_len(n), function(i) sample_activity(activity)),
    background = purrr::map(seq_len(n), function(i) sample_background(background)),
    excess = purrr::map2(
      .data$activity,
      .data$background,
      function(activity, background) without_errors(pb210_excess(activity, background))
    ),
    decay_constant = purrr::map_dbl(
      seq_len(n),
      function(i) sample_decay_constant(decay_constant)
    )
  )

  results <- purrr::map2(
    inputs$excess, inputs$decay_constant,
    purrr::safely(fit_fun, otherwise = pb210_age_depth_na())
  )

  fits <- purrr::map(results, "result")
  problems <- purrr::map(results, "error")
  has_problems <- !purrr::map_lgl(problems, is.null)

  if (any(has_problems)) {
    rlang::warn(
      glue::glue(
        "{sum(has_problems)} model(s) failed to fit. Use `$problems` to diagnose."
      )
    )
  }

  list(
    fit_base = fit_base,
    n = n,
    inputs = inputs,
    fits = fits,
    problems = problems,
    has_problems = has_problems
  )
}

#' @importFrom rlang :=
predict_many <- function(prediction_results, key, len,
                         summarise_value = ~stats::median(..1, na.rm = TRUE),
                         summarise_min = ~stats::quantile(..1, 0.05, na.rm = TRUE),
                         summarise_max = ~stats::quantile(..1, 0.95, na.rm = TRUE)) {
  key <- key[length(key)]
  by_depth <- transpose_predictions(prediction_results, key, len = len)

  tibble::tibble(
    !!key := purrr::map_dbl(by_depth, summarise_value),
    !!paste0(key, "_min") := purrr::map_dbl(by_depth, summarise_min),
    !!paste0(key, "_max") := purrr::map_dbl(by_depth, summarise_max),
    !!paste0(key, "_values") := by_depth
  )
}

transpose_predictions <- function(prediction_results, column,
                                  len = nrow(prediction_results[[1]])) {
  force(column)
  purrr::map(
    seq_len(len),
    function(i) purrr::map_dbl(prediction_results, ~..1[[column]][i])
  )
}

#' Random samplers
#'
#' For use in [pb210_crs_monte_carlo()]. In the future, other distributions
#' may be considered; we approximate mean + standard error as a normal distribution
#' because this is done in
#'
#' @param x A vector of values, possibly with [errors::errors()].
#'
#' @return A (errorless) vector of randomly sampled values if `x` has
#'   [errors::errors()], otherwise `x`, unchanged.
#' @export
#'
#' @references
#' Aquino-López, M.A., Blaauw, M., Christen, J.A.,
#' and Sanderson, N.K. 2018. Bayesian Analysis of ^210^Pb Dating.
#' Journal of Agricultural, Biological and Environmental Statistics, 23: 317–333.
#' <https://doi.org/10.1007/s13253-018-0328-7>.
#'
#' @examples
#' pb210_sample_norm(set_errors(1:10, 1))
#' pb210_sample_norm(1:10)
#'
pb210_sample_norm <- function(x) {
  if (inherits(x, "errors") && any(is.finite(extract_errors(x)))) {
    # this is the easiest way to deal with NA values in x and NA values in errors
    suppressWarnings(stats::rnorm(length(x), mean = drop_errors(x), sd = errors(x)))
  } else {
    without_errors(x)
  }
}


pb210_age_depth_na <- function() {
  structure(list(must_not_be_empty = TRUE), class = "pb210_age_depth_na")
}

#' @export
predict.pb210_age_depth_na <- function(object, ..., out_length, out_names) {
  na_vec <- rep_len(NA_real_, out_length)
  out <- rep_len(list(na_vec), length(out_names))
  names(out) <- out_names
  tibble::as_tibble(out)
}
