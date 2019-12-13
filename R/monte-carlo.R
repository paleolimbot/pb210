
#' Run a Monte-Carlo simulation on a CRS or CIC fit
#'
#'
#'
#' @inheritParams pb210_cic
#' @inheritParams pb210_excess
#' @param n The number of permutations. The default is 1,000, as
#'   Sanchez-Cabeza et al. (2014) found that this was the minimum
#'   number of iterations needed for Monte-Carlo uncertainty to
#'   converge on the quadrature-propegated uncertainty. In general,
#'   Sanchez-Cabeza et al. (2014) used n values from 1,000 to 4,000.
#' @param sample_activity,sample_background,sample_decay_constant Random
#'   sampler functions such as [pb210_sample_norm()] that are called
#'   `n` times for the appropriate argument.
#' @param summarise_value,summarise_sd Functions that accept a vector
#'   of values and calculate a summary value or error, respectively.
#'   The default calculates a median and standard error.
#'
#' @export
#'
#' @references
#' Binford, M.W. 1990. Calculation and uncertainty analysis of ^210^Pb dates for
#' PIRLA project lake sediment cores. Journal of Paleolimnology, 3: 253–267.
#' doi:10.1007/BF00219461.
#'
#' Sanchez-Cabeza, J.-A., Ruiz-Fernández, A.C., Ontiveros-Cuadras, J.F.,
#' Pérez Bernal, L.H., and Olid, C. 2014. Monte Carlo uncertainty calculation of ^210^Pb
#' chronologies and accumulation rates of sediments and peat bogs.
#' Quaternary Geochronology, 23: 80–93. doi:10.1016/j.quageo.2014.06.002.
#'
pb210_cic_monte_carlo <- function(cumulative_dry_mass, activity, background = 0,
                                  model_top = ~pb210_fit_exponential(..1, ..2),
                                  decay_constant = pb210_decay_constant(),
                                  n = 100,
                                  sample_activity = pb210_sample_norm,
                                  sample_background = pb210_sample_norm,
                                  sample_decay_constant = pb210_sample_norm) {
  stopifnot(
    n > 2,
    is.function(sample_activity),
    is.function(sample_background),
    is.function(sample_decay_constant)
  )

  # check arguments and general realisticness of model using
  # a basic fit
  fit_base <- pb210_cic(
    cumulative_dry_mass = cumulative_dry_mass,
    excess = pb210_excess(activity, background),
    model_top = model_top,
    decay_constant = decay_constant
  )

  # make sure fit_base has errors
  stopifnot(fit_base$use_errors)

  cic_factory <- function(i) {
    pb210_cic(
      cumulative_dry_mass = cumulative_dry_mass,
      excess = pb210_excess(sample_activity(activity), sample_activity(background)),
      model_top = model_top,
      decay_constant = sample_decay_constant(decay_constant)
    )
  }

  results <- purrr::map(
    seq_len(n),
    purrr::safely(cic_factory, otherwise = pb210_cic_na(length(cumulative_dry_mass)))
  )

  fits <- purrr::map(results, "result")
  problems <- purrr::map(results, "error")
  has_problems <- !purrr::map_lgl(problems, is.null)

  if (any(has_problems)) {
    rlang::warn(
      glue::glue("{sum(has_problems)} model(s) failed to fit. Use `$problems` to diagnose.")
    )
  }

  structure(
    list(
      fit_base = fit_base,
      fits = fits,
      problems = problems,
      has_problems = has_problems,
      n = n
    ),
    class = c("pb210_fit_cic_monte_carlo", "pb210_fit_mc", "pb210_fit")
  )
}

#' @rdname pb210_cic_monte_carlo
#' @export
pb210_crs_monte_arlo <- function(cumulative_dry_mass, excess,
                                inventory = pb210_inventory_calculator(),
                                core_area = pb210_core_area(),
                                decay_constant = pb210_decay_constant()) {

  abort("not implemented")
}

#' @rdname pb210_cic_monte_carlo
#' @export
predict.pb210_fit_cic_monte_carlo <- function(object,
                                              cumulative_dry_mass = NULL, ...,
                                              summarise_value = stats::median,
                                              summarise_sd = ~sd(..1) / sqrt(length(..1))) {
  prediction_results <- purrr::map(
    object$fits,
    stats::predict,
    cumulative_dry_mass = cumulative_dry_mass
  )

  ages_by_depth <- purrr::map(
    seq_len(nrow(prediction_results[[1]])),
    function(i) purrr::map_dbl(prediction_results, ~..1$age[i])
  )

  tibble::tibble(
    age = purrr::map_dbl(ages_by_depth, summarise_value),
    age_sd = purrr::map_dbl(ages_by_depth, summarise_sd),
    ages = ages_by_depth
  )
}



#' Random samplers
#'
#' @param x A vector of values, possibly with [errors::errors()].
#'
#' @return A (errorless) vector of randomly sampled values if `x` has
#'   [errors::errors()], otherwise `x`, unchanged.
#' @export
#'
#' @examples
#' pb210_sample_norm(set_errors(1:10, 1))
#' pb210_sample_norm(set_errors(1:10, 1))
#' pb210_sample_norm(1:10)
#'
pb210_sample_norm <- function(x) {
  if (inherits(x, "errors") && any(is.finite(extract_errors(x)))) {
    stats::rnorm(length(x), mean = drop_errors(x), sd = errors(x))
  } else {
    without_errors(x)
  }
}


pb210_cic_na <- function(out_length) {
  structure(list(out_length), class = "pb210_fit_cic_na")
}

pb210_crs_na <- function(out_length) {
  structure(list(out_length), class = "pb210_fit_crs_na")
}

#' @export
predict.pb210_na_fit <- function(object, ...) {
  tibble::tibble(
    age = rep_len(NA_real_, object$out_length),
    age_sd = rep_len(NA_real_, object$out_length)
  )
}

#' @export
predict.pb210_na_fit <- function(object, ...) {
  na_vec <- rep_len(NA_real_, object$out_length)
  tibble::tibble(
    age = na_vec,
    age_sd = na_vec,
    mar = na_vec,
    mar_sd = na_vec,
    inventory = na_vec,
    inventory_sd = na_vec
  )
}
