
#' Calculate excess (unsupported) lead-210
#'
#' Boath `activitiy` and  `background` should be arranged such that the
#' top of the core is first.
#'
#' @param activity A vector of measured lead-210 specific activities (in Bq/kg) and
#'   associated error. These can have [errors::errors()].
#' @param background A vector of estimated background
#'   lead-210 specific activity (in Bq/kg) and associated error.
#'   These can have [errors::errors()].
#'
#' @return A vector with [errors::errors()] of the excess lead-210 specific activity.
#'    Background is determined by the first point at which `activity` is less than
#'    or equal to `background`. All excess values are set to `NA` below
#'    this point.
#' @export
#'
#' @examples
#' core <- pb210_simulate_core(depth_step = rep(1, 30)) %>%
#'   pb210_simulate_counting()
#'
#' pb210_excess(
#'   set_errors(
#'     core$activity_estimate,
#'     core$activity_sd
#'   ),
#'   background = 10
#' )
#'
pb210_excess <- function(activity, background = 0) {
  excess <- with_errors(activity) - with_errors(background)
  excess_is_negative <- (without_errors(excess) - extract_errors(excess)) <= 0

  # it causes substantial problems if excess values below the first
  # "background" value are not treated as "background" (i.e., set to NA here)
  first_negative_excess <- which(excess_is_negative)[1]
  if (!is.na(first_negative_excess)) {
    excess_is_negative <- excess_is_negative | (seq_along(excess) > first_negative_excess)
  }

  excess[excess_is_negative] <- NA_real_
  excess
}
