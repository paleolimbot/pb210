
#' Calculate excess (unsupported) lead-210
#'
#' @param activity A vector of measured lead-210 specific activities (in Bq/kg) and
#'   associated error. These can have [errors::errors()].
#' @param background A vector of estimated background
#'   lead-210 specific activity (in Bq/kg) and associated error.
#'
#' @return A vector with [errors::errors()] of the excess lead-210 specific activity.
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
  excess[(without_errors(excess) - extract_errors(excess)) <= 0] <- NA_real_
  excess
}
