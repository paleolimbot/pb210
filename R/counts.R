
#' Work with raw count data
#'
#' In an ideal world, the people who count your samples also calculate the
#' specific activity and associated error. In an imperfect world, these functions
#' are here to calculate specific activity and error given the required information.
#'
#' @param pb210_specific_activity A specific activity of lead-210, in Bq / kg.
#' @param counts The number of measured decays
#' @param count_mass The amount of mass each sample that was counted (kg). Can be
#'   a vector to specify different masses for each sample.
#' @param count_time The time each sample was counted, as resolved by
#'   [lubridate::as.duration()]. Raw numbers are treated as a number of seconds, however
#'   we reccomend using [lubridate::ddays()] and/or [lubridate::dhours()] to make
#'   the units explicit.
#'
#' @return A vector of specific activities or specific activity errors in Bq / kg.
#' @export
#'
#' @examples
#' pb210_error_from_specific_activity(
#'   c(500, 100, 10, 5),
#'   count_mass = 0.5 / 1000,
#'   count_time = lubridate::ddays(1)
#' )
#'
#' pb210_error_from_counts(
#'   c(21600, 4320, 432, 216),
#'   count_mass = 0.5 / 1000,
#'   count_time = lubridate::ddays(1)
#' )
#'
#' pb210_specific_activity_from_counts(
#'   c(21600, 4320, 432, 216),
#'   count_mass = 0.5 / 1000,
#'   count_time = lubridate::ddays(1)
#' )
#'
#' pb210_counts_from_specific_activity(
#'   c(500, 100, 10, 5),
#'   count_mass = 0.5 / 1000,
#'   count_time = lubridate::ddays(1)
#' )
#'
pb210_error_from_specific_activity <- function(pb210_specific_activity, count_mass, count_time) {
  counts <- pb210_counts_from_specific_activity(pb210_specific_activity, count_mass, count_time)
  pb210_error_from_counts(counts, count_mass, count_time)
}

#' @rdname pb210_error_from_specific_activity
#' @export
pb210_error_from_counts <- function(counts, count_mass, count_time) {
  check_count_params(counts, count_mass, count_time)
  count_time <- count_time_seconds(count_time)

  # in a poisson distribution, the variance = the expected counts
  sqrt(counts) / count_time / count_mass # Bq / kg
}

#' @rdname pb210_error_from_specific_activity
#' @export
pb210_specific_activity_from_counts <- function(counts, count_mass, count_time) {
  check_count_params(counts, count_mass, count_time)
  count_time <- count_time_seconds(count_time)

  counts / count_time / count_mass # Bq / kg
}

#' @rdname pb210_error_from_specific_activity
#' @export
pb210_counts_from_specific_activity <- function(pb210_specific_activity, count_mass, count_time) {
  check_count_params(pb210_specific_activity, count_mass, count_time)
  count_time <- count_time_seconds(count_time)
  pb210_specific_activity * count_mass * count_time # counts
}


check_count_params <- function(x, count_mass, count_time) {
  stopifnot(
    is.numeric(x),
    length(count_mass) == 1 || length(count_mass) == length(x),
    all(count_mass > 0),
    length(count_time) == 1 || length(count_time) == length(x),
    all(as.numeric(count_time) > 0)
  )
}

count_time_seconds <- function(count_time) {
  as.numeric(lubridate::as.duration(count_time), "seconds") # s
}

