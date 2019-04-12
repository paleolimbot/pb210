
#' Calculate a decay constant
#'
#' @param half_life A half-life value in units of time. For lead-210, this value is 22.26 years.
#'
#' @export
#'
#' @examples
#' pb210_decay_constant()
#'
pb210_decay_constant <- function(half_life = 22.26) {
  log(2) / half_life
}


#' Calculate core area from an internal diameter
#'
#' @param diameter A diameter in meters. The default is 0.063 m (6.3 cm), which
#'   is the internal diameter of the core tubes for our trusty 2.5" Glew (1989) corer.
#'
#' @references
#' Glew, J.R. 1989. A new trigger mechanism for sediment samplers.
#' Journal of Paleolimnology, 2: 241–243. doi:10.1007/BF00195474.
#'
#' @export
#'
#' @examples
#' pb210_core_area()
#'
pb210_core_area <- function(diameter = 6.3 / 100) {
  pi * (diameter / 2)^2
}


#' Generate a time duration in seconds
#'
#' @param days,hours,minutes,seconds Quantities to convert to seconds. Multiple inputs will be
#'   added
#'
#' @return A time in seconds
#' @export
#'
#' @examples
#' pb210_time_seconds(days = 1)
#' pb210_time_seconds(hours = 24)
#'
pb210_time_seconds <- function(days = 0, hours = 0, minutes = 0, seconds = 0) {
  seconds + (minutes * 60) + (hours * 60 * 60) + (days * 24 * 60 * 60)
}

