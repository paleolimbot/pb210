
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
#' @param diameter A diameter in units of length. The default is 6.3, which
#'   is the internal diameter of core tubes for our trusty 2.5" Glew (1989) corer
#'   (in cm).
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
pb210_core_area <- function(diameter = 6.3) {
  pi * (diameter / 2)^2
}
