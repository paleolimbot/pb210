
#' Calculate a decay constant
#'
#' @param half_life A half-life value in years. For lead-210, this value is 22.26 years.
#'
#' @export
#'
#' @examples
#' pb210_decay_constant()
#'
pb210_decay_constant <- function(half_life = 22.26) {
  log(2) / half_life
}





