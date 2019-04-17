
#' Calculate cumulative mass
#'
#' @param masses A vector of sequential masses
#' @param position 0 for the cumulative mass at the bottom of the section
#'   and 1 for the cumulative mass
#'
#' @return A vector of cumulative masses
#' @export
#'
#' @examples
#' # top of slice
#' pb210_cumulative_mass(c(0.1, 0.1, 0.1, 0.1), position = 0)
#'
#' # middle of slice
#' pb210_cumulative_mass(c(0.1, 0.1, 0.1, 0.1), position = 0.5)
#'
#' # bottom of slice
#' pb210_cumulative_mass(c(0.1, 0.1, 0.1, 0.1), position = 0.5)
#'
pb210_cumulative_mass <- function(masses, position = 0) {
  stopifnot(
    all(is.finite(masses)),
    all(is.finite(position)),
    all((position >= 0) & (position <= 1)),
    length(position) == 1 || length(position) == length(masses)
  )

  mass_top <- c(0, cumsum(masses[-1]))
  mass_bottom <- cumsum(masses)
  (mass_bottom * position) + (mass_top * (1 - position))
}






