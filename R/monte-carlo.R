
#' Run a Monte-Carlo simulation on a CRS or CIC fit
#'
#' @param object A [pb210_crs()] or [pb210_cic()] fit.
#' @param n The number of permutations
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
pb210_monte_carlo <- function(object, n = 100) {
  UseMethod("pb210_monte_carlo")
}

#' @rdname pb210_monte_carlo
#' @export
pb210_monte_carlo.pb210_fit_cic <- function(object, n = 100) {
  abort("not implemented")
}

#' @rdname pb210_monte_carlo
#' @export
pb210_monte_carlo.pb210_fit_crs <- function(object, n = 100) {
  abort("not implemented")
}
