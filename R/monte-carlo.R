
#' Run a Monte-Carlo simulation on a CRS or CIC fit
#'
#'
#'
#' @param object A [pb210_crs()] or [pb210_cic()] fit.
#' @param n The number of permutations. The default is 1,000, as
#'   Sanchez-Cabeza et al. (2014) found that this was the minimum
#'   number of iterations needed for Monte-Carlo uncertainty to
#'   converge on the quadrature-propegated uncertainty. In general,
#'   Sanchez-Cabeza et al. (2014) used n values from 1,000 to 4,000.
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
pb210_monte_carlo <- function(object, n = 1000) {
  UseMethod("pb210_monte_carlo")
}

#' @rdname pb210_monte_carlo
#' @export
pb210_monte_carlo.pb210_fit_cic <- function(object, n = 1000) {
  abort("not implemented")
}

#' @rdname pb210_monte_carlo
#' @export
pb210_monte_carlo.pb210_fit_crs <- function(object, n = 1000) {
  abort("not implemented")
}
