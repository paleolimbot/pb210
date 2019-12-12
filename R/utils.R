
#' Calculate a decay constant
#'
#' The most commonly cited half-life for lead-210 is 22.26 with 2-sigma
#' uncertainty of 0.22 (Hohndorf 1969), however Holden (1990) recommended using
#' a value of 22.6(1). Most recently, Basunia (2014) recommended using a value
#' of 22.20 with Hohndorf's (1969) uncertainty of 0.22 years. Sanchez-Cabeza et
#' al. (2014) use the half life and uncertainty reported by Chiste and Be (2007)
#' of 22.23(15). The default value is the most recently reccomended value
#' (Basunia 2014).
#'
#' @param half_life A half-life value (in years).
#'
#' @export
#'
#' @references Basunia, M.S. 2014. Nuclear Data Sheets for A = 210. Nuclear Data
#' Sheets, 121: 561–694. doi:10.1016/j.nds.2014.09.004.
#'
#' Chisté, V., and Bé, M.M. 2007. 210Pb - Comments on evaluation of decay data.
#' Laboratoire National Henri Becquerel, Gif-sur-Yvette Cedex, France. Available
#' from http://www.lnhb.fr/nuclear-data/nuclear-data-table/.
#'
#' Hohndorf, A. 1969. Bestimmung der Halbwertszeit von ^210^Pb. Zeitschrift fur
#' Naturforschung A, 24: 612–615. doi:10.1515/zna-1969-0419.
#'
#' Holden, N.E. 1990. Total half-lives for selected nuclides. Pure and Applied
#' Chemistry, 62: 941–958. doi:10.1351/pac199062050941.
#'
#' @examples
#' pb210_decay_constant()
#'
pb210_decay_constant <- function(half_life = set_errors(22.20, 0.22)) {
  with_errors(log(2), 0) / half_life
}


#' Calculate core area from an internal diameter
#'
#' @param diameter A diameter in meters. The default is `2 * sqrt(1 / pi)`,
#'   such that the default core area is exactly 1 m^2^.
#' @param thickness A thickness  in meters.
#' @param core_area The cross-sectional area being considered. The default
#'   is 1 m^2^ such that by default, functions report density and mass on a
#'   specific area basis.
#'
#' @export
#'
#' @examples
#' # area of an 8-cm diameter core
#' pb210_core_area(0.08)
#'
#' # 2-cm thick layer over the default 1 m^2
#' pb210_slice_volume(0.02)
#'
pb210_core_area <- function(diameter = 2 * sqrt(1 / pi)) {
  pi * (diameter / 2)^2
}

#' @rdname pb210_core_area
#' @export
pb210_slice_volume <- function(thickness, core_area = pb210_core_area()) {
  thickness * core_area
}


#' Test age similarity
#'
#' The testing of this package requires a lot of testing whether or not
#' two sets of ages are similar. This function is designed for use in
#' this testing. The default is 1 year of error in the last 100 years.
#'
#' @param calculated_ages The ages that we aren't sure about
#' @param known_ages The ages that we are sure about
#' @param max_delta The maximum error
#' @param age_range The age range we care about
#' @param na.rm Should missing ages be removed?
#'
#' @export
#'
#' @examples
#' # similar ages
#' expect_ages_similar(1:10 + runif(10, -1, 1), 1:10, max_delta = 1)
#'
#' # not similar ages
#' try(expect_ages_similar(1:10 + runif(10, -1, 1), 1:10, max_delta = 0.01))
#'
expect_ages_similar <- function(calculated_ages, known_ages, max_delta = 1, age_range = 0:100, na.rm = FALSE) {
  stopifnot(
    is.numeric(calculated_ages),
    is.numeric(known_ages),
    is.numeric(max_delta), all(is.finite(max_delta)),
    is.numeric(age_range), all(is.finite(age_range))
  )

  calc_ages_label <- deparse(substitute(calculated_ages))
  known_ages_label <- deparse(substitute(known_ages))
  max_delta_label <- deparse(substitute(max_delta))

  tbl <- tibble::tibble(calculated_ages, known_ages, max_delta)
  tbl <- tbl[(tbl$known_ages >= min(age_range)) & (tbl$known_ages <= max(age_range)), ]
  if(na.rm) {
    tbl <- tbl[is.finite(tbl$calculated_ages), ]
  }
  if(nrow(tbl) == 0) abort("Zero ages to compare")

  testthat::expect_true(
    all(abs(tbl$calculated_ages - tbl$known_ages) <= tbl$max_delta),
    label = sprintf(
      "abs((%s) - (%s)) <= %s (from age %s-%s)",
      calc_ages_label, known_ages_label, max_delta_label, min(age_range), max(age_range)
    )
  )
}

integrate_trapezoid <- function(x, y, xout = x, from = c("left", "right")) {
  from <- match.arg(from)
  if (from == "right") {
    return(integrate_trapezoid(-x, y, xout = -xout))
  }

  constant <- with_errors(0)
  x_order <- order(x)
  y <- with_errors(y[x_order])

  area <- (y[-1] + y[-length(y)]) / with_errors(2) * with_errors(diff(x[x_order]))
  cumulative_area <- c(constant, constant + cumsum(area))
  approx_error(x[x_order], cumulative_area, xout = xout)
}

approx_error <- function(x, y, xout) {
  value <- stats::approx(without_errors(x), without_errors(y), xout = xout)$y
  error <- extract_errors(y)[match(xout, x)]
  with_errors(value, error)
}

approx_no_error <- function(x, y, xout) {
  stats::approx(x, y, xout = xout)$y
}

with_errors <- function(x, error = NA_real_) {
  if(inherits(x, "errors") && any(!is.na(error))) {
    warning("Two errors included. Using error internal to x.")
    x
  } else if(inherits(x, "errors")) {
    x
  } else {
    set_errors(x, error)
  }
}

without_errors <- function(x) {
  if(inherits(x, "errors")) {
    drop_errors(x)
  } else {
    x
  }
}

extract_errors <- function(x, default = NA_real_) {
  if(inherits(x, "errors") && any(!is.na(default))) {
    warn("Two errors included. Using error internal to x.")
    errors(x)
  } else if(inherits(x, "errors")) {
    errors(x)
  } else {
    default
  }
}

check_mass_and_activity <- function(cumulative_dry_mass, excess) {
  stopifnot(
    is.numeric(cumulative_dry_mass),
    all(is.finite(cumulative_dry_mass)),
    all(cumulative_dry_mass >= 0),
    all(diff(cumulative_dry_mass) > 0),
    is.numeric(excess),
    sum(is.finite(excess) & (excess > 0)) >= 3,
    length(cumulative_dry_mass) == length(excess),
    all(excess >= 0, na.rm = TRUE)
  )
}
