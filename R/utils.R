
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
pb210_core_area <- function(diameter = 0.063) {
  pi * (diameter / 2)^2
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
    is.numeric(known_ages), all(is.finite(known_ages)),
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
  if(nrow(tbl) == 0) stop("Zero ages to compare")

  testthat::expect_true(
    all(abs(tbl$calculated_ages - tbl$known_ages) <= tbl$max_delta),
    label = sprintf(
      "abs((%s) - (%s)) <= %s (from age %s-%s)",
      calc_ages_label, known_ages_label, max_delta_label, min(age_range), max(age_range)
    )
  )
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
