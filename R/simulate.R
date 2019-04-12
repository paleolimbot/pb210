
pb210_simulate_accumulation <- function(max_age = 300, time_step = 1,
                                        mass_accumulation = pb210_mass_accumulation_constant(),
                                        compressibility = pb210_compressibility_constant(),
                                        density = pb210_density_constant(),
                                        percent_water = pb210_water_content_constant()) {

}

pb210_simulate_core <- function(age_simulation = pb210_simulate_accumulation(),
                                depth_top = seq(0, 30, by = 0.5),
                                depth_bottom = depth_top + 0.5,
                                core_area = pb210_core_area()) {

}

#' Parameter generators for mass accumulation rates
#'
#' @param value,mean,initial An accumulation rate, in kg / m2 / year.
#' @param sd For random normal accumulation rates, the standard deviation
#' @param mean_slope,sd_slope The mean and standard deviation of the change in
#'   slope, in kg / m2 / year / year
#'
#' @return A function of a single parameter, `age`, which is a (decreasing) vector of ages.
#' @export
#'
#' @examples
#' age_compare <- tibble::tibble(
#'   ages = 150:0,
#'   pb210_mass_accumulation_constant()(ages),
#'   pb210_mass_accumulation_rnorm()(ages),
#'   pb210_mass_accumulation_rnorm_trend()(ages)
#' )
#'
pb210_mass_accumulation_constant <- function(value = 0.150)  {
  force(value)
  function(age) {
    value
  }
}

#' @rdname pb210_mass_accumulation_constant
#' @export
pb210_mass_accumulation_rnorm <- function(mean = 0.150, sd = 0.020) {
  force(mean)
  force(sd)
  function(age) {
    stats::rnorm(length(age), mean = 150, sd = 20)
  }
}

#' @rdname pb210_mass_accumulation_constant
#' @export
pb210_mass_accumulation_rnorm_trend <- function(initial = 0.150, mean_slope = 0, sd_slope = 0.005) {
  force(initial)
  force(mean_slope)
  force(sd_slope)
  function(age) {
    rev(cumsum(c(initial, stats::rnorm(length(age) - 1, mean = mean_slope, sd = sd_slope))))
  }
}

#' Parameter generators for density values
#'
#' Note that this refers to dry density at the time of deposition.
#'
#' @param value A density at the time of deposition, in kg / m3
#'
#' @return A function of a single parameter, `age`, which is a (decreasing) vector of ages.
#' @export
#'
#' @examples
#' age_compare <- tibble::tibble(
#'   ages = 150:0,
#'   pb210_density_constant()(ages)
#' )
#'
pb210_density_constant <- function(value = 1500)  {
  force(value)
  function(age) {
    value
  }
}

#' Parameter generators for water content
#'
#' Note that this refers to water content at the time of deposition.
#'
#' @param value A water content at the time of deposition, unitless (mass water / mass sample)
#'
#' @return A function of a single parameter, `age`, which is a (decreasing) vector of ages.
#' @export
#'
#' @examples
#' age_compare <- tibble::tibble(
#'   ages = 150:0,
#'   pb210_water_content_constant()(ages)
#' )
#'
pb210_water_content_constant <- function(value = 0.95)  {
  force(value)
  function(age) {
    value
  }
}

#' Parameter generators for compressibility
#'
#' Note that this refers to compressibility at the time of deposition.
#'
#' @param value A compressibility at the time of deposition, in 1/Pa.
#'   Lower values indicate a higher propensity for the material to be compressed.
#'
#' @return A function of a single parameter, `age`, which is a (decreasing) vector of ages.
#' @export
#'
#' @examples
#' age_compare <- tibble::tibble(
#'   ages = 150:0,
#'   pb210_compressibility_constant()(ages)
#' )
#'
pb210_compressibility_constant <- function(value = 2e-6)  {
  force(value)
  function(age) {
    value
  }
}
