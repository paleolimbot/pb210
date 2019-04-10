
#' Apply the Constant Rate of Supply model
#'
#' @param cumulative_dry_mass The cumulative dry mass of the section top
#' @param excess_pb210 Excess (non-erosional) lead-210 activity
#' @param pb210_sd The standard deviation of the excess lead-210 value
#' @param internal_diameter The internal diameter of the corer, in cm
#'
#' @references
#' Appleby, P.G., and Oldfield, F. 1983. The assessment of 210Pb data from sites with
#' varying sediment accumulation rates.
#' Hydrobiologia, 103: 29–35. doi:10.1007/BF00028424.
#'
#' @return A model fit object
#' @export
#'
pb210_crs <- function(cumulative_dry_mass, excess_pb210, pb210_sd = NA, internal_diameter = 6.3) {

}

