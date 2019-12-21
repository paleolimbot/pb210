
#' Calculate cumulative mass
#'
#' @param masses A vector of sequential masses
#' @param position 0 for the cumulative mass at the top of the section
#'   and 1 for the cumulative mass at the bottom of the section (0.5
#'   for the middle, which is the default).
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
#' pb210_cumulative_mass(c(0.1, 0.1, 0.1, 0.1), position = 1)
#'
pb210_cumulative_mass <- function(masses, position = 0.5) {
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

#' Estimate dry density from water and organic content
#'
#' @inheritParams pb210_core_area
#' @param slice_mass The mass of a sediment slice, in kg.
#' @param bulk_density The density of a sediment slice, in kg/m^3.
#' @param thickness The thickness of the sediment slice, in m.
#' @param water_content The proportion of the slice made up of water
#'   as a proportion of wet mass. Usually this is obtained as mass lost
#'   after drying a sample.
#' @param organic_content The proportion of the slice made up of organic material
#'   as a proportion of dry mass. Usually this is obtained as mass lost after
#'   ignition at 500-600 degrees C (Meyers and Teranes 2001).
#' @param density_organic The assumed density of (perfectly compacted) organic material.
#'   The default is 1570 kg/m^3^, which is a measured value of gyttja particle density
#'   as reported by Malloy and Price (2017).
#' @param density_inorganic The assumed density of (perfectly compacted) inorganic material.
#'   The default is 2830 kg/m^3^, which is the average crustal density as calculated by
#'   Christensen and Mooney (1995).
#' @param density_water The density of water, in kg/m^3^. Use to adjust this density
#'   in the unlikely event that you are dealing with non-standard temperature and/or
#'   pressures.
#'
#' @references
#' Christensen, N.I., and Mooney, W.D. 1995. Seismic velocity structure and composition of the
#' continental crust: A global view. Journal of Geophysical Research: Solid Earth, 100: 9761–9788.
#' <https://doi.org/10.1029/95JB00259>
#'
#' Malloy, S., and Price, J.S. 2017. Consolidation of gyttja in a rewetted fen peatland:
#' Potential implications for restoration. Mires and Peat,: 1–15.
#' <https://doi.org/10.19189/MaP.2015.OMB.200>
#'
#' Meyers, P.A., and Teranes, J.L. 2001. Sediment organic matter.
#' In Tracking Environmental Change Using Lake Sediments: Volume 2: Physical and Geochemical Methods.
#' Edited by W.M. Last and J.P. Smol. Kluwer Academic Publishers, The Netherlands.
#' pp. 239–269. <https://doi.org/10.1007/0-306-47670-3_9>
#'
#' @export
#'
#' @examples
#' # density of a 10 g slice that is 1 cm thick for a 6.3-cm diameter core
#' pb210_bulk_density(
#'   slice_mass = 0.010,
#'   thickness = 0.01,
#'   core_area = pb210_core_area(0.063)
#' )
#'
pb210_bulk_density <- function(slice_mass, thickness, core_area = pb210_core_area()) {
  slice_mass / pb210_slice_volume(thickness, core_area)
}

#' @rdname pb210_bulk_density
#' @export
pb210_bulk_density_estimate <- function(water_content, organic_content, density_water = 1000,
                                         density_organic = 1570, density_inorganic = 2830) {
  pb210_density_slice_from_density_solid(
    water_content,
    density_solid = pb210_density_solid(
      organic_content,
      density_organic = density_organic,
      density_inorganic = density_inorganic
    ),
    density_water = density_water
  )
}

#' @rdname pb210_bulk_density
#' @export
pb210_slice_mass <- function(bulk_density, thickness, core_area = pb210_core_area()) {
  bulk_density * thickness * core_area
}

#' @rdname pb210_bulk_density
#' @export
pb210_porosity <- function(water_content, organic_content, density_water = 1000,
                           density_organic = 1600, density_inorganic = 2830) {
  pb210_porosity_from_density_solid(
    water_content,
    density_solid = pb210_density_solid(
      organic_content,
      density_organic = density_organic,
      density_inorganic = density_inorganic
    ),
    density_water = density_water
  )
}

pb210_density_solid <- function(organic_content, density_organic, density_inorganic) {
  stopifnot(
    all(organic_content >= 0), all(organic_content <= 1)
  )

  # element-wise weighted average between organic and inorganic densities
  # kg/m^3 <- 1 * kg/m^3 + 1 * kg/m^3
  inorganic_content <- 1 - organic_content
  density_organic * organic_content + density_inorganic * inorganic_content
}


pb210_density_slice_from_density_solid <- function(water_content, density_solid, density_water)  {
  porosity <- pb210_porosity_from_density_solid(
    water_content = water_content,
    density_solid = density_solid,
    density_water = density_water
  )

  pb210_density_slice_from_porosity(
    porosity,
    density_solid = density_solid,
    density_water = density_water
  )
}

pb210_density_slice_from_porosity <- function(porosity, density_solid, density_water) {
  stopifnot(
    all(porosity >= 0), all(porosity <= 1)
  )

  volume_total <- 1 # m^3
  volume_water <- porosity * volume_total
  volume_solid <- volume_total - volume_water # m^3
  solid_mass <- density_solid * volume_solid # kg
  solid_mass / volume_total # kg / m^3
}

pb210_porosity_from_density_solid <- function(water_content, density_solid, density_water) {
  stopifnot(
    all(water_content >= 0), all(water_content <= 1)
  )

  # derived from the system of equations
  # (volume solid) + (volume water) = (volume total)
  # (water_content) = (density_water * volume_water) / (density_total * volume_total
  # (solid_content) = (density_solid * volume_solid) / (density_total * volume_total)
  # porosity = volume_water / volume_solid

  solid_content <- 1 - water_content
  (water_content * density_solid) / (water_content * density_solid + solid_content * density_water)
}
