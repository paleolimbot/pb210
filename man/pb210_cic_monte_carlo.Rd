% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monte-carlo.R
\name{pb210_cic_monte_carlo}
\alias{pb210_cic_monte_carlo}
\alias{pb210_crs_monte_carlo}
\alias{predict.pb210_fit_cic_monte_carlo}
\alias{predict.pb210_fit_crs_monte_carlo}
\title{Run a Monte-Carlo simulation on a CRS or CIC fit}
\usage{
pb210_cic_monte_carlo(
  cumulative_dry_mass,
  activity,
  background = 0,
  model_top = ~pb210_fit_exponential(..1, ..2),
  decay_constant = pb210_decay_constant(),
  n = 1000,
  sample_activity = pb210_sample_norm,
  sample_background = pb210_sample_norm,
  sample_decay_constant = pb210_sample_norm
)

pb210_crs_monte_carlo(
  cumulative_dry_mass,
  activity,
  background = 0,
  inventory = pb210_inventory_calculator(),
  core_area = pb210_core_area(),
  decay_constant = pb210_decay_constant(),
  n = 1000,
  sample_activity = pb210_sample_norm,
  sample_background = pb210_sample_norm,
  sample_decay_constant = pb210_sample_norm
)

\method{predict}{pb210_fit_cic_monte_carlo}(object, cumulative_dry_mass = NULL, ...)

\method{predict}{pb210_fit_crs_monte_carlo}(object, cumulative_dry_mass = NULL, ...)
}
\arguments{
\item{cumulative_dry_mass}{The cumulative dry mass of the core (in kg), starting at the
surface sample and including all samples in the core.
These must be greater than 0 and in increasing order.}

\item{activity}{A vector of measured lead-210 specific activities (in Bq/kg) and
associated error. These can have \code{\link[errors:errors]{errors::errors()}}.}

\item{background}{A vector of estimated background
lead-210 specific activity (in Bq/kg) and associated error.
These can have \code{\link[errors:errors]{errors::errors()}}.}

\item{model_top}{A fit object, such as one generated by \code{\link[=pb210_fit_exponential]{pb210_fit_exponential()}} or a
constant specifying the surface \code{excess}. The choice of this value has
considerable impact on young dates.}

\item{decay_constant}{The decay contstant for lead-210 (in 1/years). This is an argument
rather than a constant because we have found that different spreadsheets in the wild
use different decay constants. See \code{\link[=pb210_decay_constant]{pb210_decay_constant()}}.}

\item{n}{The number of permutations. The default is 1,000, as
Sanchez-Cabeza et al. (2014) found that this was the minimum
number of iterations needed for Monte-Carlo uncertainty to
converge on the quadrature-propagated uncertainty. In general,
Sanchez-Cabeza et al. (2014) used n values from 1,000 to 4,000.}

\item{sample_activity, sample_background, sample_decay_constant}{Random
sampler functions such as \code{\link[=pb210_sample_norm]{pb210_sample_norm()}} that are called
\code{n} times for the appropriate argument.}

\item{inventory}{The cumulative excess lead-210 activity (in Bq), starting at the bottom
of the core. By default, this is estimated by the default \code{\link[=pb210_inventory_calculator]{pb210_inventory_calculator()}}.
If specifying a vector of values, ensure  that the surface (0 cumulative mass) value is
specified.}

\item{core_area}{The internal area of the corer (in m^2^). This can be calculated
from an internal diameter using \code{\link[=pb210_core_area]{pb210_core_area()}}.}

\item{object}{A fit object generated by \code{\link[=pb210_crs]{pb210_crs()}} or \code{\link[=pb210_cic]{pb210_cic()}}.}

\item{...}{Unused.}
}
\description{
These functions run many simulations on randomly-sampled activity values
constrained by the measured activity and estimated background. Excess
is calculated by \code{\link[=pb210_excess]{pb210_excess()}} for each simulation. Prediction
results are presented as the median result and are constrained by
min (5th percentile) and max (95th percentile) values (instead of
quadrature-propagated error like \code{\link[=pb210_cic]{pb210_cic()}} and \code{\link[=pb210_crs]{pb210_crs()}}). Note
that this may take 10 seconds per 1,000 iterations (depending on
hardware).
}
\examples{
# simulate a core
core <- pb210_simulate_core() \%>\%
  pb210_simulate_counting()

# calculate ages using the CRS model
crs <- pb210_crs_monte_carlo(
  pb210_cumulative_mass(core$slice_mass),
  set_errors(
    core$activity_estimate,
    core$activity_se
  ),
  n = 100
)

predict(crs)

}
\references{
Binford, M.W. 1990. Calculation and uncertainty analysis of ^210^Pb dates for
PIRLA project lake sediment cores. Journal of Paleolimnology, 3: 253–267.
\url{https://doi.org/10.1007/BF00219461}

Sanchez-Cabeza, J.-A., Ruiz-Fernández, A.C., Ontiveros-Cuadras, J.F.,
Pérez Bernal, L.H., and Olid, C. 2014. Monte Carlo uncertainty calculation of ^210^Pb
chronologies and accumulation rates of sediments and peat bogs.
Quaternary Geochronology, 23: 80–93. \url{https://doi.org/10.1016/j.quageo.2014.06.002}
}
