% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/background.R
\name{pb210_excess}
\alias{pb210_excess}
\title{Calculate excess (unsupported) lead-210}
\usage{
pb210_excess(activity, background = 0)
}
\arguments{
\item{activity}{A vector of measured lead-210 specific activities (in Bq/kg) and
associated error. These can have \code{\link[errors:errors]{errors::errors()}}.}

\item{background}{A vector of estimated background
lead-210 specific activity (in Bq/kg) and associated error.
These can have \code{\link[errors:errors]{errors::errors()}}.}
}
\value{
A vector with \code{\link[errors:errors]{errors::errors()}} of the excess lead-210 specific activity.
Background is determined by the first point at which \code{activity} is less than
or equal to \code{background}. All excess values are set to \code{NA} below (after)
this point.
}
\description{
Boath \code{activitiy} and  \code{background} should be arranged such that the
top of the core is first.
}
\examples{
core <- pb210_simulate_core(depth_step = rep(1, 30)) \%>\%
  pb210_simulate_counting()

pb210_excess(
  set_errors(
    core$activity_estimate,
    core$activity_se
  ),
  background = 10
)

}
