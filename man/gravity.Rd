% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{gravity}
\alias{gravity}
\title{Spatial Accessibility via Gravity Method}
\usage{
gravity(dist_table, supply, demand, dist, gravity_exp, supply_vol, demand_vol)
}
\arguments{
\item{dist_table}{A data frame containing an origin-destination (OD) table, with additional columns for supply and demand volumes.}

\item{supply}{The name of the column in `dist_table` that uniquely identifies supply points (hospitals, physicians, etc.).}

\item{demand}{The name of the column in `dist_table` that uniquely identifies demand points (census tracts, residences, etc.).}

\item{dist}{The name of the column in `dist_table` that contains the distance between the `supply` and the `demand`.}

\item{gravity_exp}{The exponent to use for the inverse distance function.}

\item{supply_vol}{The name of the column in `dist_table` that contains supply volumes (# of hospital beds, # of physicians, etc.).}

\item{demand_vol}{The name of the column in `dist_table` that contains demand volumes (population counts, etc.).}
}
\value{
A data frame.
}
\description{
Luo & Wang 2003, DOI:10.1068/b29120
}
