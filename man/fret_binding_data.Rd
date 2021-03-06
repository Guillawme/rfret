% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{fret_binding_data}
\alias{fret_binding_data}
\title{A FRET binding assay dataset}
\format{A data frame with 144 rows and 7 columns. Rows contain two replicates
of three titrations series, each containing 24 data points. Columns are:
\describe{
  \item{Well Row}{The row where the sample corresponding to this record was
  located into the 384-well microplate.}
  \item{Well Col}{The column where the sample corresponding to this record
  was located into the 384-well microplate.}
  \item{Content}{A word describing the experiment and replicate this
  particular data point belongs to (\code{acceptor_only.1},
  \code{acceptor_only.2}, \code{donor_only.1}, \code{donor_only.2}
  \code{titration.1}, \code{titration.2}).}
  \item{fret_channel}{The fluorescence intensities measured in the FRET
  channel.}
  \item{acceptor_channel}{The fluorescence intensities measured in the
  acceptor channel.}
  \item{donor_channel}{The fluorescence intensities measured in the donor
  channel.}
  \item{concentration}{The concentrations of macromolecule in the titration
  series.}
}}
\source{
Guillaume Gaullier
}
\usage{
fret_binding_data
}
\description{
This dataset was collected on June 2nd, 2017. It is a protein-DNA interaction
assay, where the protein is labeled with a donor fluorophore and the DNA with
an acceptor fluorophore. This dataset yields a binding constant of 4.3 +/-
0.5 (using the hyperbolic model).
}
\keyword{datasets}
