#' A FRET binding assay dataset
#'
#' This dataset was collected on June 2nd, 2017. It is a protein-DNA interaction
#' assay, where the protein is labeled with a donor fluorophore and the DNA with
#' an acceptor fluorophore. This dataset yields a binding constant of 4.3 +/-
#' 0.5 (using the hyperbolic model).
#'
#' @format A data frame with 144 rows and 7 columns. Rows contain two replicates
#' of three titrations series, each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{acceptor_only.1},
#'   \code{acceptor_only.2}, \code{donor_only.1}, \code{donor_only.2}
#'   \code{titration.1}, \code{titration.2}).}
#'   \item{fret_channel}{The fluorescence intensities measured in the FRET
#'   channel.}
#'   \item{acceptor_channel}{The fluorescence intensities measured in the
#'   acceptor channel.}
#'   \item{donor_channel}{The fluorescence intensities measured in the donor
#'   channel.}
#'   \item{concentration}{The concentrations of macromolecule in the titration
#'   series.}
#' }
#' @source Guillaume Gaullier
"fret_binding_data"

#' A fluorecence polarization/anisotropy binding assay dataset
#'
#' This dataset was collected on July 26th, 2017. It is a protein-DNA interaction
#' assay, where the DNA is labeled with a fluorophore. This dataset yields a
#' binding constant of 13.28 +/- 0.58 (using the hyperbolic model).
#'
#' @format A data frame with 48 rows and 9 columns. Rows contain two replicates
#' of a titration series, each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{titration.1},
#'   \code{titration.2}, \code{baseline.1}, \code{baseline.2}).}
#'   \item{Raw Data (parallel)}{The fluorescence intensities measured in the
#'   parallel polarization plane.}
#'   \item{Raw Data (perpendicular)}{The fluorescence intensities measured in the
#'   perpendicular polarization plane.}
#'   \item{Polarization}{The fluorescence polarization values, in mP.}
#'   \item{Anisotropy}{The fluorescence anisotropy values, in mA.}
#'   \item{Intensity}{The overall fluorescence intensity values.}
#'   \item{concentration}{The concentrations of macromolecule in the titration
#'   series.}
#' }
#' @source Guillaume Gaullier
"fp_binding_data"
