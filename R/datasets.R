#' A good FRET binding assay dataset
#'
#' This dataset was collected on March 16th, 2017. It is a protein-protein
#' interaction assay, where one of the two proteins is labeled with Alexa 488
#' (donor fluorophore) and the other with Atto 647 (acceptor fluorophore). This
#' dataset yields a binding curve and a binding constant of 48 +/- 6.
#'
#' @format A data frame with 96 rows and 7 columns. Rows contain two replicates
#' of two experiments each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well.Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well.Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{blank_1}, \code{blank_2},
#'   \code{titration_1}, \code{titration_2}).}
#'   \item{fret_channel}{The fluorescence intensities measured in the FRET
#'   channel.}
#'   \item{acceptor_channel}{The fluorescence intensities measured in the
#'   acceptor channel.}
#'   \item{donor_channel}{The fluorescence intensities measured in the donor
#'   channel.}
#'   \item{concentration}{The concentrations of acceptor-labeled protein in the
#'   titration series.}
#' }
#' @source Guillaume Gaullier
"fret_good"

#' A FRET binding assay dataset with outliers in the donor channel
#'
#' This dataset was collected on February 24th, 2017. It is a protein-protein
#' interaction assay, where one of the two proteins is labeled with Alexa 488
#' (donor fluorophore) and the other with Atto 647 (acceptor fluorophore). This
#' dataset has outliers in the donor channel, but can still yield a binding
#' curve and a binding constant of 147 +/- 41.
#'
#' @format A data frame with 96 rows and 7 columns. Rows contain two replicates
#' of two experiments each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well.Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well.Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{blank_1}, \code{blank_2},
#'   \code{titration_1}, \code{titration_2}).}
#'   \item{fret_channel}{The fluorescence intensities measured in the FRET
#'   channel.}
#'   \item{acceptor_channel}{The fluorescence intensities measured in the
#'   acceptor channel.}
#'   \item{donor_channel}{The fluorescence intensities measured in the donor
#'   channel.}
#'   \item{concentration}{The concentrations of acceptor-labeled protein in the
#'   titration series.}
#' }
#' @source Guillaume Gaullier
"fret_donor_outliers"

#' A FRET binding assay dataset with outliers in the acceptor channel
#'
#' This dataset was collected on March 13th, 2017. It is a protein-protein
#' interaction assay, where one of the two proteins is labeled with Alexa 488
#' (donor fluorophore) and the other with Atto 647 (acceptor fluorophore). This
#' dataset has outliers in the acceptor channel, but can still yield a binding
#' curve and a binding constant of 63 +/- 10.
#'
#' @format A data frame with 96 rows and 7 columns. Rows contain two replicates
#' of two experiments each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well.Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well.Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{blank_1}, \code{blank_2},
#'   \code{titration_1}, \code{titration_2}).}
#'   \item{fret_channel}{The fluorescence intensities measured in the FRET
#'   channel.}
#'   \item{acceptor_channel}{The fluorescence intensities measured in the
#'   acceptor channel.}
#'   \item{donor_channel}{The fluorescence intensities measured in the donor
#'   channel.}
#'   \item{concentration}{The concentrations of acceptor-labeled protein in the
#'   titration series.}
#' }
#' @source Guillaume Gaullier
"fret_acceptor_outliers"

#' A bad FRET binding assay dataset
#'
#' This dataset was collected on March 16th, 2017. It is a protein-protein
#' interaction assay, where one of the two proteins is labeled with Alexa 488
#' (donor fluorophore) and the other with Atto 647 (acceptor fluorophore). This
#' dataset doesn't yield any binding curve nor binding constant. The reasons for
#' that are because the concentrations of donor- and acceptor-labeled proteins
#' are not what they should be (possibly due to protein agregation, or proteins
#' sticking to the microplate plastic), and the microplate used has an
#' unusually high auto-fluorescence background in the donor and FRET channels.
#'
#' @format A data frame with 96 rows and 7 columns. Rows contain two replicates
#' of two experiments each containing 24 data points. Columns are:
#' \describe{
#'   \item{Well.Row}{The row where the sample corresponding to this record was
#'   located into the 384-well microplate.}
#'   \item{Well.Col}{The column where the sample corresponding to this record
#'   was located into the 384-well microplate.}
#'   \item{Content}{A word describing the experiment and replicate this
#'   particular data point belongs to (\code{blank_1}, \code{blank_2},
#'   \code{titration_1}, \code{titration_2}).}
#'   \item{fret_channel}{The fluorescence intensities measured in the FRET
#'   channel.}
#'   \item{acceptor_channel}{The fluorescence intensities measured in the
#'   acceptor channel.}
#'   \item{donor_channel}{The fluorescence intensities measured in the donor
#'   channel.}
#'   \item{concentration}{The concentrations of acceptor-labeled protein in the
#'   titration series.}
#' }
#' @source Guillaume Gaullier
"fret_bad"
