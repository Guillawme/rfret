#' @title Average technical replicates
#'
#' @description This function calculates averages of fluorescence values from
#'     an arbitrary number of technical replicates.
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     contain at least 8 columns named:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment.}
#'     \item{Type}{Either "blank" (no donor) or "titration" (donor present).}
#'     \item{Replicate}{A number identifying the technical replicate (1, 2, etc.).}
#'     \item{Observation}{A number identifying each observation in a titration
#'     series (corresponds to the plate column numbers, if experiments are set
#'     up as rows in a 384-well plate). The number of observations for an
#'     experiment and its blanks must match, and a given observation number must
#'     associate data points at the same concentration in the titration series.}
#'     \item{concentration}{The ligand concentration in the titration series.}
#'     \item{fret_channel}{Fluorescence intensity in the FRET channel.}
#'     \item{acceptor_channel}{Fluorescence intensity in the acceptor channel.}
#'     \item{donor_channel}{Fluorescence intensity in the donor channel.}
#'     }
#'     The output of \code{\link{format_data}} can be used directly as input for
#'     this function.
#' @return A dataframe containing the reduced dataset after averaging across
#'     replicates. It contains all of the above columns \emph{except}
#'     \code{Replicate}, because it returns the average values over replicates.
#'
#' @seealso \code{\link{format_data}} to prepare datasets for use with
#'     \code{average_technical_replicates}.
#'
#' @export

average_technical_replicates <- function(raw_data) {
  raw_data %>%
    dplyr::group_by(Experiment, Type, Observation, concentration) %>%
    dplyr::summarise(fret = mean(fret_channel),
                     acceptor = mean(acceptor_channel),
                     donor = mean(donor_channel))
}
