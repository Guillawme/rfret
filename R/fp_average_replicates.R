#' @title Average technical replicates from a fluorescence polarization or anisotropy experiment
#'
#' @description This function calculates averages of fluorescence polarization
#'     or anisotropy values from an arbitrary number of technical replicates.
#'
#' @param raw_data A dataframe containing the fluorescence polarization or
#'     anisotropy data. It must contain at least 8 columns named:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment.}
#'     \item{Type}{For example, "titration".}
#'     \item{Replicate}{A number identifying the technical replicate (1, 2, etc.).}
#'     \item{Observation}{A number identifying each observation in a titration
#'     series (corresponds to the plate column numbers, if experiments are set
#'     up as rows in a 384-well plate). The number of observations for an
#'     experiment and its blanks must match, and a given observation number must
#'     associate data points at the same concentration in the titration series.}
#'     \item{concentration}{The ligand concentration in the titration series.}
#'     \item{polarization}{Fluorescence polarization.}
#'     \item{anisotropy}{Fluorescence anisotropy.}
#'     \item{intensity}{Fluorescence intensity.}
#'     }
#'     The output of \code{\link{format_data}} can be used directly as input
#'     for this function.
#' @return A dataframe containing the reduced dataset after averaging across
#'     replicates. It contains all of the above columns \emph{except}
#'     \code{Replicate}, because it returns the average values over replicates.
#'
#' @seealso \code{\link{format_data}} to prepare datasets for use with
#'     \code{fp_average_replicates}.
#'
#' @importFrom magrittr %>%
#'
#' @export

fp_average_replicates <- function(raw_data) {
    raw_data %>%
        dplyr::group_by(Experiment, Type, Observation, concentration) %>%
        dplyr::summarise(polarization = mean(polarization),
                         anisotropy   = mean(anisotropy),
                         intensity    = mean(intensity))
}
