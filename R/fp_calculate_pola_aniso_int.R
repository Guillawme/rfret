#' @title Calculate fluorescence polarization and anisotropy from raw data
#'
#' @description This function calculates fluorescence polarization and
#'     anisotropy values from the raw data of parallel and perpendicularly
#'     polarized channels.
#'
#' @param raw_data A dataframe containing the raw data. It must contain at least
#'     7 columns named:
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
#'     \item{\code{parallel}}{The name of the column containing fluorescence
#'     intensities from the parallel channel.}
#'     \item{\code{perpendicular}}{The name of the column containing fluorescence
#'     intensities from the perpendicular channel.}
#'     }
#'     The output of \code{\link{format_data}} can be used directly as input
#'     for this function.
#' @return A dataframe containing the same columns as the input dataframe, plus
#'     three columns called \code{polarization}, \code{anisotropy} and
#'     \code{intensity}.
#'
#' @seealso \code{\link{format_data}} to prepare datasets for use with
#'     \code{fp_calculate_pola_aniso_int}.
#'
#' @importFrom magrittr %>%
#'
#' @export

fp_calculate_pola_aniso_int <- function(raw_data) {
    raw_data %>%
        dplyr::group_by(Experiment, Type, Observation, concentration) %>%
        dplyr::mutate(polarization = (parallel - perpendicular) / (parallel + perpendicular),
                      anisotropy   = (parallel - perpendicular) / (parallel + 2 * perpendicular),
                      intensity    = parallel + 2 * perpendicular)
}
