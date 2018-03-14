#' @title Choose which signal to fit between fluorescence polarization and anisotropy
#'
#' @description This function renames the appropriate column in a fluorescence
#'     polarization and anisotropy dataset for subsequent fitting by
#'     \code{\link{fit_binding_model}}.
#'
#' @param data A dataframe containing the fluorescence polarization and
#'     anisotropy data. It must contain at least 7 columns named:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment.}
#'     \item{Type}{For example, "titration".}
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
#'     The output of \code{\link{fp_average_replicates}} can be used directly as
#'     input for this function.
#' @param sig A character string to identify the signal to use for curve fitting.
#'     Possible values are \code{"polarization"} or \code{"p"} to use fluorescence
#'     polarization signal, and \code{"anisotropy"} or \code{"a"} to use
#'     fluorescence anisotropy signal.
#' @return A dataframe containing the same dataset with columns appropriately
#'     named for \code{\link{fit_binding_model}}.
#'
#' @seealso \code{\link{format_data}} and \code{\link{fp_average_replicates}}.
#'
#' @export

fp_use_signal <- function(data, sig = NULL) {
    # Sanity checks
    if (!(sig %in% c("polarization", "anisotropy", "p", "a"))) {
        stop("Available signals: 'polarization' (or 'p' for short), 'anisotropy' (or 'a' for short).")
    }

    # Rename appropriate column to 'signal' for use by fit_binding_model
    if (sig %in% c("polarization", "p")) {
        return(dplyr::rename(data, signal = polarization))
    }
    if (sig %in% c("anisotropy", "a")) {
        return(dplyr::rename(data, signal = anisotropy))
    }
}
