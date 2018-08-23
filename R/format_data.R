#' @title Format raw experimental data for subsequent processing
#'
#' @description This function formats data for subsequent processing. It will
#'     accept all data files in a given directory, or any number of data files
#'     specified by their names, or any number of dataframes supplied as a named
#'     list, or even a single dataframe supplied as is.
#' @param input A directory name, a vector of file names, a single dataset
#'     already loaded in memory as a dataframe, or a list of datasets already
#'     loaded in memory as dataframes.
#' @param data_type A character string describing which type of data is supplied
#'     for pre-processing. Currently supported options are \code{"fret"} and
#'     \code{"fp"}.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @param metadata_json A JSON file describing mappings between internal
#'     names and metadata information in the actual data files. It can contain
#'     the following entries, independent of the type of data:
#'     \describe{
#'     \item{\code{content}}{The name of the column describing sample content.
#'     Default value: \code{"Content"}.}
#'     \item{\code{concentration}}{The name of the column containing the
#'     concentration series. Default value: \code{"concentration"}.}
#'     \item{\code{titration}}{The name describing points of the titration
#'     series. Default value: \code{"titration"}.}
#'     \item{\code{buffer_only}}{The name describing points of the control
#'     series with only buffer. Default value: \code{"buffer_only"}.}}
#'     A metadata file for FRET datasets can also contain the following entries:
#'     \describe{
#'     \item{\code{fret_channel}}{The name of the column containing fluorescence
#'     intensities from the FRET channel. Default value: \code{"fret_channel"}.}
#'     \item{\code{acceptor_channel}}{The name of the column containing
#'     fluorescence intensities from the acceptor channel. Default value:
#'     \code{"acceptor_channel"}.}
#'     \item{\code{donor_channel}}{The name of the column containing fluorescence
#'     intensities from the donor channel. Default value: \code{"donor_channel"}.}
#'     \item{\code{acceptor_only}}{The name describing points of the control
#'     series with only acceptor. Default value: \code{"acceptor_only"}.}
#'     \item{\code{donor_only}}{The name describing points of the control series
#'     with only donor. Default value: \code{"donor_only"}.}}
#'     A metadata file for FP/FA datasets can also contain the following entries:
#'     \describe{
#'     \item{\code{parallel}}{The name of the column containing fluorescence
#'     intensities from the parallel channel. Default value: \code{"parallel"}.}
#'     \item{\code{perpendicular}}{The name of the column containing
#'     fluorescence intensities from the perpendicular channel. Default value:
#'     \code{"perpendicular"}.}
#'     \item{\code{polarization}}{The name of the column containing fluorescence
#'     polarization values, if already calculated by the instrument. Default
#'     value: \code{"polarization"}.}
#'     \item{\code{anisotropy}}{The name of the column containing fluorescence
#'     anisotropy values, if already calculated by the instrument. Default
#'     value: \code{"anisotropy"}.}
#'     \item{\code{intensity}}{The name of the column containing total
#'     fluorescence intensity values, if already calculated by the instrument.
#'     Default value: \code{"intensity"}.}
#'     \item{\code{baseline}}{The name describing points of the baseline series
#'     (fluorescent probe only, without titrant molecule). Default value:
#'     \code{"baseline"}.}}
#' @return A single dataframe with the combined input data, containing 8
#'     columns: \code{Experiment}, \code{Type}, \code{Replicate},
#'     \code{Observation}, \code{fret_channel}, \code{acceptor_channel},
#'     \code{donor_channel} and \code{concentration}. Rows with missing values
#'     will be dropped.
#'
#' @importFrom magrittr %>%
#'
#' @export

format_data <- function(input,
                        data_type,
                        skip_lines = 0,
                        metadata_json = NULL) {
    # Stop if the type of data is not supported, and choose the appropriate
    # pre-processing function according to the type of data.
    pre_process <- switch(data_type,
                          "fret" = fret_format_one_dataset,
                          "fp" = fp_format_one_dataset,
                          stop("Unsupported data type: ", data_type, "\nCurrently supported data types: fret, fp."))

    # Check if we need to read user-provided metadata (otherwise, defaults will
    # be used automatically). If we use non-default metadata, reset to default
    # after the function terminates.
    if (!is.null(metadata_json)) {
        default <- get_user_metadata(metadata_json)
        on.exit(assign(x = "metadata", value = default, envir = .rfret))
    }

    # Load data
    raw_data <- load_data(input, skip_lines)

    # Format data and return a single large dataframe
    raw_data %>%
        purrr::map2(.x = .,
                    .y = names(raw_data),
                    .f = pre_process) %>%
        dplyr::bind_rows() %>%
        tidyr::drop_na()
}
