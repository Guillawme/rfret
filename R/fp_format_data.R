#' @title Format raw experimental fluorescence polarization or anisotropy binding data for subsequent processing
#'
#' @description This function formats data for subsequent processing by
#'     \code{\link{fp_average_replicates}}. It will accept all data files
#'     in a given directory, or any number of data files specified by their
#'     names, or any number of dataframes supplied as a named list, or even a
#'     single dataframe supplied as is.
#' @param input A directory name, a vector of file names, a single dataset
#'     already loaded in memory as a dataframe, or a list of datasets already
#'     loaded in memory as dataframes.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @param metadata_json A JSON file describing mappings between internal
#'     names and metadata information in the actual data files. It can contain
#'     the following entries:
#'     \describe{
#'     \item{\code{content}}{The name of the column describing sample content.
#'     Default value: \code{"Content"}.}
#'     \item{\code{concentration}}{The name of the column containing the
#'     concentration series. Default value: \code{"concentration"}.}
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
#'     \item{\code{titration}}{The name describing points of the titration
#'     series. Default value: \code{"titration"}.}
#'     \item{\code{baseline}}{The name describing points of the baseline series
#'     (fluorescent probe only, without titrant molecule). Default value:
#'     \code{"baseline"}.}
#'     \item{\code{buffer_only}}{The name describing points of the control
#'     series with only buffer. Default value: \code{"buffer_only"}.}}
#' @return A single dataframe with the combined input data, containing 8
#'     columns: \code{Experiment}, \code{Type}, \code{Replicate},
#'     \code{Observation}, \code{polarization}, \code{anisotropy},
#'     \code{intensity} and \code{concentration}. Rows with missing values
#'     will be dropped.
#' @examples
#' \dontrun{
#' fp_format_data("my_data_directory", 4)
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export

fp_format_data <- function(input = NULL,
                           skip_lines = 0,
                           metadata_json = NULL) {
    # Check if we need to read user-provided metadata (otherwise, defaults will
    # be used automatically). If we use non-default metadata, reset to default
    # after the function terminates.
    if(!is.null(metadata_json)) {
        default <- get_user_metadata(metadata_json)
        on.exit(assign(x = "metadata", value = default, envir = .rfret))
    }

    # Load data
    raw_data <- load_data(input, skip_lines)

    # Format data and return a single large dataframe
    raw_data %>%
        purrr::map2(.x = .,
                    .y = names(raw_data),
                    .f = fp_format_one_dataset) %>%
        dplyr::bind_rows() %>%
        tidyr::drop_na()
}

#' @title Format a single fluorescence polarization or anisotropy dataset for subsequent processing
#'
#' @description This internal function processes a single dataframe to generate
#'     the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{fp_average_replicates}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name The name of the corresponding experiment.
#' @return A dataframe containing 8 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{polarization},
#'     \code{anisotropy}, \code{intensity} and \code{concentration}.
#' @examples
#' \dontrun{
#' fp_format_one_dataset(my_data, "my_experiment")
#' }
#'
#' @importFrom magrittr %>%

fp_format_one_dataset <- function(raw_data, experiment_name) {
    # Generate an Experiment column, and retrieve experiment name here
    raw_data$Experiment <- experiment_name

    # Data formatting pipeline:
    # Generate columns Type and Replicate, based on column Content
    raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = stringr::str_split(rlang::UQ(.rfret$metadata$content),
                                                        pattern = stringr::fixed(".")),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate) %>%
        dplyr::ungroup() %>%
    # Generate an Observation column, to label observations across different
    # types of experiments (baseline and titration) and replicates
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation = rlang::UQ(.rfret$metadata$concentration) %>%
                          dplyr::desc() %>%
                          dplyr::row_number()
                      ) %>%
        dplyr::ungroup() %>%
    # Rename sample type identifyers
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$buffer_only)),
                                     "buffer_only")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$baseline)),
                                     "baseline")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$titration)),
                                     "titration")) %>%
    # Reorder columns and return dataset
        dplyr::select(Experiment,
                      Type,
                      Replicate,
                      Observation,
                      polarization  = rlang::UQ(.rfret$metadata$polarization),
                      anisotropy    = rlang::UQ(.rfret$metadata$anisotropy),
                      intensity     = rlang::UQ(.rfret$metadata$intensity),
                      concentration = rlang::UQ(.rfret$metadata$concentration)) %>%
        dplyr::mutate(Replicate = as.integer(Replicate))
}
