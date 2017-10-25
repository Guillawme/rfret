#' @title Format raw experimental FRET binding data for subsequent processing
#'
#' @description This function formats data for subsequent processing by
#'     \code{\link{fret_average_replicates}}. It will accept all data files
#'     in a given directory, or any number of data files specified by their
#'     names, or any number of dataframes supplied as a named list, or even a
#'     single dataframe supplied as is.
#' @param input A directory name, a vector of file names, a single dataset
#'     already loaded in memory as a dataframe, or a list of datasets already
#'     loaded in memory as dataframes.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @return A single dataframe with the combined input data, containing 8
#'     columns: \code{Experiment}, \code{Type}, \code{Replicate},
#'     \code{Observation}, \code{fret_channel}, \code{acceptor_channel},
#'     \code{donor_channel} and \code{concentration}. Rows with missing values
#'     will be dropped.
#' @examples
#' \dontrun{
#' format_data("my_data_directory", 4)
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export

fret_format_data <- function(input = NULL, skip_lines = 0) {
    # Load data
    raw_data <- load_data(input, skip_lines)

    # Format data and return a single large dataframe
    raw_data %>%
        mapply(fret_format_one_dataset,
               .,
               names(raw_data),
               SIMPLIFY = FALSE) %>%
        dplyr::bind_rows() %>%
        tidyr::drop_na()
}

#' @title Format a single dataframe for subsequent processing
#'
#' @description This internal function processes a single dataframe to generate
#'     the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{fret_average_replicates}} and
#'     \code{\link{fret_correct_signal}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name The name of the corresponding experiment.
#' @return A dataframe containing 8 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{fret_channel},
#'     \code{acceptor_channel}, \code{donor_channel} and \code{concentration}.
#' @examples
#' \dontrun{
#' format_one_dataset(my_data, "my_experiment")
#' }
#'
#' @importFrom magrittr %>%

fret_format_one_dataset <- function(raw_data, experiment_name) {
    # Generate an Experiment column, where the experiment name is its
    # corresponding file name without the .csv extension
    raw_data$Experiment <- experiment_name

    # Generate columns Type and Replicate, based on column Content
    raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = strsplit(Content,
                                              split = ".",
                                              fixed = TRUE),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate) %>%
        # Generate an Observation column, to label observations across different
        # types of experiments (blank, titration) and replicates
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation =
                          dplyr::row_number(dplyr::desc(concentration))) %>%
        dplyr::ungroup() %>%
        # Reorder columns and return dataset
        dplyr::select(Experiment,
                      Type,
                      Replicate,
                      Observation,
                      fret_channel,
                      acceptor_channel,
                      donor_channel,
                      concentration) %>%
        dplyr::mutate(Replicate = as.integer(Replicate))
}
