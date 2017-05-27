#' @title Format a single dataframe for subsequent processing
#'
#' @description This internal function processes a single dataframe to generate
#'     the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{average_technical_replicates}} and
#'     \code{\link{correct_fret_signal}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name Name of the corresponding experiment.
#' @return A dataframe containing 11 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{Well Row}, \code{Well Col},
#'     \code{Content}, \code{fret_channel}, \code{acceptor_channel},
#'     \code{donor_channel} and \code{concentration}.
#' @examples
#' \dontrun{
#' format_one_dataset(my_data, "my_experiment")
#' }

format_one_dataset <- function(raw_data, experiment_name) {
    # Generate an Experiment column, where the experiment name is its
    # corresponding file name without the .csv extension
    raw_data$Experiment <- experiment_name

    # Generate columns Type and Replicate, based on column Content
    raw_data %<>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = strsplit(Content, "_"),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate)

    # Generate an Observation column, to label observations across different
    # types of experiments (blank, titration) and replicates
    raw_data %<>%
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation =
                          dplyr::row_number(dplyr::desc(concentration)))

    # Reorder columns and return dataset
    raw_data <- raw_data[c("Experiment",
                           "Type",
                           "Replicate",
                           "Observation",
                           "Well Row",
                           "Well Col",
                           "Content",
                           "fret_channel",
                           "acceptor_channel",
                           "donor_channel",
                           "concentration")]
}

#' @title Format experimental data for subsequent processing
#'
#' @description This function formats data for subsequent processing by
#'     \code{\link{average_technical_replicates}}. It will accept all data files
#'     in a given directory, or any number of data files specified by their
#'     names, or any number of dataframes supplied as a named list.
#' @param input A directory name, a vector of file names, or a list of datasets
#'     already loaded in memory as dataframes.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @return A single dataframe with the combined input data, containing 11
#'     columns: \code{Experiment}, \code{Type}, \code{Replicate},
#'     \code{Observation}, \code{Well Row}, \code{Well Col}, \code{Content},
#'     \code{fret_channel}, \code{acceptor_channel}, \code{donor_channel} and
#'     \code{concentration}.
#' @examples
#' \dontrun{
#' format_data("my_data_directory", 4)
#' }
#' @export

format_data <- function(input = NULL, skip_lines = 0) {
    # Check the input type to determine whether we need to load datafiles from
    # files.
    # If no input is provided, ask for one:
    if (is.null(input)) {
        stop("You must specify an input: a directory name, a vector of file names or a list of dataframes.")
    }
    # If a list if provided, we will assume it's a list of already loaded
    # datasets:
    if (is.list(input)) {
        raw_data <- input
    }
    # If a character is provided, try to find out whether it's a directory name
    # or vector of file names, and load files accordingly:
    if (is.character(input) && length(input) == 1) {
        if (dir.exists(input)) {
            files <- list.files(pattern = ".csv")
            raw_data <- lapply(files, readr::read_csv, skip = skip_lines)
            names(raw_data) <- sub(files,
                                   pattern = ".csv",
                                   replacement = "")
        } else {
            stop(paste("Directory not found:", input))
        }
    }
    if (is.character(input) && length(input) > 1) {
        if (FALSE %in% file.exists(input)) {
            stop(paste("File not found:", input[file.exists(input) == FALSE]))
        } else {
            raw_data <- lapply(input, readr::read_csv, skip = skip_lines)
            names(raw_data) <- sub(input,
                                   pattern = ".csv",
                                   replacement = "")
        }
    }

    # Format data and return a single large dataframe
    raw_data %>%
        mapply(format_one_dataset, ., names(raw_data), SIMPLIFY = FALSE) %>%
        dplyr::bind_rows()
}
