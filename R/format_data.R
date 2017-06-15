#' @title Format raw experimental FRET binding data for subsequent processing
#'
#' @description This function formats data for subsequent processing by
#'     \code{\link{average_technical_replicates}}. It will accept all data files
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
#'     \code{donor_channel} and \code{concentration}.
#' @examples
#' \dontrun{
#' format_data("my_data_directory", 4)
#' }
#' @export

format_data <- function(input = NULL, skip_lines = 0) {
    # Check the input type to determine how to access the data
    if (is.null(input)) {
        # If no input is provided, ask for one
        stop("You must specify an input: a directory name, a vector of file names, a dataframe, or a list of dataframes.")
    } else if (is.list(input) && is.data.frame(input)) {
        # If a single dataframe is provided, we put it in a named list so mapply
        # can process it
        raw_data <- list(input)
        names(raw_data) <- deparse(substitute(input))
    } else if (is.list(input) && !is.data.frame(input)) {
        # If we receive a list, we will assume it is a list of dataframes
        # corresponding to already loaded datasets
        raw_data <- input
    } else if (is.character(input) && length(input) == 1) {
        # If we receive a character, try to figure out whether it is the name
        # of a directory or a vector of file names
        if (dir.exists(input)) {
            files <- list.files(pattern = ".csv")
            raw_data <- lapply(files, readr::read_csv, skip = skip_lines)
            names(raw_data) <- sub(files,
                                   pattern = ".csv",
                                   replacement = "")
        } else if (file.exists(input)) {
            raw_data <- lapply(input, readr::read_csv, skip = skip_lines)
            names(raw_data) <- sub(input,
                                   pattern = ".csv",
                                   replacement = "")
        } else {
            stop("File or directory not found: ", input)
        }
    } else if (is.character(input) && length(input) > 1) {
        # If we receive a character vector longer than 1, it has to be a set of
        # fine names
        if (FALSE %in% file.exists(input)) {
            stop("File not found: ", input[file.exists(input) == FALSE])
        } else {
            raw_data <- lapply(input, readr::read_csv, skip = skip_lines)
            names(raw_data) <- sub(input,
                                   pattern = ".csv",
                                   replacement = "")
        }
    } else {
        # If the input doesn't match any of the above, complain!
        stop("Please provide a valid directory name, or valid file names.")
    }

    # Format data and return a single large dataframe
    raw_data %>%
        mapply(format_one_dataset, ., names(raw_data), SIMPLIFY = FALSE) %>%
        dplyr::bind_rows()
}

#' @title Format a single dataframe for subsequent processing
#'
#' @description This internal function processes a single dataframe to generate
#'     the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{average_technical_replicates}} and
#'     \code{\link{correct_fret_signal}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name The name of the corresponding experiment.
#' @return A dataframe containing 8 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{fret_channel},
#'     \code{acceptor_channel}, \code{donor_channel} and \code{concentration}.
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
        dplyr::mutate(intermediate = strsplit(Content,
                                              split = ".",
                                              fixed = TRUE),
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
    raw_data[c("Experiment",
               "Type",
               "Replicate",
               "Observation",
               "fret_channel",
               "acceptor_channel",
               "donor_channel",
               "concentration")]
}
