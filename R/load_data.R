#' @title Check and load input data
#'
#' @description This internal function checks input data before subsequent
#'     processing by \code{*_format_data} functions. It will accept all data
#'     files in a given directory, or any number of data files specified by
#'     their names, or any number of dataframes supplied as a named list, or
#'     even a single dataframe supplied as is.
#' @param input A directory name, a vector of file names, a single dataset
#'     already loaded in memory as a dataframe, or a list of datasets already
#'     loaded in memory as dataframes.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @return A named list of dataframes to be further processed by
#'     \code{*_format_data} functions.
#' @examples
#' \dontrun{
#' load_data("my_data_directory", 4)
#' }

load_data <- function(input = NULL, skip_lines = NULL) {
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
        # If we receive a list, we will assume it is a named list of dataframes
        # corresponding to already loaded datasets
        raw_data <- input
        names(raw_data) <- names(input)
    } else if (is.character(input) && length(input) == 1) {
        # If we receive a character, try to figure out whether it is the name
        # of a directory or a vector of file names
        if (dir.exists(input)) {
            files <- list.files(path = input, pattern = ".csv")
            raw_data <- read_files(files, skip_lines = skip_lines)
        } else if (file.exists(input)) {
            raw_data <- read_files(input, skip_lines = skip_lines)
        } else {
            stop("File or directory not found: ", input)
        }
    } else if (is.character(input) && length(input) > 1) {
        # If we receive a character vector longer than 1, it has to be a set of
        # fine names
        if (FALSE %in% file.exists(input)) {
            stop("File not found: ", input[file.exists(input) == FALSE])
        } else {
            raw_data <- read_files(input, skip_lines = skip_lines)
        }
    } else {
        # If the input doesn't match any of the above, complain!
        stop("Please provide a valid directory name, or valid file names.")
    }

    # Return raw data
    raw_data
}

#' @title Read files
#'
#' @description This internal function simply reads in data files.
#' @param input A vector of file names, optionally with named items.
#' @param skip_lines The number of lines to skip at the beginning of CSV files
#'     (usually containing header information before the actual data starts).
#' @return A named list of dataframes to be further processed by
#'     \code{*_format_data} functions. If the input vector is named, these item
#'     names will be retained in the output list. Otherwise, the list items will
#'     be named after the files read fron disk.
#' @examples
#' \dontrun{
#' read_files(my_file_list, 4)
#' }

read_files <- function(input = NULL, skip_lines = NULL) {
    loaded_files <- lapply(input, readr::read_csv, skip = skip_lines)
    if(is.null(names(input))) {
        # If the vector of files has no names attribute, use each corresponding
        # file name without the .csv extension to name each element of the vector
        names(loaded_files) <- sub(input, pattern = ".csv", replacement = "")
    } else {
        # Or simply keep the names attribute from the original vector, if
        # it already exists
        names(loaded_files) <- names(input)
    }
    loaded_files
}
