#' @title Batch process FRET data
#'
#' @description This function applies \code{\link{average_technical_replicates}}
#'     and \code{\link{correct_fret_signal}} to all raw data files present in a
#'     given directory, and either writes output files in another directory or
#'     returns a list containing corrected datasets for programmatic
#'     manipulation.
#'
#' @param directory_in A directory containing raw data files.
#' @param titrations A character vector containing words that identify the
#'     titration series (e.g. \code{titration}). These words must match the
#'     ones in \code{raw_data$Content} (case sensitive).
#' @param blanks A character vector containing words that identify the blank
#'     experiment series (e.g. \code{blank}). These words must match the ones
#'     in \code{raw_data$Content} (case sensitive).
#' @param skip_lines Number of lines to skip at the beginning of CSV files (see
#'     \code{\link[utils]{read.csv}}). Defaults to 4.
#' @param directory_out An optional directory where to write output files.
#' @return Write to \code{directory_out} data files containing the corrected
#'     FRET signal from each input raw data file. Each output file contains two
#'     columns named \code{concentration} and \code{fret_corrected}, and has the
#'     same file name as the original raw data file appended with
#'     \code{_corrected}. If no output directory is specified, the function
#'     instead returns a list containing the corrected datasets, which can then
#'     be accessed programmatically.
#' @seealso \code{\link{average_technical_replicates}} and
#'     \code{\link{correct_fret_signal}}, which can be used manually on an
#'     individual dataset.
#' @export

batch_process <- function(directory_in = NULL,
                          titrations = NULL,
                          blanks = NULL,
                          skip_lines = 4,
                          directory_out = NULL){
    # Sanity checks
    if(is.null(blanks) || is.null(titrations)){
        stop("You must specify the names of your blanks and titrations data series.")
    }
    if(!is.character(blanks) || !is.character(titrations)){
        stop("Invalid blanks and titrations names. These arguments must be character vectors.")
    }
    if(is.null(directory_in)){
        stop("You must specify an input directory.")
    }
    if(!dir.exists(directory_in)){
        stop("Input directory not found.")
    }

    # Read input files, load associated datasets into a list
    data_files <- list.files(directory_in, pattern = ".csv", full.names = TRUE)
    filenames <- list.files(directory_in, pattern = ".csv", full.names = FALSE)
    filenames <- sub(filenames,
                     pattern = ".csv",
                     replacement = "")
    data_tables <- lapply(data_files, read.csv, skip = skip_lines)
    names(data_tables) <- filenames

    # Apply replicate averaging
    datasets_reduced <- lapply(data_tables,
                               average_technical_replicates,
                               titrations = titrations,
                               blanks = blanks)

    # Apply signal correction
    datasets_corrected <- lapply(datasets_reduced, correct_fret_signal)

    # Depending on input, write output files or simply return the list
    if(is.null(directory_out)){
        return(datasets_corrected)
    } else {
        if(!dir.exists(directory_out)){
            message(paste("Creating directory:", directory_out))
            dir.create(directory_out)
        }
        mapply(write.csv,
               datasets_corrected,
               file = paste(directory_out,
                            names(datasets_corrected),
                            "_corrected.csv",
                            sep = ""),
               MoreArgs = list(row.names = FALSE))
    }
}
