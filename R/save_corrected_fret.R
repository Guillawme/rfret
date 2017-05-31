#' @title Save corrected FRET data
#'
#' @description This function saves the corrected FRET data produced by
#'     \code{\link{correct_fret_signal}} to CSV files in a specified output
#'     directory. It will keep original file names, and append \code{_corrected}
#'     after the file name and before the \code{.csv} extension.
#'
#' @param input A single dataframe containing corrected FRET data, like the
#'     output of \code{\link{correct_fret_signal}}.
#' @param output_directory The name of an output directory. This directory will
#'     be created if it does not already exist.
#' @return Write to \code{output_directory} CSV data files containing the
#'     corrected FRET signal from each input experiment. Each output file
#'     contains three columns named \code{Experiment}, \code{concentration} and
#'     \code{fret_corrected}, and has the same file name as the original raw
#'     data file appended with \code{_corrected}.
#' @seealso \code{\link{format_data}}, \code{\link{average_technical_replicates}}
#'     and \code{\link{correct_fret_signal}}, which are the other steps of the
#'     FRET data processing pipeline.
#' @export

save_corrected_fret <- function (input = NULL,
                                 output_directory = NULL) {
    # Sanity checks
    if (is.null(input)) {
        stop("Please provide input data. For example, the output of correct_fret_signal().")
    }
    if (is.null(output_directory)) {
        stop("Please provide an output directory name.")
    }
    if (length(output_directory) > 1) {
        stop("Please provide a single directory name.")
    }
    # Create output directory, if it does not already exist
    if (!dir.exists(output_directory)) {
        message("Creating directory: ", output_directory)
        dir.create(output_directory)
    }

    # Split single dataframe by experiment name, then write each result to a CSV
    # file in the output directory
    input %>%
        split(input$Experiment) %>%
        mapply(readr::write_csv,
               .,
               path = paste(output_directory,
                            "/",
                            names(.),
                            "_corrected.csv",
                            sep = ""))
}
