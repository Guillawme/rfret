#' @title Inspect raw data
#'
#' @description This function plots the raw data of experiments for visual
#'     inspection.
#'
#' @param raw_data A dataframe containing the raw data. It must be the output of
#'     \code{\link{format_data}} or \code{\link{fp_calculate_pola_aniso_int}}.
#' @param data_type A character string describing which type of data is supplied
#'     for pre-processing. Currently supported options are \code{"fret"} and
#'     \code{"fp"}.
#' @param highest_signal An optional number corresponding to the maximal signal
#'     measurable by the instrument used. Defaults to \code{NULL}, which won't
#'     check for the presence of saturated reads. The input number is not
#'     checked in any way: make sure it really corresponds to a saturated
#'     read with your instrument.
#' @param output_directory An optional directory name where to write plots. This
#'     directory will be created if it does not already exist.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return A list in which each item is named after the corresponding experiment,
#'     and holds a single \code{ggplot2} graph objects or a list of several such
#'     objects.
#'
#' @importFrom magrittr %>%
#' @importFrom zeallot %<-%
#'
#' @export

inspect_raw_data <- function(raw_data,
                             data_type,
                             highest_signal = NULL,
                             output_directory = NULL,
                             plot_format = "png") {
    # Stop if the type of data is not supported, and choose the appropriate
    # inspection and plot saving functions according to the type of data.
    inspect_dataset <- NULL
    save_inspection_plots <- NULL
    c(inspect_dataset, save_inspection_plots) %<-%
        switch(data_type,
               "fret" = c(fret_inspect_one_dataset, fret_save_inspection_plots),
               "fp"   = c(fp_inspect_one_dataset, fp_save_inspection_plot),
               stop("Unsupported data type: ", data_type, "\nCurrently supported data types: fret, fp."))

    # Split input data by Experiment and make plots for each dataset
    results <- raw_data %>%
        split(raw_data$Experiment) %>%
        purrr::map(inspect_dataset,
                   highest_signal = highest_signal)

    # Optionally, write plots to files in the specified directory
    if (!is.null(output_directory)) {
        if (!(plot_format %in% c("png", "pdf", "svg"))) {
            stop("Please use one of the following output formats: 'png', 'pdf' or 'svg'.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }
        purrr::walk2(.x = results,
                     .y = names(results),
                     .f = save_inspection_plots,
                     output_directory = output_directory,
                     plot_format = plot_format)
    }

    # Always return results
    results
}
