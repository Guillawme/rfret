#' @title Make a figure with FRET stoichiometry data (Job plot)
#'
#' @description This function takes raw or corrected FRET data from a
#'     FRET stoichiometry experiment (continuous variation method) and outputs a
#'     Job plot.
#'
#' @param dataset A dataframe containing at least three columns called
#'     \code{Experiment}, \code{fret_channel} (or \code{signal}), and
#'     \code{concentration}. This can be the output of
#'     \code{\link{fret_format_data}} or \code{\link{fret_correct_signal}}.
#' @param output_directory An optional directory name where to write the plot.
#'     This directory will be created if it does not already exist.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return A \code{\link[ggplot2]{ggplot}} graph object of a Job plot figure.
#'
#' @importFrom magrittr %>%
#'
#' @export

fret_job_plot <- function(dataset,
                          output_directory = NULL,
                          plot_format = "png") {
    # Sanity checks
    column_fret_channel_present <- "fret_channel" %in% colnames(dataset)
    column_signal_present <- "signal" %in% colnames(dataset)
    columns_ok <- xor(column_fret_channel_present, column_signal_present)
    if (!columns_ok) {
        stop("Could not find FRET signal in this dataset. You need a column named 'fret_channel' or 'signal' (not both).")
    }

    # Get correct column name for the Y axis
    y_axis <- NULL
    y_axis_title <- NULL
    if (column_fret_channel_present) {
        y_axis <- dataset$fret_channel
        y_axis_title <- "Raw FRET"}
    if (column_signal_present) {
        y_axis <- dataset$signal
        y_axis_title <- "FRET corrected"
        }

    job_plot <- dataset %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration,
                                     y = y_axis)) +
        ggplot2::geom_point(ggplot2::aes(shape = Experiment)) +
        ggplot2::theme_bw() +
        ggplot2::xlab("Molar fraction of receptor") +
        ggplot2::ylab(y_axis_title) +
        ggplot2::ggtitle("Job plot")

    # Optionally, write plots to files in the specified directory
    if (!is.null(output_directory)) {
        if (!(plot_format %in% c("png", "pdf", "svg"))) {
            stop("Please use one of the following output formats: 'png', 'pdf' or 'svg'.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }
        ggplot2::ggsave(plot = job_plot,
                        filename = paste("Job-plot.", plot_format, sep = ""),
                        path = output_directory,
                        device = plot_format,
                        width = 8.21,
                        height = 4.61,
                        units = "in",
                        dpi = 300)
    }

    # Return plot
    job_plot
}
