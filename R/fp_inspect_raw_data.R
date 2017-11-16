#' @title Inspect raw fluorescence data from FP binding assays
#'
#' @description This function plots the total fluorescence intensity from FP
#'     datasets as a function of the titration series.
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{fp_format_data}} or
#'     \code{\link{fp_calculate_pola_aniso_int}}.
#' @param highest_signal An optional number corresponding to the maximal signal
#'     measurable by the plate reader instrument used. Defaults to \code{NULL},
#'     which won't check for the presence of saturated reads. The input number
#'     is not checked in any way: make sure it really corresponds to a saturated
#'     read with your instrument.
#' @param output_directory An optional directory name where to write plots. This
#'     directory will be created if it does not already exist.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return A list in which each item is named after the corresponding experiment,
#'     and holds a \code{ggplot2} graph objects named \code{fluo_intensity}.
#'
#' @importFrom magrittr %>%
#'
#' @export

fp_inspect_raw_data <- function(raw_data,
                                highest_signal = NULL,
                                output_directory = NULL,
                                plot_format = "png") {
    # Split input data by Experiment and make plots for each dataset
    results <- raw_data %>%
        split(raw_data$Experiment) %>%
        mapply(fp_inspect_one_dataset,
               .,
               MoreArgs = list(highest_signal = highest_signal),
               SIMPLIFY = FALSE)

    # Optionally, write plots to files in the specified directory
    if (!is.null(output_directory)) {
        if (!(plot_format %in% c("png", "pdf", "svg"))) {
            stop("Please use one of the following output formats: 'png', 'pdf' or 'svg'.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }
        mapply(fp_save_inspection_plot,
               results,
               names(results),
               MoreArgs = list(output_directory = output_directory,
                               plot_format = plot_format))
    }

    # Always return results
    results
}

#' @title Inspect raw fluorescence data from a single FP binding assay
#'
#' @description This internal function plots the total fluorescence intensity
#'     from an FP dataset as a function of the titration series.
#'
#' @param dataset A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{fp_format_data}} or
#'     \code{\link{fp_calculate_pola_aniso_int}}.
#' @param highest_signal An optional number corresponding to the maximal signal
#'     measurable by the plate reader instrument used. Defaults to \code{NULL},
#'     which won't check for the presence of saturated reads. The input number
#'     is not checked in any way: make sure it really corresponds to a saturated
#'     read with your instrument.
#' @return A named list containing three \code{ggplot2} graph objects named
#'     \code{donor}, \code{acceptor} and \code{fret}.
#'
#' @importFrom magrittr %>%

fp_inspect_one_dataset <- function(dataset,
                                   highest_signal = NULL) {
    # Check whether the data contains saturated reads
    if (!is.null(highest_signal)) {
        sat_reads <- highest_signal %in% dataset$intensity
        if (!sat_reads) {
            message("No saturated fluorescence counts detected.")
        } else {
            warning("Dataset contains saturated reads. Measure again with a lower gain.",
                    call. = FALSE)
        }
    }

    # Get dataset name to annotate plot
    dataset_name <- levels(as.factor(dataset$Experiment))

    # Calculate average fluorescence intensity
    intensity_average <- mean(dataset$intensity, na.rm = TRUE)

    # Build and return the fluorescence intensity plot
    dataset %>%
        dplyr::mutate(Replicate = as.factor(Replicate)) %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration,
                                     y = intensity)) +
        ggplot2::geom_point(ggplot2::aes(color = Type,
                                         shape = Replicate)) +
        ggplot2::geom_hline(yintercept = intensity_average) +
        ggplot2::geom_ribbon(ggplot2::aes(ymin = intensity_average - intensity_average * 10 / 100,
                                          ymax = intensity_average + intensity_average * 10 / 100),
                             alpha = "0.2") +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle(paste("Total fluorescence intensities of", dataset_name))
}

#' @title Save raw data plots from a single FP binding dataset
#'
#' @description This internal function saves the total fluorescence intensity
#'     plots from an FP dataset to PNG, PDF or SVG files.
#'
#' @param input_plot The output of  \code{\link{fp_inspect_one_dataset}}.
#' @param dataset_name The name of the corresponding dataset.
#' @param output_directory The name of the output directory where plots will be
#'     saved.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return Writes plots in files on disk.

fp_save_inspection_plot <- function(input_plot,
                                    dataset_name,
                                    output_directory,
                                    plot_format) {
    # Save plot
    ggplot2::ggsave(plot = input_plot,
                    filename = paste(dataset_name,
                                     "_fluo-intensities.",
                                     plot_format,
                                     sep = ""),
                    path = output_directory,
                    device = plot_format,
                    width = 8.21,
                    height = 4.61,
                    units = "in",
                    dpi = 300)
}
