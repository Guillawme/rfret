#' @title Inspect raw fluorescence data from FRET binding assays
#'
#' @description This function plots the raw fluorescence data from the donor,
#'     acceptor and FRET channels, as a function of the titration series.
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{fret_format_data}}.
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
#'     and holds a named list containing three \code{ggplot2} graph objects
#'     named \code{donor}, \code{acceptor} and \code{fret}.
#'
#' @importFrom magrittr %>%
#'
#' @export

fret_inspect_raw_data <- function(raw_data,
                                  highest_signal = NULL,
                                  output_directory = NULL,
                                  plot_format = "png") {
    # Split input data by Experiment and make plots for each dataset
    results <- raw_data %>%
        split(raw_data$Experiment) %>%
        purrr::map(fret_inspect_one_dataset,
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
                     .f = fret_save_inspection_plots,
                     output_directory = output_directory,
                     plot_format = plot_format)
    }

    # Always return results
    results
}

#' @title Inspect raw fluorescence data from a single FRET binding assay
#'
#' @description This internal function plots the raw fluorescence data from the
#'     donor, acceptor and FRET channels, as a function of the titration series.
#'
#' @param dataset A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{fret_format_data}}.
#' @param highest_signal An optional number corresponding to the maximal signal
#'     measurable by the plate reader instrument used. Defaults to \code{NULL},
#'     which won't check for the presence of saturated reads. The input number
#'     is not checked in any way: make sure it really corresponds to a saturated
#'     read with your instrument.
#' @return A named list containing three \code{ggplot2} graph objects named
#'     \code{donor}, \code{acceptor} and \code{fret}.
#'
#' @importFrom magrittr %>%

fret_inspect_one_dataset <- function(dataset,
                                     highest_signal = NULL) {
    # Check whether the data contains saturated reads
    if (!is.null(highest_signal)) {
        sat_donor <- highest_signal %in% dataset$donor_channel
        sat_acceptor <- highest_signal %in% dataset$acceptor_channel
        sat_fret <- highest_signal %in% dataset$fret_channel
        sat_reads <- sat_donor | sat_acceptor | sat_fret
        if (!sat_reads) {
            message("No saturated fluorescence counts detected.")
        }
        if (sat_donor) {
            warning("Donor channel contains saturated reads. Measure again with a lower gain for this channel.",
                    call. = FALSE)
        }
        if (sat_acceptor) {
            warning("Acceptor channel contains saturated reads. Measure again with a lower gain for this channel.",
                    call. = FALSE)
        }
        if (sat_fret) {
            warning("FRET channel contains saturated reads. Measure again with a lower gain for this channel.",
                    call. = FALSE)
        }
    }

    # Get dataset name to annotate plots
    dataset_name <- levels(as.factor(dataset$Experiment))

    # Build the donor channel plot
    donor_plot <- dataset %>%
        dplyr::mutate(Replicate = as.factor(Replicate)) %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration,
                                     y = donor_channel)) +
        ggplot2::geom_point(ggplot2::aes(color = Type,
                                         shape = Replicate)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle(paste("Donor channel of", dataset_name))

    # Build the acceptor channel plot
    acceptor_plot <- dataset %>%
        dplyr::mutate(Replicate = as.factor(Replicate)) %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration,
                                     y = acceptor_channel)) +
        ggplot2::geom_point(ggplot2::aes(color = Type,
                                         shape = Replicate)) +
        ggplot2::theme_bw() +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle(paste("Acceptor channel of", dataset_name))

    # Build the fret channel plot
    fret_plot <- dataset %>%
        dplyr::mutate(Replicate = as.factor(Replicate)) %>%
        ggplot2::ggplot(ggplot2::aes(x = concentration,
                                     y = fret_channel)) +
        ggplot2::geom_point(ggplot2::aes(color = Type,
                                         shape = Replicate)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle(paste("FRET channel of", dataset_name))

    # Return all three plots
    list(donor    = donor_plot,
         acceptor = acceptor_plot,
         fret     = fret_plot)
}

#' @title Save raw data plots from a single FRET binding dataset
#'
#' @description This internal function saves the raw fluorescence plots from the
#'     donor, acceptor and FRET channels to PNG, PDF or SVG files.
#'
#' @param input_plots The output of  \code{\link{fret_inspect_raw_data}}.
#' @param dataset_name The name of the corresponding dataset.
#' @param output_directory The name of the output directory where plots will be
#'     saved.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return Writes plots in files on disk.

fret_save_inspection_plots <- function(input_plots,
                                       dataset_name,
                                       output_directory,
                                       plot_format) {
    # Save all plots by applying over plot types
    mapply(ggplot2::ggsave,
           plot = input_plots,
           filename = paste(dataset_name,
                            "_raw-",
                            names(input_plots),
                            ".",
                            plot_format,
                            sep = ""),
           MoreArgs = list(path = output_directory,
                           device = plot_format,
                           width = 8.21,
                           height = 4.61,
                           units = "in",
                           dpi = 300))
}
