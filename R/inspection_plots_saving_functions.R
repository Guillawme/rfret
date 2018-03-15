#' @title Save raw data plots from a single FRET binding dataset
#'
#' @description This internal function saves the raw fluorescence plots from the
#'     donor, acceptor and FRET channels to PNG, PDF or SVG files.
#'
#' @param input_plots The output of  \code{\link{fret_inspect_one_dataset}}.
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
