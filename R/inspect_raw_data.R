#' @title Inspect raw fluorescence data
#'
#' @description This function plots the raw fluorescence data from the donor,
#'   acceptor and FRET channels, as a function of the acceptor concentration
#'   (titration series).
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     contain at least five columns named \code{Content}, \code{concentration},
#'     \code{donor_channel}, \code{acceptor_channel} and \code{fret_channel}.
#'     The \code{Content} column is used to plot distinct data series with
#'     different symbols: simply add in the \code{Content} column, for each row,
#'     a word describing which data series or replicate this row belongs to
#'     (like \code{control_1}, \code{titration_1}, etc.).
#' @return A list containing three \code{ggplot2} graph objects (named
#'     \code{donor}, \code{acceptor} and \code{fret}). Warning messages appear
#'     when missing values are encountered, and can be safely ignored.
#' @examples
#' inspect_raw_data(my_data) # Send all plots to the output device.
#' my_plots <- inspect_raw_data(my_data) # Store plots in a variable.
#' my_plots # Send all plots to the output device.
#' my_plots$donor # Send only donor channel plot to the output device.
#' @export

inspect_raw_data <- function(raw_data) {
    donor_plot <- ggplot2::ggplot(data = raw_data,
                                  ggplot2::aes(x = concentration,
                                               y = donor_channel))
    donor_plot <- donor_plot + ggplot2::geom_point(ggplot2::aes(shape = Content))
    donor_plot <- donor_plot + ggplot2::theme_bw()
    donor_plot <- donor_plot + ggplot2::scale_x_log10()

    acceptor_plot <- ggplot2::ggplot(data = raw_data,
                                     ggplot2::aes(x = concentration,
                                                  y = acceptor_channel))
    acceptor_plot <- acceptor_plot + ggplot2::geom_point(ggplot2::aes(shape = Content))
    acceptor_plot <- acceptor_plot + ggplot2::theme_bw()

    fret_plot <- ggplot2::ggplot(data = raw_data,
                                 ggplot2::aes(x = concentration,
                                              y = fret_channel))
    fret_plot <- fret_plot + ggplot2::geom_point(ggplot2::aes(shape = Content))
    fret_plot <- fret_plot + ggplot2::theme_bw()
    fret_plot <- fret_plot + ggplot2::scale_x_log10()

    list(donor = donor_plot,
         acceptor = acceptor_plot,
         fret = fret_plot)
}
