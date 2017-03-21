#' @title Inspect raw fluorescence data
#'
#' @description This function plots the raw fluorescence data from the donor,
#'     acceptor and FRET channels, as a function of the acceptor concentration
#'     (titration series).
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     contain at least five columns named \code{Content}, \code{donor_channel},
#'     \code{acceptor_channel}, \code{fret_channel} and \code{concentration}.
#'     The \code{Content} column is used to plot distinct data series with
#'     different symbols; it must contain, for each row, a word describing
#'     which data series or replicate this row belongs to (like
#'     \code{"blank_1"}, \code{"titration_1"}, etc.).
#' @param titrations An optional character vector containing the names
#'     associated to the titration series. If not specified, the donor channel
#'     plot will not display the mean and +/- 10 \% shaded area.
#' @return A list containing three \code{ggplot2} graph objects (named
#'     \code{donor}, \code{acceptor} and \code{fret}). Warning messages appear
#'     when missing values are encountered, and can be safely ignored.
#' @examples
#' inspect_raw_data(my_data) # Send all plots to the output device.
#' inspect_raw_data(my_data, "titration") # Donor plot will display pipetting precision.
#' my_plots <- inspect_raw_data(my_data) # Store plots in a variable.
#' my_plots # Send all plots to the output device.
#' my_plots$donor # Send only donor channel plot to the output device.
#' @export

inspect_raw_data <- function(raw_data, titrations = NULL){
    # Sanity checks
    if(!is.null(titrations) && class(titrations) != "character"){
        stop("Invalid titration names. The 'titrations' argument should be a character vector.")
    }

    # Build the donor channel plot
    donor_plot <- ggplot2::ggplot(data = raw_data,
                                  ggplot2::aes(x = concentration,
                                               y = donor_channel,
                                               shape = Content)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle("Donor channel")
    if(!is.null(titrations)){
        donor_average <- mean(raw_data$donor_channel[raw_data$Content %in% titrations],
                              na.rm = TRUE)
        donor_plot <- donor_plot +
            ggplot2::geom_hline(yintercept = donor_average) +
            ggplot2::geom_ribbon(ggplot2::aes(ymin = donor_average - donor_average * 10 / 100,
                                              ymax = donor_average + donor_average * 10 / 100),
                                 alpha = "0.1")
    }

    # Build the acceptor channel plot
    acceptor_plot <- ggplot2::ggplot(data = raw_data,
                                     ggplot2::aes(x = concentration,
                                                  y = acceptor_channel)) +
        ggplot2::geom_point(ggplot2::aes(shape = Content)) +
        ggplot2::geom_smooth(method = "lm") +
        ggplot2::theme_bw() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle("Acceptor channel")

    # Build the fret channel plot
    fret_plot <- ggplot2::ggplot(data = raw_data,
                                 ggplot2::aes(x = concentration,
                                              y = fret_channel,
                                              shape = Content)) +
        ggplot2::geom_point() +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Fluorescence Intensity") +
        ggplot2::ggtitle("FRET channel")

    # Return all three plots
    list(donor    = donor_plot,
         acceptor = acceptor_plot,
         fret     = fret_plot)
}
