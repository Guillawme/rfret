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
#' @param highest_signal An optional number corresponding to the maximal signal
#'     measurable by the plate reader instrument used. Defaults to \code{NULL},
#'     which won't check for the presence of saturated reads. The input number
#'     is not checked in any way: make sure it really corresponds to a saturated
#'     read with your instrument.
#' @return A list containing a logical value \code{saturated_reads} indicating
#'     the presence of saturated reads in the dataset (equals to \code{TRUE} if
#'     saturated reads are present, \code{FALSE} if there is no saturated read,
#'     or \code{NULL} if this was not tested), and three \code{ggplot2} graph
#'     objects (named \code{donor}, \code{acceptor} and \code{fret}). Warning
#'     messages appear when missing values are encountered, and can be safely
#'     ignored.
#' @examples
#' \dontrun{
#' # Send all plots to the output device.
#' inspect_raw_data(fret_good)
#'
#' # Donor plot will display pipetting precision.
#' inspect_raw_data(fret_good, c("titration_1", "titration_2"))
#'
#' # Store plots in a variable, print all plots, print only one of them.
#' my_plots <- inspect_raw_data(fret_good)
#' my_plots
#' my_plots$acceptor
#' }
#' @export

inspect_raw_data <- function(raw_data,
                             titrations = NULL,
                             highest_signal = NULL) {
    # Sanity checks
    if (!is.null(titrations) && !is.character(titrations)) {
        stop("Invalid parameter: 'titrations' must be a vector of words.")
    }
    if (!is.null(highest_signal) && !is.numeric(highest_signal)) {
        stop("Invalid parameter: 'highest_signal' must be a number.")
    }

    # Check whether the data contains saturated reads
    if (is.null(highest_signal)) {
        sat_reads <- NULL
    } else {
        sat_donor <- highest_signal %in% raw_data$donor_channel
        sat_acceptor <- highest_signal %in% raw_data$acceptor_channel
        sat_fret <- highest_signal %in% raw_data$fret_channel
        sat_reads <- sat_donor | sat_acceptor | sat_fret
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
    if (!is.null(titrations)) {
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

    # Return saturated read check and all three plots
    list(saturated_reads = sat_reads,
         donor           = donor_plot,
         acceptor        = acceptor_plot,
         fret            = fret_plot)
}
