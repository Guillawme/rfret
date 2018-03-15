#' @title Inspect raw fluorescence data from a single FRET binding dataset
#'
#' @description This internal function plots the raw fluorescence data from the
#'     donor, acceptor and FRET channels, as a function of the titration series.
#'
#' @param dataset A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{format_data}}.
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

#' @title Inspect raw fluorescence data from a single FP binding assay
#'
#' @description This internal function plots the total fluorescence intensity
#'     from an FP dataset as a function of the titration series.
#'
#' @param dataset A dataframe containing the raw fluorescence data. It must
#'     be the output of \code{\link{format_data}} or
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
