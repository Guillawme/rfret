#' @title Correct FRET signal
#'
#' @description This function subtracts the FRET background measured by the
#'     blank experiment (without donor) from the raw FRET signal of the actual
#'     titration, applying corrections given by
#'     \href{https://doi.org/10.1093/nar/gkr1045}{Hieb AR \emph{et al} (2012)}.
#'
#' @param reduced_dataset A dataframe containing the fluorescence data. It must
#'     contain at least five columns named \code{Content}, \code{donor_channel},
#'     \code{acceptor_channel}, \code{fret_channel} and \code{concentration}.
#'     The \code{Content} column must contain, for each row, a word describing
#'     which data series this row belongs to (like \code{"blank"} or
#'     \code{"titration"}). The \code{\link{average_technical_replicates}}
#'     function produces a dataframe with this exact format.
#' @param blank A character vector containing a word that identifies the blank
#'     experiment series (e.g. \code{"blank"}). This word must match the one
#'     in \code{reduced_dataset$Content} (case sensitive). Defaults to
#'     \code{"blank"}, as set by the \code{\link{average_technical_replicates}}
#'     function.
#' @param titration A character vector containing a word that identifies the
#'     titration series (e.g. \code{titration}). This word must match the one in
#'     \code{reduced_dataset$Content} (case sensitive). Defaults to
#'     \code{"titration"}, as set by the
#'     \code{\link{average_technical_replicates}} function.
#' @return A dataframe containing the corrected FRET signal. It contains two
#'     columns named \code{concentration} and \code{fret_corrected}.
#' @seealso \code{\link{average_technical_replicates}} to prepare a dataset for
#'     use with \code{correct_fret_signal}.
#'
#'     Hieb AR \emph{et al} (2012) Fluorescence Strategies for High-Throughput
#'     Quantification of Protein Interactions. \emph{Nucleic Acids Research}
#'     40 (5): e33
#'     (\href{https://doi.org/10.1093/nar/gkr1045}{doi:10.1093/nar/gkr1045}) for
#'     the details on the signal correction applied by
#'     \code{correct_fret_signal}.
#' @export

correct_fret_signal <- function(reduced_dataset,
                                blank = "blank",
                                titration = "titration"){
    # Calculate acceptor direct excitation (equation 5 in Hieb et al 2012)
    acceptor_direct_exc <-
        reduced_dataset$fret_channel[(reduced_dataset$Content == blank) &
                                         !is.na(reduced_dataset$concentration)] /
        reduced_dataset$acceptor_channel[(reduced_dataset$Content == blank) &
                                             !is.na(reduced_dataset$concentration)]

    # Calculate donor bleed through (equation 4 in Hieb et al 2012)
    donor_bleed_through <-
        mean(
            reduced_dataset$fret_channel[(reduced_dataset$Content == titration) &
                                             is.na(reduced_dataset$concentration)],
            na.rm = TRUE
        ) /
        mean(
            reduced_dataset$donor_channel[(reduced_dataset$Content == titration) &
                                              is.na(reduced_dataset$concentration)],
            na.rm = TRUE
        )

    # Calculate corrected FRET (equation 6 in Hieb et al 2012)
    fret_corr <-
        reduced_dataset$fret_channel[
            (reduced_dataset$Content == titration) &
                !is.na(reduced_dataset$concentration)] -
        donor_bleed_through * reduced_dataset$donor_channel[
            (reduced_dataset$Content == titration) &
                !is.na(reduced_dataset$concentration)] -
        acceptor_direct_exc * reduced_dataset$acceptor_channel[
            (reduced_dataset$Content == titration) &
                !is.na(reduced_dataset$concentration)]
    # Add a constant to shift all points such that the lowest is 0
    fret_corr <- fret_corr + abs(min(fret_corr))

    # Build final dataframe
    fret_corrected <- data.frame(
        concentration = reduced_dataset$concentration[
            (reduced_dataset$Content == titration) &
                !is.na(reduced_dataset$concentration)],
        fret_corrected = fret_corr
        )
}
