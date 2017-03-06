#' @title Correct FRET signal
#'
#' @description This function subtracts the FRET background of the blank
#'   titration (without donor) from the raw FRET signal of the actual titration.
#'
#' @param fret_data A dataframe containing the fluorescence data (possibly an
#'   average of technical replicates). It must contain at least four columns
#'   named \code{fret_acceptor_only} (FRET channel of the blank titration with
#'   only acceptor-labeled molecule), \code{acceptor_acceptor_only} (acceptor
#'   channel of the blank titration with only acceptor-labeled molecule),
#'   \code{fret_titration} (FRET channel of the actual titration) and
#'   \code{concentration}.
#' @param F_donor_only A FRET channel fluorescence count measured from a sample
#'   with donor only (typically, an average from 2 to 4 technical replicates).
#' @param D_donor_only A donor channel fluorescence count measured from a sample
#'   with donor only (typically, an average from 2 to 4 technical replicates).
#' @return A dataframe containing the corrected FRET signal. It contains two
#'   columns named \code{fret_corrected} and \code{concentration}.
#' @export

correct_fret_signal <- function(fret_data, F_donor_only, D_donor_only){
    donor_bleed_through <- F_donor_only / D_donor_only
    acceptor_direct_exc <- fret_data$fret_acceptor_only / fret_data$acceptor_acceptor_only
    f_corr <- data.frame(fret_data$concentration)
    f_corr$fret_corrected <- fret_data$fret_titration - donor_bleed_through * D_donor_only - acceptor_direct_exc * fret_data$acceptor_acceptor_only
    colnames(f_corr) <- c("concentration", "fret_corrected")
    f_corr
}
