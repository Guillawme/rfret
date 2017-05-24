#' @title Correct FRET signal
#'
#' @description This function corrects the raw FRET signal by applying
#'     corrections for donor bleedthrough and acceptor direct excitation. See
#'     \href{https://doi.org/10.1093/nar/gkr1045}{Hieb AR \emph{et al} (2012)}
#'     for details.
#'
#' @param data A dataframe containing the FRET data after avergaing over
#'     technical replicates. This dataframe must contain the following columns:
#'     
#'     - \code{Experiment}: A unique string identifier for each experiment
#'     - \code{Type}: Either "blank" (no donor) or "titration" (donor present)
#'     - \code{Observation}: An integer that identifies each observation 
#'     within a run. For a plate-reader based assay, this is usually the row
#'     number of a well. The number of observations for a titration and a 
#'     blank must match, and a given \code{Observation} value must have the 
#'     same ligand concentration for the titration and the blank.
#'     - \code{concentration}: Ligand concentration
#'     - \code{donor_channel}: Donor fluorescence
#'     - \code{acceptor_channel}: Acceptor fluorescence
#'     - \code{fret_channel}: FRET signal
#'     
#'     The \code{\link{average_technical_replicates}}
#'     function produces a dataframe with this exact format.
#'
#' @return A dataframe containing the corrected FRET signal. It contains three
#'     columns:
#'     
#'     - \code{Experiment}: Experiment identifier (same as input)
#'     - \code{concentration}: Ligand concentration (same as input)
#'     - \code{fret}: The corrected FRET signal
#'
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
correct_fret_signal <- function(data){
  data %>% 
    dplyr::group_by(Experiment) %>%
    dplyr::do(correct_one_expt(.))
}

# This function performs the corrections for each experiment
correct_one_expt = 
  function(one_expt){
    # Colculate donor bleed through
    only_donor = dplyr::filter(one_expt, Type == "titration", concentration == 0)
    donor_bleed_through = with(only_donor, mean(fret)/mean(donor))
    
    # Calculate acceptor direct excitation
    only_acceptor = one_expt %>%
      dplyr::filter(Type == "blank", concentration != 0) %>%
      dplyr::mutate(acceptor_direct_excitation = fret/acceptor)
    
    # Apply correction factors
    titration = dplyr::filter(one_expt, Type == "titration", concentration != 0)
    titration$donor_correction = donor_bleed_through
    titration$acceptor_correction = only_acceptor$acceptor_direct_excitation
    corrected_data = titration %>%
      dplyr::transmute(
        concentration = concentration,
        fret = fret - donor*donor_correction - acceptor*acceptor_correction)
    
    # Subtract baseline
    corrected_data = corrected_data %>% dplyr::mutate(fret = fret - min(fret))
    return(corrected_data)
  }

