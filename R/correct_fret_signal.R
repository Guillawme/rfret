#' @title Correct FRET signal
#'
#' @description This function corrects the raw FRET signal by applying
#'     corrections for donor bleedthrough and acceptor direct excitation. See
#'     \href{https://doi.org/10.1093/nar/gkr1045}{Hieb AR \emph{et al} (2012)}
#'     and \href{https://doi.org/10.1016/B978-0-12-391940-3.00011-1}{Winkler DD
#'     \emph{et al} (2012)} for details.
#'
#' @param data A dataframe containing the FRET data after avergaing over
#'     technical replicates. This dataframe must contain the following columns:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment.}
#'     \item{Type}{Either "blank" (no donor) or "titration" (donor present).}
#'     \item{Observation}{A number identifying each observation in a titration
#'     series (corresponds to the plate column numbers, if experiments are set
#'     up as rows in a 384-well plate). The number of observations for an
#'     experiment and its blanks must match, and a given observation number must
#'     associate data points at the same concentration in the titration series.}
#'     \item{concentration}{The ligand concentration in the titration series.}
#'     \item{fret_channel}{Fluorescence intensity in the FRET channel.}
#'     \item{acceptor_channel}{Fluorescence intensity in the acceptor channel.}
#'     \item{donor_channel}{Fluorescence intensity in the donor channel.}
#'     }
#'     The output of \code{\link{average_technical_replicates}} can be used
#'     directly as input for this function.
#'
#' @return A dataframe containing the corrected FRET signal. It contains three
#'     columns:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment (same as in
#'     the input data).}
#'     \item{concentration}{The ligand concentration in the titration series
#'     (same as in the input data).}
#'     \item{fret}{The corrected FRET signal.}
#'     }
#'
#' @seealso \code{\link{average_technical_replicates}} to prepare a dataset for
#'     use with \code{correct_fret_signal}.
#'
#'     For details on the signal correction applied by \code{correct_fret_signal},
#'     see:
#'     \itemize{
#'     \item Hieb AR \emph{et al} (2012) Fluorescence Strategies for High-Throughput
#'     Quantification of Protein Interactions. \emph{Nucleic Acids Research}
#'     40 (5): e33 (\href{https://doi.org/10.1093/nar/gkr1045}{doi:10.1093/nar/gkr1045}) and
#'     \item Winkler DD \emph{et al} (2012) Quantifying Chromatin-Associated
#'     Interactions: The HI-FI System. In \emph{Methods in Enzymology} pp
#'     243–274. Elsevier
#'     (\href{https://doi.org/10.1016/B978-0-12-391940-3.00011-1}{doi:10.1016/B978-0-12-391940-3.00011-1}).
#'     }
#'
#' @export
correct_fret_signal <- function(data){
  data %>%
    dplyr::group_by(Experiment) %>%
    dplyr::do(correct_one_expt(.))
}

#' @title Correct FRET signal for one experiment
#'
#' @description This internal function corrects the raw FRET signal of a single
#'     experiment by applying corrections for donor bleedthrough and acceptor
#'     direct excitation. See
#'     \href{https://doi.org/10.1093/nar/gkr1045}{Hieb AR \emph{et al} (2012)}
#'     and \href{https://doi.org/10.1016/B978-0-12-391940-3.00011-1}{Winkler DD
#'     \emph{et al} (2012)} for details.
#'
#' @param data A dataframe containing the FRET data after avergaing over
#'     technical replicates. This dataframe must contain the following columns:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment.}
#'     \item{Type}{Either "blank" (no donor) or "titration" (donor present).}
#'     \item{Observation}{A number identifying each observation in a titration
#'     series (corresponds to the plate column numbers, if experiments are set
#'     up as rows in a 384-well plate). The number of observations for an
#'     experiment and its blanks must match, and a given observation number must
#'     associate data points at the same concentration in the titration series.}
#'     \item{concentration}{The ligand concentration in the titration series.}
#'     \item{fret_channel}{Fluorescence intensity in the FRET channel.}
#'     \item{acceptor_channel}{Fluorescence intensity in the acceptor channel.}
#'     \item{donor_channel}{Fluorescence intensity in the donor channel.}
#'     }
#'     The output of \code{\link{average_technical_replicates}} can be used
#'     directly as input for this function.
#'
#' @return A dataframe containing the corrected FRET signal. It contains three
#'     columns:
#'     \describe{
#'     \item{Experiment}{A unique name identifying each experiment (same as in
#'     the input data).}
#'     \item{concentration}{The ligand concentration in the titration series
#'     (same as in the input data).}
#'     \item{fret}{The corrected FRET signal.}
#'     }
#'
#' @seealso \code{\link{average_technical_replicates}} to prepare a dataset for
#'     use with \code{correct_fret_signal}.
#'
#'     For details on the signal correction applied by \code{correct_fret_signal},
#'     see:
#'     \itemize{
#'     \item Hieb AR \emph{et al} (2012) Fluorescence Strategies for High-Throughput
#'     Quantification of Protein Interactions. \emph{Nucleic Acids Research}
#'     40 (5): e33 (\href{https://doi.org/10.1093/nar/gkr1045}{doi:10.1093/nar/gkr1045}) and
#'     \item Winkler DD \emph{et al} (2012) Quantifying Chromatin-Associated
#'     Interactions: The HI-FI System. In \emph{Methods in Enzymology} pp
#'     243–274. Elsevier
#'     (\href{https://doi.org/10.1016/B978-0-12-391940-3.00011-1}{doi:10.1016/B978-0-12-391940-3.00011-1}).
#'     }

correct_one_expt <- function(one_expt){
    # Calculate donor bleed through
    only_donor <- dplyr::filter(one_expt, Type == "titration", concentration == 0)
    donor_bleed_through <- with(only_donor, mean(fret)/mean(donor))

    # Calculate acceptor direct excitation
    only_acceptor <- one_expt %>%
      dplyr::filter(Type == "blank", concentration != 0) %>%
      dplyr::mutate(acceptor_direct_excitation = fret/acceptor)

    # Apply correction factors
    titration <- dplyr::filter(one_expt, Type == "titration", concentration != 0)
    titration$donor_correction <- donor_bleed_through
    titration$acceptor_correction <- only_acceptor$acceptor_direct_excitation
    corrected_data <- titration %>%
      dplyr::transmute(
        concentration = concentration,
        fret = fret - donor*donor_correction - acceptor*acceptor_correction)

    # Subtract baseline
    corrected_data <- corrected_data %>% dplyr::mutate(fret = fret - min(fret))
    return(corrected_data)
  }
