#' @title Average technical replicates
#'
#' @description This function calculates averages of fluorescence values from
#'     an arbitrary number of technical replicates.
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     contain at least 8 columns named 
#'     - \code{Experiment}: A unique string identifier for each experiment
#'     - \code{Type}: Either "blank" (no donor) or "titration" (donor present)
#'     - \code{Replicate}: An integer that indicates the technical replicate 
#'     number (ie, 1, 2,...) 
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
#' @return A dataframe containing the reduced dataset after averaging across
#'     replicates. It contains all of the above columns *except* 
#'     \code{Replicate} (becuase we average over replicates)
#' @export

average_technical_replicates <- function(raw_data) {
  raw_data %>% 
    group_by(Experiment, Type, Observation, concentration) %>%
    summarise(fret = mean(fret_channel), 
              donor = mean(donor_channel),
              acceptor = mean(acceptor_channel))
}
