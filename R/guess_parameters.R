#' @title Guess initial values for parameters of the binding model equations
#'
#' @description This function provides reasonable initial guesses for \code{kd},
#'     \code{fmin} and \code{fmax} (parameters of the binding model
#'     equations used to fit the data) 
#'
#' @param fret_corr A dataframe containing the corrected FRET signal. It must
#'     contain at least two columns:
#'     - \code{concentration}: ligand concentration,
#'     - \code{fret}: the corrected FRET signal.
#'     
#' @return A list containing initial guesses for \code{kd}, \code{fmin}
#'     and \code{fmax}.
#' @export

guess_parameters <- function(fret_corr) {
    # Minimal value of measured FRET signal is a good approximation
    # for signa_min
    fmin_guess <- min(fret_corr$fret)

    # Maximal value of measured FRET signal is not a perfect approximation for
    # signal_max (because experimental curves rarely saturate), but good enough
    # for an initial guess
    fmax_guess <- max(fret_corr$fret)

    # As an initial guess of kd, we simply take the concentration value at
    # half-maximum FRET signal
    half_fret <- with(fret_corr, (max(fret) - min(fret)) / 2)
    i <- which.min( abs(fret_corr$fret - half_fret) )
    kd_guess <- fret_corr$concentration[i]

    list(kd   = kd_guess,
         fmin = fmin_guess,
         fmax = fmax_guess)
}
