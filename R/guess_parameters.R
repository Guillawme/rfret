#' @title Guess initial values for parameters of the binding model equations
#'
#' @description This function guesses initial values for \code{kd},
#'     \code{fret_min} and \code{fret_max} (the parameters of the binding model
#'     equations), using the corrected FRET data.
#'
#' @param fret_corr A dataframe containing the corrected FRET signal. It must
#'     contain at least two columns named \code{fret_corrected} and
#'     \code{concentration}.
#' @return A list containing initial guesses for \code{kd}, \code{fret_min}
#'     and \code{fret_max}.
#' @export

guess_parameters <- function(fret_corr) {
    # Minimal value of measured FRET signal is a good approximation
    # for signa_min
    fret_min_guess <- min(fret_corr$fret_corrected)

    # Maximal value of measured FRET signal is not a perfect approximation for
    # signal_max (because experimental curves rarely saturate), but good enough
    # for an initial guess
    fret_max_guess <- max(fret_corr$fret_corrected)

    # As an initial guess of kd, we simply take the concentration value at
    # half-maximum FRET signal
    half_fret_theoretical <- (max(fret_corr$fret_corrected) -
                                  min(fret_corr$fret_corrected)) / 2
    half_fret_measured_index <- which.min(
        abs(fret_corr$fret_corrected - half_fret_theoretical)
    )
    kd_guess <- fret_corr$concentration[half_fret_measured_index]

    list(kd       = kd_guess,
         fret_min = fret_min_guess,
         fret_max = fret_max_guess)
}
