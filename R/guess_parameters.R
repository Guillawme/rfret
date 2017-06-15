#' @title Guess initial values for parameters of the binding model equations
#'
#' @description This function provides reasonable initial guesses for \code{kd},
#'     \code{signal_min} and \code{signal_max} (parameters of the binding model
#'     equations used to fit the data).
#' @param input_data A dataframe containing the corrected binding signal. It must
#'     contain at least two columns: \code{concentration} (the ligand
#'     concentration) and \code{signal} (the observed binding signal).
#' @return A named list containing initial guesses for \code{kd},
#'     \code{signal_min} and \code{signal_max}.
#' @export

guess_parameters <- function(input_data) {
    # Minimal value of measured binding signal is a good approximation
    # for signal_min
    smin_guess <- min(input_data$signal)

    # Maximal value of measured binding signal is not a perfect approximation
    # for signal_max (because experimental curves rarely saturate), but good
    # enough for an initial guess
    smax_guess <- max(input_data$signal)

    # As an initial guess of kd, we simply take the concentration value at
    # half-maximum binding signal
    half_smax_theoretical <- with(input_data, (max(signal) - min(signal)) / 2)
    half_smax_index <- which.min(abs(input_data$signal - half_smax_theoretical))
    kd_guess <- input_data$concentration[half_smax_index]

    # Return guessed initial parameters as a list
    list(kd         = kd_guess,
         signal_min = smin_guess,
         signal_max = smax_guess)
}
