#' @title Fit a binding model equation to the experimental FRET data
#'
#' @description This function uses initial guesses of \code{kd}, \code{fret_min}
#'     and \code{fret_max} to fit a binding model equation to the corrected
#'     experimental FRET data. This function is a wrapper around
#'     \code{\link[stats]{nls}} and provides reasonnable default settings (i.e.
#'     refine only \code{kd}, \code{fret_min} and \code{fret_max}, and possibly
#'     the Hill coefficient \code{n}, but keep probe concentration fixed).
#'
#' @param corrected_data A dataframe containing the corrected FRET signal. It
#'     must contain at least two columns named \code{fret_corrected} and
#'     \code{concentration}. The output of the \code{\link{correct_fret_signal}}
#'     function can be used directly as input here.
#' @param binding_model A binding model equation to fit to the experimental
#'     data. Possible values are \code{hyperbola}, \code{quadratic} and
#'     \code{quadratic_homodimer}.
#' @param parameters A named list containing initial guesses for \code{kd},
#'     \code{fret_min} and \code{fret_max}. The output of
#'     \code{\link{guess_parameters}} can be used directly as input here.
#' @param donor_concentration The concentration of donor-labeled molecule.
#'     Defaults to 10.
#' @return An named list containing: \code{fit}, an \code{\link[stats]{nls}}
#'     object containing the results of the fit; \code{binding_model}, the
#'     equation used in the fitting procedure; and \code{donor_concentration},
#'     the donor concentration used in the fitting procedure.
#'     This list can be directly used as input to \code{\link{make_figure}}.
#' @export

fit_binding_model <- function(corrected_data,
                              binding_model = NULL,
                              parameters,
                              donor_concentration = 10) {
    # Sanity checks
    if (is.null(binding_model) || !is.function(binding_model)) {
        stop("You must provide a binding model.")
    }

    # Check if we need to refine a Hill coefficient
    # If not, we lock it to 1 to have the normal binding model
    if (exists("n", where = parameters)) {
        n <- parameters$n
    } else {
        n <- 1
    }

    # Fit binding model to the experimental data, using the provided initial
    # guesses of parameters
    fit <- stats::nls(formula =
                          fret_corrected ~ binding_model(concentration,
                                                         kd,
                                                         n,
                                                         fret_min,
                                                         fret_max,
                                                         donor_concentration),
                      data = corrected_data,
                      start = parameters)

    # Return a list containing the fit object, binding model and donor
    # concentration used to fit the model
    list(fit                 = fit,
         binding_model       = binding_model,
         donor_concentration = donor_concentration)
}
