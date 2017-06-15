#' @title Fit a binding model equation to the experimental FRET data
#'
#' @description This function fits corrected FRET data to a binding model.
#'
#' @param data_to_fit A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal). The output of the
#'     \code{\link{correct_fret_signal}} function can be used directly as input
#'     here.
#' @param binding_model A binding model equation to fit to the experimental
#'     data. Possible values are \code{"hyperbolic"} or \code{"quadratic"}.
#' @param probe_concentration The fixed concentration of probe molecule
#'     (required to use the quadratic binding model).
#' @param fit_hill_coef A Boolean value (TRUE or FALSE) that indicates whether
#'     to fit for the Hill coefficient in the hyperbolic model. If it is
#'     \code{FALSE} (the default value), the Hill coefficent is fixed to 1.
#' @return A named list where each element is named after the corresponding
#'     experiment and holds an \code{\link[stats]{nls}}
#'     object containing the results of the fit. This list can be directly used
#'     as input to \code{\link{make_figure}}.
#' @export

fit_binding_model <- function(data_to_fit,
                              binding_model = NULL,
                              donor_concentration = NULL,
                              fit_hill_coef = FALSE) {
    # Sanity checks
    if (is.null(binding_model)) stop("You must provide a binding model.")

    # Select model and fit data
    if (binding_model == "hyperbolic") {
        if (fit_hill_coef) {
            fits <- fit_hill(data_to_fit)
        } else {
            fits <- fit_hyperbolic(data_to_fit)
        }
    } else if (binding_model == "quadratic") {
        if (fit_hill_coef) {
            stop("Invalid option: fit_hill_coef = TRUE is a valid option only for binding_model = 'hyperbolic'. To use the quadratic binding model, set fit_Hill_coef to FALSE.")
        } else if (is.null(donor_concentration)) {
            stop("Missing donor concentration. You must provide a donor concentration to use the quadratic binding model.")
        } else {
            fits <- fit_quadratic(data_to_fit, donor_concentration)
        }
    } else {
        stop("Invalid model. Must be either 'hyperbolic' or 'quadratic'.")
    }
    return(fits)
}

# fit to Hill function with n as a free parameter
fit_hill = function(data){
    model <- as.formula(signal ~ hyperbolic(concentration,
                                            parameters = list(
                                                signal_min = signal_min,
                                                signal_max = signal_max,
                                                kd = kd,
                                                n = n)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = minpack.lm::nlsLM(formula = model,
                                          data = .,
                                          start = c(guess_parameters(.), n = 1),
                                          lower = c(0, 0, 0, 0)))
}

# fit to Hill function with n = 1 (hyperbolic fit)
fit_hyperbolic = function(data){
    model = as.formula(signal ~ hyperbolic(concentration,
                                           parameters = list(
                                               signal_min = signal_min,
                                               signal_max = signal_max,
                                               kd = kd,
                                               n = 1)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = minpack.lm::nlsLM(formula = model,
                                          data = .,
                                          start = guess_parameters(.),
                                          lower =c(0, 0, 0)))
}

fit_quadratic = function(data, donor_concentration){
    model = as.formula(signal ~ quadratic(concentration,
                                          parameters = list(
                                              signal_min = signal_min,
                                              signal_max = signal_max,
                                              kd = kd,
                                              probe_conc = donor_concentration)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = minpack.lm::nlsLM(formula = model,
                                          data = .,
                                          start = guess_parameters(.),
                                          lower = c(0, 0, 0)))
}
