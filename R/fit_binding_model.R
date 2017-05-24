#' @title Fit a binding model equation to the experimental FRET data
#'
#' @description This function uses initial guesses of \code{kd}, \code{fret_min}
#'     and \code{fret_max} to fit a binding model equation to the corrected
#'     experimental FRET data. This function is a wrapper around
#'     \code{\link[minpack.lm]{nlsLM}} and provides reasonable default 
#'     settings (ie refine only \code{kd}, \code{fret_min} and \code{fret_max},
#'     and possibly the Hill coefficient \code{n}, but keep probe concentration 
#'     fixed).
#'
#' @param data_to_fit A dataframe containing the corrected FRET signal. It must
#'     contain at least two columns:
#'     - \code{concentration}: ligand concentration,
#'     - \code{fret}: the corrected FRET signal.
#'     The output of the \code{\link{correct_fret_signal}} function can be used
#'     directly as input here.
#' @param binding_model A binding model equation to fit to the experimental
#'     data. Possible values are \code{hyperbola} or \code{quadratic}.
#' @param donor_concentration The concentration of donor-labeled molecule
#'     (required for the quadratic binding model)
#' @param fit_Hill_coef A Boolean value (TRUE/FALSE) that indicates whether to 
#'     fit for the Hill coefficient in the hyperbolic model. If FALSE, the
#'     Hill coefficent is fixed to 1. (default = FALSE)
#' @return An named list containing: \code{fit}, an \code{\link[stats]{nls}}
#'     object containing the results of the fit; \code{binding_model}, the
#'     equation used in the fitting procedure; and \code{donor_concentration},
#'     the donor concentration used in the fitting procedure.
#'     This list can be directly used as input to \code{\link{make_figure}}.
#' @export

fit_binding_model <- function(data_to_fit,
                              model = NULL,
                              donor_concentration = NULL,
                              fit_Hill_coef = FALSE) {
  # Sanity checks
  if (is.null(model)) stop("You must provide a binding model.")
  
  # Select model and fit data
  if (model == "hyperbola") {
    if (fit_Hill_coef) {
      fits = fit.Hill(data_to_fit)
    } else {
      fits = fit.Hyperbola(data_to_fit)
    }
  } else if (model == "quadratic") {
    if (fit_Hill_coef) {
      stop("Invalid option: fit_Hill_coef = TRUE is a valid option only for model = hyperbola. To use quadratic binding model, set fit_Hill_coef to FALSE")
    } else if (is.null(donor_concentration)) {
      stop("Missing donor concentration. You must provide a donor concentration for the quadratic binding model")
    } else {
      fits = fit.Quadratic(data_to_fit, donor_concentration)
    }
  } else {
    stop("Invalid model. Must be either hyperbola or quadratic")
  }
  return(fits)
}

# fit to Hill function with n as a free parameter
fit.Hill = function(data){
  model = as.formula(fret ~ hyperbola(concentration, 
                                      parameters = list(
                                        signal_min = fmin,
                                        signal_max = fmax,
                                        kd = kd,
                                        n = n)))
  data %>%
    group_by(Experiment) %>%
    do(fit = nlsLM(formula = model,
                   data = .,
                   start = c(guess_parameters(.), n=1),
                   lower = c(0, 0, 0, 0)))
}

# fit to Hill function with n=1 (hyperbolic fit)
fit.Hyperbola = function(data){
  model = as.formula(fret ~ hyperbola(concentration, 
                                      parameters = list(
                                        signal_min = fmin,
                                        signal_max = fmax,
                                        kd = kd,
                                        n = 1)))
  data %>%
    group_by(Experiment) %>%
    do(fit = nlsLM(formula = model,
                   data = .,
                   start = guess_parameters(.),
                   lower =c(0, 0, 0)))
}

fit.Quadratic = function(data, donor_concentration){
  model = as.formula(fret ~ quadratic(concentration,
                                      parameters = list(
                                        signal_min = fmin,
                                        signal_max = fmax,
                                        kd = kd,
                                        probe_conc = donor_concentration
                                      )))
  data %>% 
    group_by(Experiment) %>%
    do(fit = nlsLM(formula = model,
                   data = .,
                   start = guess_parameters(.),
                   lower = c(0, 0, 0)))
}  
