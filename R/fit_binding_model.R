#' @title Fit a binding model equation to the experimental binding data
#'
#' @description This function fits a binding model equation (hyperbolic model,
#'     Hill model, or quadratic model) to experimental binding data.
#'
#' @param data A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal). The output of the
#'     \code{\link{correct_fret_signal}} function can be used directly as input
#'     here.
#' @param binding_model A binding model equation to fit to the experimental
#'     data. Possible values are \code{"hyperbolic"}, \code{"hill"} or
#'     \code{"quadratic"}.
#' @param probe_concentration The fixed concentration of probe molecule
#'     (required to use the quadratic binding model, ignored by other models).
#' @param output_directory An optional directory name where to write fit results.
#'     This directory will be created if it does not already exist.
#' @return A named list where each element is named after the corresponding
#'     experiment and holds an \code{\link[stats]{nls}} object containing the
#'     results of the fit. This list can be directly used
#'     as input to \code{\link{make_figure}}.
#' @export

fit_binding_model <- function(data,
                              binding_model = NULL,
                              probe_concentration = NULL,
                              output_directory = NULL) {
    # Sanity checks
    if (is.null(binding_model)) {
        stop("Please provide a binding model.")
    }

    # Select model and fit data
    if (binding_model == "hyperbolic") {
        fits <- fit_hyperbolic(data)
    } else if (binding_model == "hill") {
        fits <- fit_hill(data)
    } else if (binding_model == "quadratic") {
        # Sanity check
        if (is.null(probe_concentration)) {
            stop("Please provide a probe concentration to use the quadratic model.")
        }
        # Fit
        fits <- fit_quadratic(data, probe_concentration)
    } else {
        stop("Unknown binding model. Available models: 'hyperbolic', 'hill', 'quadratic'.")
    }

    # Optionally write the results in TXT files in the specified output directory
    if (!is.null(output_directory)) {
        # Sanity check
        if (length(output_directory) > 1) {
            stop("Please provide a single directory name.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }
        fit_summaries <- fits %>%
            broom::tidy(fit) %>%
            dplyr::select(Experiment, term, estimate, std.error)
        colnames(fit_summaries) <- c("Experiment",
                                     "parameter",
                                     "estimate",
                                     "std.error")
        # Split single dataframe by experiment name, then write each fit summary
        # to a CSV file in the output directory
        fit_summaries %>%
            split(fit_summaries$Experiment) %>%
            mapply(readr::write_csv,
                   .,
                   path = paste(output_directory,
                                "/",
                                names(.),
                                "_",
                                binding_model,
                                "-fit-summary.csv",
                                sep = ""))
    }

    # Build a named list where items are named after experiments and contain
    # the corresponding fit object
    results <- fits$fit
    names(results) <- fits$Experiment

    # Return results
    results
}

#' @title Fit the Hill model equation to the experimental binding data
#'
#' @description This internal function fits a the Hill model equation to the
#'     experimental binding data.
#'
#' @param data A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal).
#' @return An \code{\link[stats]{nls}} object containing the results of the fit.

fit_hill <- function(data) {
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

#' @title Fit the hyperbolic model equation to the experimental binding data
#'
#' @description This internal function fits a the hyperbolic model equation to
#'     the experimental binding data.
#'
#' @param data A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal).
#' @return An \code{\link[stats]{nls}} object containing the results of the fit.

fit_hyperbolic <- function(data) {
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

#' @title Fit the quadratic model equation to the experimental binding data
#'
#' @description This internal function fits a the quadratic model equation to
#'     the experimental binding data.
#'
#' @param data A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal).
#' @param probe_concentration The fixed concentration of probe molecule in the
#'     experiment.
#' @return An \code{\link[stats]{nls}} object containing the results of the fit.

fit_quadratic <- function(data, probe_concentration) {
    model = as.formula(signal ~ quadratic(concentration,
                                          parameters = list(
                                              signal_min = signal_min,
                                              signal_max = signal_max,
                                              kd = kd,
                                              probe_conc = probe_concentration)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = minpack.lm::nlsLM(formula = model,
                                          data = .,
                                          start = guess_parameters(.),
                                          lower = c(0, 0, 0)))
}
