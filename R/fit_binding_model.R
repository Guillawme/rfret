#' @title Fit a binding model equation to the experimental binding data
#'
#' @description This function fits a binding model equation (hyperbolic model,
#'     Hill model, or quadratic model) to experimental binding data.
#'
#' @param data A dataframe containing the binding signal. It must contain
#'     at least two columns: \code{concentration} (the ligand concentration) and
#'     \code{signal} (the observed binding signal). The output of the
#'     \code{\link{fret_correct_signal}} and \code{\link{fp_use_signal}}
#'     functions can be used directly as input here.
#' @param binding_model A binding model equation to fit to the experimental
#'     data. Possible values are \code{"hyperbolic"}, \code{"hill"} or
#'     \code{"quadratic"}.
#' @param probe_concentration The fixed concentration of probe molecule
#'     (required to use the quadratic binding model, ignored by other models).
#' @param output_directory An optional directory name where to write fit results.
#'     This directory will be created if it does not already exist.
#' @return A named list where each element is named after the corresponding
#'     experiment and holds an \code{\link[stats]{nls}} object containing the
#'     results of the fit. Only succesful fits will be returned. This list can
#'     be directly used as input for \code{\link{make_figure}}.
#' @importFrom magrittr %>%
#' @export

fit_binding_model <- function(data,
                              binding_model,
                              probe_concentration = NULL,
                              output_directory = NULL) {
    # Select model and fit data
    fits <- switch(binding_model,
                   "hyperbolic" = fit_hyperbolic(data),
                   "hill"       = fit_hill(data),
                   "quadratic"  = fit_quadratic(data, probe_concentration),
                   stop("Unknown binding model: ", binding_model, "\nAvailable models: hyperbolic, hill, quadratic."))

    # Filter out failed fits
    fits_success <- fits %>%
        dplyr::filter(status == "success")
    # Stop here if there is no successful fit left...
    if(nrow(fits_success) == 0) {
        stop("All fits failed. Check your raw data.")
    }
    # Report failed fits so the user can investigate
    fits_failure <- fits %>%
        dplyr::filter(status == "failure")
    if(nrow(fits_failure) > 0) {
        warning("Failed to fit datasets:\n",
                paste(fits_failure$Experiment, collapse = "\n"),
                call. = FALSE)
    }

    # Optionally write the results in CSV files in the specified output directory
    if (!is.null(output_directory)) {
        # Sanity check
        if (length(output_directory) > 1) {
            stop("Please provide a single directory name.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }

        # Filter out failed fits
        fits_success %>%
            dplyr::ungroup() %>%
            # Tidy results
            broom::tidy(fit) %>%
            dplyr::select(Experiment, parameter = term, estimate, std.error) %>%
            # Split by experiment name, then write each fit summary to a CSV
            # file in the output directory
            split(.$Experiment) %>%
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
    # the corresponding fit object. Return only successful fits.
    results <- fits_success$fit
    names(results) <- fits_success$Experiment
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
#' @importFrom magrittr %>%

fit_hill <- function(data) {
    model <- stats::as.formula(signal ~ hill(concentration,
                                             parameters = list(
                                                 signal_min = signal_min,
                                                 signal_max = signal_max,
                                                 kd = kd,
                                                 n = n)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = try(minpack.lm::nlsLM(formula = model,
                                              data = .,
                                              start = c(guess_parameters(.), n = 1),
                                              lower = c(0, 0, 0, 0)),
                            silent = TRUE)) %>%
        dplyr::mutate(status = ifelse(class(fit) == "nls", "success", "failure"))
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
#' @importFrom magrittr %>%

fit_hyperbolic <- function(data) {
    model <- stats::as.formula(signal ~ hyperbolic(concentration,
                                                   parameters = list(
                                                       signal_min = signal_min,
                                                       signal_max = signal_max,
                                                       kd = kd)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = try(minpack.lm::nlsLM(formula = model,
                                              data = .,
                                              start = guess_parameters(.),
                                              lower = c(0, 0, 0)),
                            silent = TRUE)) %>%
        dplyr::mutate(status = ifelse(class(fit) == "nls", "success", "failure"))
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
#' @importFrom magrittr %>%

fit_quadratic <- function(data, probe_concentration) {
    model <- stats::as.formula(signal ~ quadratic(concentration,
                                                  parameters = list(
                                                      signal_min = signal_min,
                                                      signal_max = signal_max,
                                                      kd = kd,
                                                      probe_conc = probe_concentration)))
    data %>%
        dplyr::group_by(Experiment) %>%
        dplyr::do(fit = try(minpack.lm::nlsLM(formula = model,
                                              data = .,
                                              start = guess_parameters(.),
                                              lower = c(0, 0, 0)),
                            silent = TRUE)) %>%
        dplyr::mutate(status = ifelse(class(fit) == "nls", "success", "failure"))
}
