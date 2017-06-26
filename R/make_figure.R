#' @title Make a figure with experimental data and fitted binding curve
#'
#' @description This function takes a list containing any number of model
#'     objects from \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}
#'     and returns a binding curve plot figure for each model.
#'
#' @param fits A named list where items are named after the corresponding
#'     experiment and contain a model object from \code{\link[stats]{nls}}. The
#'     output of \code{\link{fit_binding_model}} can be used directly as input
#'     here.
#' @param probe_concentration The fixed concentration of probe molecule. This is
#'     ignored for models obtained with the hyperbolic and hill equations, but
#'     required for models obtained with the quadratic equation.
#' @param output_directory An optional directory name where to write plots. This
#'     directory will be created if it does not already exist.
#' @param plot_format A character string indicating the file format to use to
#'     save plots. Possible values are \code{"png"} (default value),
#'     \code{"pdf"} and \code{"svg"}.
#' @return A named list where items are named after the corresponding experiment
#'     and contain a \code{\link[ggplot2]{ggplot}} graph object of a binding
#'     curve figure.
#' @export

make_figure <- function(fits,
                        probe_concentration = NULL,
                        output_directory = NULL,
                        plot_format = "png") {
    # Get binding models used to obtain the input fits
    binding_models <- sapply(fits, detect_binding_model)

    # Generate plot for each input model
    figures <- mapply(make_one_figure,
                      fit = fits,
                      experiment_name = names(fits),
                      binding_model = binding_models,
                      MoreArgs = list(probe_conc = probe_concentration),
                      SIMPLIFY = FALSE)

    # Optionally, write plots to files in the specified directory
    if (!is.null(output_directory)) {
        if (!(plot_format %in% c("png", "pdf", "svg"))) {
            stop("Please use one of the following output formats: 'png', 'pdf' or 'svg'.")
        }
        if (!dir.exists(output_directory)) {
            message("Creating directory: ", output_directory)
            dir.create(output_directory)
        }
        mapply(ggplot2::ggsave,
               plot = figures,
               filename = paste(names(figures),
                                "_",
                                binding_models,
                                "-model_final-figure.",
                                plot_format,
                                sep = ""),
               MoreArgs = list(path = output_directory,
                               device = plot_format,
                               width = 8.21,
                               height = 4.61,
                               units = "in",
                               dpi = 300))
    }

    # Return results
    figures
}

#' @title Make a figure with data and binding curve from a single experiment
#'
#' @description This internal function takes a model object from
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}} and returns
#'     a \code{\link[ggplot2]{ggplot}} graph object of a binding curve figure.
#'
#' @param fit An \code{\link[stats]{nls}} object. The output of
#'     \code{\link{fit_binding_model}} can be used directly as input here.
#' @param experiment_name The name of the corresponding experiment.
#' @param binding_model The binding model used to fit the data.
#' @param probe_conc The fixed concentration of probe molecule. This is ignored
#'     for models obtained with the hyperbolic and hill equations, but required
#'     for models obtained with the quadratic equation.
#' @return A \code{\link[ggplot2]{ggplot}} graph object of a binding curve
#'     figure.

make_one_figure <- function(fit,
                            experiment_name,
                            binding_model,
                            probe_conc = NULL) {
    # Get parameters from the fit in an easily accessible dataframe
    params <- broom::tidy(fit)

    # Get parameters for the binding model equation
    kd <- params$estimate[params$term == "kd"]
    s_min <- params$estimate[params$term == "signal_min"]
    s_max <- params$estimate[params$term == "signal_max"]

    # Check whether we need to retrieve a Hill coefficient for subsequent use
    if ("n" %in% params$term) {
        hill_coeff <- params$estimate[params$term == "n"]
    } else {
        hill_coeff <- 1
    }

    # Get correct equation from binding model name
    equation <- NULL
    if (!(binding_model %in% c("hyperbolic", "hill", "quadratic"))) {
        stop("Unknown binding model. Available binding models: 'hyperbolic', 'hill' and 'quadratic'.")
    } else if (binding_model == "hyperbolic") {
        equation <- hyperbolic
    } else if (binding_model == "hill") {
        equation <- hill
    } else if (binding_model == "quadratic") {
        if (is.null(probe_conc)) {
            stop("Please provide a probe concentration to use the quadratic model.")
        }
        equation <- quadratic
    }

    # Build a data frame containing original data, and predicted values and
    # residuals from fit
    graph_data <- broom::augment(fit)

    # Build a graph with data points and fit curve, and a smaller residual plot
    # below the main graph
    binding_plot <- ggplot2::ggplot(data = graph_data) +
        ggplot2::geom_point(ggplot2::aes(x = concentration,
                                         y = signal)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Signal") +
        ggplot2::stat_function(fun = equation,
                               args = list(parameters = list(kd = kd,
                                                             signal_min = s_min,
                                                             signal_max = s_max,
                                                             probe_conc = probe_conc,
                                                             n = hill_coeff))) +
        ggplot2::ggtitle(paste(experiment_name, binding_model, sep = " - "))
    resid_plot <- ggplot2::ggplot(data = graph_data) +
        ggplot2::geom_point(ggplot2::aes(x = concentration,
                                         y = .resid)) +
        ggplot2::geom_hline(yintercept = 0) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Residuals")
    figure <- cowplot::plot_grid(binding_plot,
                                 resid_plot,
                                 ncol = 1,
                                 nrow = 2,
                                 rel_heights = c(7, 3))
}

#' @title Detect which binding model equation was used to fit a dataset
#'
#' @description This internal function takes a model object from
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}} and returns
#'     the name of the binding model used to fit the dataset.
#'
#' @param fit An \code{\link[stats]{nls}} object. The output of
#'     \code{\link{fit_binding_model}} can be used directly as input here.
#' @return A character vector corresponding to the name of the binding model
#'     equation used to fit the dataset corresponding to the model object.
#'
#' @importFrom magrittr %>%

detect_binding_model <- function(fit) {
    # Get binding model name from an nls object
    binding_model_name <- NULL

    hyperbolic_detected <- fit %>%
        stats::formula() %>%
        stringr::str_detect(pattern = "hyperbolic") %>%
        sum()

    hill_detected <- fit %>%
        stats::formula() %>%
        stringr::str_detect(pattern = "hill") %>%
        sum()

    quadratic_detected <- fit %>%
        stats::formula() %>%
        stringr::str_detect(pattern = "quadratic") %>%
        sum()

    if (hyperbolic_detected) {
        binding_model_name <- "hyperbolic"
    }

    if (hill_detected) {
        binding_model_name <- "hill"
    }

    if (quadratic_detected) {
        binding_model_name <- "quadratic"
    }

    if (is.null(binding_model_name)) {
        stop("Could not detect binding model.")
        }

    # Return binding model name
    binding_model_name
}
