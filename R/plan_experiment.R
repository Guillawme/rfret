#' @title Plan a binding assay experiment
#'
#' @description This function takes a value of Kd, a minimal and maximal
#'     concentration of titrant a binding model equation, an optional probe
#'     concentration and an optional Hill coefficient, and outputs a plot of the
#'     theoretical resulting binding curve. This is useful for planning an
#'     experiment, when knowing the order of magnitude of Kd, to ensure that the
#'     experimental binding curve will display both the lower and upper plateaus.
#'
#' @param kd A Kd value.
#' @param min_concentration The minimal value of the concentration range to
#'     simulate the binding curve over. Defaults to \code{1e-3}.
#' @param max_concentration The maximal value of the concentration range to
#'     simulate the binding curve over. Defaults to \code{1e5}.
#' @param binding_model A binding model equation. Possible values are
#'     \code{"hyperbolic"}, \code{"hill"} and \code{"quadratic"}. Defaults to
#'     \code{"hyperbolic"}.
#' @param probe_conc Fixed concentration of probe molecule. If not specified,
#'     the hyperbolic binding model equation is used by default.
#' @param hill_coef An optional Hill coefficient. This is ignored by the
#'     quadratic binding model equation. Defaults to 1.
#' @return A \code{ggplot2} graph object of the simulated binding curve.
#' @export

plan_experiment <- function(kd,
                            min_concentration = 1e-3,
                            max_concentration = 1e5,
                            binding_model = "hyperbolic",
                            probe_conc = NULL,
                            hill_coef = 1) {
    # Prepare parameters, model equation and plot title
    my_equation <- NULL
    my_plot_title <- NULL
    if (binding_model == "hyperbolic") {
        my_equation <- hyperbolic
        my_plot_title <- paste("Simulated curve: hyperbolic model,",
                               "Kd =",
                               kd)
    } else if (binding_model == "hill") {
        my_equation <- hill
        my_plot_title <- paste("Simulated curve: hill model,",
                               "Kd =",
                               kd,
                               ", Hill coefficient =",
                               hill_coef)
    } else if (binding_model == "quadratic") {
        if (is.null(probe_conc)) {
            stop("Please specify a probe concentration to use the quadratic model.")
        }
        my_equation <- quadratic
        my_plot_title <- paste("Simulated curve: quadratic model,",
                               "Kd =",
                               kd,
                               ", probe concentration =",
                               probe_conc)
    } else {
        stop("Unknown binding model. Available binding models: 'hyperbolic', 'hill, 'quadratic'.")
    }

    my_plot <- ggplot2::ggplot(data = data.frame(x = min_concentration:max_concentration),
                               ggplot2::aes(x = x)) +
        ggplot2::stat_function(fun = my_equation,
                               args = list(list(kd = kd,
                                                signal_min = 0,
                                                signal_max = 100,
                                                probe_conc = probe_conc,
                                                n = hill_coef))) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Signal (% saturation)") +
        ggplot2::ggtitle(my_plot_title)

    # Return plot
    my_plot
}
