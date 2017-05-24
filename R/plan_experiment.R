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
#'     \code{hyperbola} and \code{quadratic}. Defaults to \code{hyperbola}.
#' @param probe_conc Fixed concentration of probe molecule. If not specified,
#'     the hyperbola binding model equation is used by default.
#' @param hill An optional Hill coefficient. This is ignored by the
#'     \code{quadratic} binding model equation.
#' @return A \code{ggplot2} graph object of the simulated binding curve.
#' @export

plan_experiment <- function(kd,
                            min_concentration = 1e-3,
                            max_concentration = 1e5,
                            binding_model = hyperbola,
                            probe_conc = NULL,
                            hill = NULL) {
    # Sanity checks
    if (is.null(hill)) { hill <- 1 }
    if (is.null(probe_conc)) {
        if (identical(binding_model, quadratic)) {
            warning("No probe concentration specified. Using hyperbola equation instead.")
        }
        binding_model <- hyperbola
    }

    # Make plot
    my_plot <- ggplot2::ggplot(data = data.frame(x = min_concentration:max_concentration),
                               ggplot2::aes(x = x)) +
        ggplot2::stat_function(fun = binding_model,
                               args = list(list(kd = kd,
                                                signal_min = 0,
                                                signal_max = 100,
                                                probe_conc = probe_conc,
                                                n = hill))) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("Signal (% saturation)") +
        ggplot2::ggtitle("Simulated binding curve")

    # Return plot
    my_plot
}
