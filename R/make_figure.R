#' @title Make a figure with experimental data and fitted binding curve
#'
#' @description This function takes a model object from
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}} and returns
#'     a result table with with parameters and their standard errors, and
#'     three plots for the binding curve, the residual plot and the final
#'     figure.
#'
#' @param corrected_data A dataframe containing the corrected FRET signal. It
#'     must contain at least two columns named \code{fret_corrected} and
#'     \code{concentration}. The output of the \code{\link{correct_fret_signal}}
#'     function can be used directly as input here.
#' @param fit A model object from \code{\link{fit_binding_model}} or
#'     \code{\link[stats]{nls}}.
#' @return A list containing estimates and standard errors of the model
#'     parameters from \code{\link[stats]{nls}}, stored in a
#'     data frame (list item named \code{results}), and three \code{ggplot2}
#'     graph objects of the data points and fit curve (list item named
#'     \code{binding_curve}), residual plot (list item named
#'     \code{residual_plot}) and final figure with both plots (list item named
#'     \code{final_figure}).
#' @export

make_figure <- function(corrected_data, fit) {
    # Build a result table
    params <- broom::tidy(fit)
    result_table <- params[c("term", "estimate", "std.error")]

    # Build a data frame containing original data, prediction from fit and
    # residuals
    dat <- broom::augment(fit, corrected_data)
    graph_data <- dat[c("concentration",
                        "fret_corrected",
                        ".fitted",
                        ".resid")]

    # Build a graph with data points and fit curve, and a smaller residual plot
    # below the main graph
    binding_plot <- ggplot2::ggplot(data = graph_data) +
        ggplot2::geom_point(ggplot2::aes(x = concentration,
                                         y = fret_corrected)) +
        ggplot2::geom_line(ggplot2::aes(x = concentration,
                                        y = .fitted)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("FRET corrected")
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
                                 rel_heights=c(7, 3))

    # Return result table and plots
    list(results = result_table,
         binding_curve = binding_plot,
         residual_plot = resid_plot,
         final_figure = figure)
}
