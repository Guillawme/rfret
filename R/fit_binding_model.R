#' @title Fit a binding model equation to the experimental FRET data
#'
#' @description This function uses initial guesses of a binding model equation
#'     parameters to fit this equation to the corrected experimental FRET data.
#'
#' @param fret_corrected A dataframe containing the corrected FRET signal. It
#'     must contain at least two columns named \code{fret_corrected} and
#'     \code{concentration}. The output of the \code{\link{correct_fret_signal}}
#'     function can be used directly as input here.
#' @param donor_concentration The concentration of donor-labeled molecule.
#'     Defaults to 10.
#' @param parameters A named list containing initial guesses for the parameters
#'     of the equation associated to the binding model. The output of
#'     \code{\link{guess_quadratic_parameters}} can be used directly as
#'     input here.
#' @return A list containing estimates and standard errors of the model
#'     parameters from \code{\link[stats]{nls}}, stored in a
#'     data frame (list item named \code{results}), and a \code{ggplot2}
#'     graph object of the data points and fit curve (list item named
#'     \code{graph}).
#' @export

fit_binding_model <- function(fret_corrected,
                              donor_concentration = 10,
                              parameters){
    # Get donor concentration
    donor_conc <- donor_concentration

    # Define the equation to be fitted to the data
    equation <- c(
        "fret_corrected ~
         fret_min + (fret_max - fret_min) *
         ( (kd + donor_conc + concentration) -
           sqrt( (-(kd + donor_conc + concentration))^2 -
           4 * donor_conc * concentration )
         ) /
         (2 * donor_conc)"
    )

    # Fit equation to the experimental data, using the provided initial guesses
    # of parameters
    fit_curve <- nls(formula = equation,
                     data = fret_corrected,
                     start = parameters)

    # Build a result table
    params <- broom::tidy(fit_curve)
    result_table <- params[c("term", "estimate", "std.error")]

    # Build a data frame containing original data, prediction from fit and
    # residuals
    dat <- broom::augment(fit_curve, fret_corrected)
    graph_data <- dat[c("concentration",
                        "fret_corrected",
                        ".fitted",
                        ".resid")]

    # Build a graph with data points and fit curve
    final_graph <- ggplot2::ggplot(data = graph_data) +
        ggplot2::geom_point(ggplot2::aes(x = concentration,
                                         y = fret_corrected)) +
        ggplot2::geom_line(ggplot2::aes(x = concentration,
                                        y = .fitted)) +
        ggplot2::theme_bw() +
        ggplot2::scale_x_log10() +
        ggplot2::xlab("Concentration") +
        ggplot2::ylab("FRET corrected")

    # Return result table and graph
    list(results = result_table,
         graph = final_graph)
}
