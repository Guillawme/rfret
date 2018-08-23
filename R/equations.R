#' @title Hyperbolic model equation
#'
#' @description This function sets up the hyperbolic model equation for use by
#'     \code{\link{fit_binding_model}}.
#'
#' @param conc Ligand concentration.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), and values of minimal and maximal signal (\code{signal_min},
#'     \code{signal_max}).
#' @export

hyperbolic <- function(conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd

    # Equation definition
    s_min + (s_max - s_min) * conc / (kd + conc)
}

#' @title Hill model equation
#'
#' @description This function sets up the Hill model equation for use by
#'     \code{\link{fit_binding_model}}.
#'
#' @param conc Ligand concentration.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), a Hill coefficient (\code{n}), and values of minimal and
#'     maximal signal (\code{signal_min}, \code{signal_max}).
#' @export

hill <- function(conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd
    n <- parameters$n

    # Equation definition
    s_min + (s_max - s_min) * conc^n / (kd + conc^n)
}

#' @title Quadratic model equation
#'
#' @description This function sets up the quadratic model equation for use by
#'     \code{\link{fit_binding_model}}.
#'
#' @param conc Ligand concentration.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), a probe concentration (\code{probe_conc}), and values of
#'     minimal and maximal signal (\code{signal_min}, \code{signal_max}).
#' @export

quadratic <- function(conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd
    probe_conc <- parameters$probe_conc

    # Equation definition
    s_min + (s_max - s_min) / (2 * probe_conc) *
      ((kd + probe_conc + conc) -
           sqrt((kd + probe_conc + conc)^2 - 4 * probe_conc * conc))
}
