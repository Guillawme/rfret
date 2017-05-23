#' @title Michaelis-Menten equation
#'
#' @description This function sets up the Michaelis-Menten equation for use by
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}. If a Hill
#'     coefficient different from 1 is specified in the set of parameters, this
#'     equation will describe a Hill model.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), a Hill coefficient (\code{n}), and values of minimal and
#'     maximal signal (\code{signal_min}, \code{signal_max}).
#' @export

hyperbola <- function(titrated_conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd
    n <- parameters$n

    # Equation definition
    s_min + (s_max - s_min) * titrated_conc^n / (kd + titrated_conc^n)
}

#' @title Quadratic equation
#'
#' @description This function sets up the quadratic equation for use by
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), a probe concentration (\code{probe_conc}), and values of
#'     minimal and maximal signal (\code{signal_min}, \code{signal_max}).
#' @export

quadratic <- function(titrated_conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd
    probe_conc <- parameters$probe_conc

    # Equation definition
    s_min + (s_max - s_min) *
        ( (kd + probe_conc + titrated_conc) -
              sqrt( (-(kd + probe_conc + titrated_conc))^2 -
                        4 * probe_conc * titrated_conc ) ) /
        (2 * probe_conc)
}

#' @title Quadratic homodimer equation
#'
#' @description This function sets up the quadratic homodimer equation for use
#'     by \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param parameters A named list of parameters to be refined against the
#'     experimental data. Parameters for this equation are a binding constant
#'     (\code{kd}), a probe concentration (\code{probe_conc}), and values of
#'     minimal and maximal signal (\code{signal_min}, \code{signal_max}).
#' @export

quadratic_homodimer <- function(titrated_conc, parameters) {
    # Give shorter names to parameters, to make the equation easier to read
    s_min <- parameters$signal_min
    s_max <- parameters$signal_max
    kd <- parameters$kd
    probe_conc <- parameters$probe_conc

    # Equation definition
    s_min + (s_max - s_min) *
        ( 4 * (probe_conc + titrated_conc) + kd -
             sqrt(kd^2 + 8 * (probe_conc + titrated_conc) * kd) ) *
        titrated_conc / (4 * (probe_conc + titrated_conc)^3)
}
