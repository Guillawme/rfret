#' @title Michaelis-Menten equation
#'
#' @description This function sets up the Michaelis-Menten equation for use by
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}. If a Hill
#'     coefficient different from 1 is specified, this equation describes a
#'     Hill model.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param kd Binding constant.
#' @param n Hill coefficient. Defaults to 1.
#' @param signal_min Minimal observed signal.
#' @param signal_max Maximal observed signal.
#' @param probe_conc Fixed concentration of probe.
#' @export

hyperbola <- function(titrated_conc,
                      kd,
                      n = 1,
                      signal_min,
                      signal_max,
                      probe_conc) {
    signal_min + (signal_max - signal_min) *
        titrated_conc^n / (kd + titrated_conc^n)
}

#' @title Quadratic equation
#'
#' @description This function sets up the quadratic equation for use by
#'     \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param kd Binding constant.
#' @param n Not used in this equation.
#' @param signal_min Minimal observed signal.
#' @param signal_max Maximal observed signal.
#' @param probe_conc Fixed concentration of probe.
#' @export

quadratic <- function(titrated_conc,
                      kd,
                      n,
                      signal_min,
                      signal_max,
                      probe_conc) {
    signal_min + (signal_max - signal_min) *
        ( (kd + probe_conc + titrated_conc) -
              sqrt( (-(kd + probe_conc + titrated_conc))^2 -
                        4 * probe_conc * titrated_conc ) ) / (2 * probe_conc)
}

#' @title Quadratic homodimer equation
#'
#' @description This function sets up the quadratic homodimer equation for use
#'     by \code{\link{fit_binding_model}} or \code{\link[stats]{nls}}.
#'
#' @param titrated_conc Concentrations in the titration series.
#' @param kd Binding constant.
#' @param n Not used in this equation.
#' @param signal_min Minimal observed signal.
#' @param signal_max Maximal observed signal.
#' @param probe_conc Fixed concentration of probe.
#' @export

quadratic_homodimer <- function(titrated_conc,
                                kd,
                                n,
                                signal_min,
                                signal_max,
                                probe_conc) {
    signal_min + (signal_max - signal_min) *
        ( 4 * (probe_conc + titrated_conc) + kd -
             sqrt(kd^2 + 8 * (probe_conc + titrated_conc) * kd) ) *
        titrated_conc / (4 * (probe_conc + titrated_conc)^3)
}
