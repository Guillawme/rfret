#' @title Extract parameter estimates and standard errors from fit output
#'
#' @description This function extracts best fit parameter estimates and standard
#'     errors from the output of \code{\link{fit_binding_model}}
#'
#' @param fits The output of the \code{\link{fit_binding_model}}
#' @return A data frame with parameter names, parameter estimates and standard
#'     errors.
#' @export

extract_fit_parameters = function(fit.output){
  fit.results =
    fit.output %>%
    broom::tidy(fit) %>%
    dplyr::select(Experiment, term, estimate, std.error)
  colnames(fit.results) = c("Experiment", "parameter", "estimate", "std.err")
  return(fit.results)
}
