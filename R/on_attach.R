#' @title Package startup messages
#'
#' @description This internal function is run when the package gets attached by
#'     \code{\link[base]{library}}.
#' @param libname A character string giving the library directory where the
#'     package defining the namespace was found. See \code{\link[base]{ns-hooks}}.
#' @param pkgname A character string giving the name of the package. See
#'     \code{\link[base]{ns-hooks}}.
#' @return Nothing, simply used for the side effect of displaying messages.

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Successfully loaded ", pkgname, " from ", libname)
}

# Make R CMD check ignore the following symbols.
# This is to silence "no visible binding to global variable" notes that are
# irrelevant because these symbols are column names in dataframes.
utils::globalVariables(names = c(".",
                                 ".resid",
                                 "Experiment",
                                 "Observation",
                                 "Replicate",
                                 "Type",
                                 "acceptor_channel",
                                 "anisotropy",
                                 "concentration",
                                 "donor_channel",
                                 "estimate",
                                 "fit",
                                 "fret_channel",
                                 "intensity",
                                 "intermediate",
                                 "parallel",
                                 "perpendicular",
                                 "polarization",
                                 "signal",
                                 "status",
                                 "std.error",
                                 "term",
                                 "x"),
                       package = "rfret",
                       add = TRUE)
