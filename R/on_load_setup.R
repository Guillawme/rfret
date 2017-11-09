# Create package-local environment (child of empty_env).
.rfret <- rlang::new_environment()

#' @title Load internal data
#'
#' @description This internal function loads internal data when the package gets
#'     loaded (either by \code{\link[base]{library()}} or by the double-colon
#'     operator).
#' @param libname A character string giving the library directory where the
#'     package defining the namespace was found. See \code{\link[base]{ns-hooks}}.
#' @param pkgname A character string giving the name of the package. See
#'     \code{\link[base]{ns-hooks}}.
#' @return The names of loaded objects (invisibly, see \code{\link[base]{load}}).

.onLoad <- function(libname, pkgname) {
    # The R/sysdata.rda file contains variables prepared by
    # data-raw/make_default_metadata.R to populate the .rfret_metadata
    # environment.
    load(file = "R/sysdata.rda",
         envir = .rfret)
}

#' @title Package startup messages
#'
#' @description This internal function is run when the package gets attached by
#'     \code{\link[base]{library()}}.
#' @param libname A character string giving the library directory where the
#'     package defining the namespace was found. See \code{\link[base]{ns-hooks}}.
#' @param pkgname A character string giving the name of the package. See
#'     \code{\link[base]{ns-hooks}}.
#' @return Nothing, simply used for the side effect of displaying messages.

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Successfully loaded ", pkgname, " from ", libname)
}
