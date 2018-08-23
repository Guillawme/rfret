#' @title Load user-supplied metadata
#'
#' @description This internal function loads user-supplied metadata from a JSON
#'     file.
#' @param user_json_file A JSON file describing mappings between internal names
#'     and names in the actual data files. It can contain the following entries:
#'     \describe{
#'     \item{\code{content}}{The name of the column describing sample content.}
#'     \item{\code{concentration}}{The name of the column containing the
#'     concentration series.}
#'     \item{\code{fret_channel}}{The name of the column containing fluorescence
#'     intensities from the FRET channel.}
#'     \item{\code{acceptor_channel}}{The name of the column containing
#'     fluorescence intensities from the acceptor channel.}
#'     \item{\code{donor_channel}}{The name of the column containing fluorescence
#'     intensities from the donor channel.}
#'     \item{\code{titration}}{The name describing points of the titration series.}
#'     \item{\code{acceptor_only}}{The name describing points of the control
#'     series with only acceptor.}
#'     \item{\code{donor_only}}{The name describing points of the control series
#'     with only donor.}
#'     \item{\code{buffer_only}}{The name describing points of the control
#'     series with only buffer.}
#'     \item{\code{parallel}}{The name of the column containing fluorescence
#'     intensities from the parallel polarization plane.}
#'     \item{\code{perpendicular}}{The name of the column containing fluorescence
#'     intensities from the perpendicular polarization plane.}
#'     \item{\code{polarization}}{The name of the column containing fluorescence
#'     polarization.}
#'     \item{\code{anisotropy}}{The name of the column containing fluorescence
#'     anisotropy.}
#'     \item{\code{intensity}}{The name of the column containing fluorescence
#'     intensity.}
#'     \item{\code{baseline}}{The name describing points of the baseline series
#'     in a fluorescence polarization or anisotropy dataset.}}
#' @return An invisible copy of the original environment containing default
#'     metadata. As a side effect, updates the column and sample names in the
#'     package-wide environment.
#'
#' @importFrom magrittr %>%

get_user_metadata <- function(user_json_file) {
    # Sanity check
    if (!file.exists(user_json_file)) {
        stop("Metadata file not found:", user_json_file)
    }

    # Remember default metadata for when we need to reset it.
    default_metadata <- get(x = "metadata", envir = .rfret)

    # Read new metadata supplied by the user.
    user_metadata <- user_json_file %>%
        jsonlite::read_json() %>%
        rlang::syms()

    # Merge them with default metadata.
    current_metadata <- get(x = "metadata", envir = .rfret)
    new_metadata <- utils::modifyList(x = current_metadata,
                                      val = user_metadata)

    # Store the new metadata in the environment.
    assign(x = "metadata", value = new_metadata, envir = .rfret)

    # Return original default metadata, so functions calling get_user_metadata()
    # can reset them to the default values.
    invisible(default_metadata)
}
