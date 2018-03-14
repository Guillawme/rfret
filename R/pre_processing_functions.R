#' @title Pre-process a single FRET dataset
#'
#' @description This internal function pre-processes a single dataframe to
#'     generate the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{fret_average_replicates}} and \code{\link{fret_correct_signal}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name The name of the corresponding experiment.
#' @return A dataframe containing 8 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{fret_channel},
#'     \code{acceptor_channel}, \code{donor_channel} and \code{concentration}.
#' @examples
#' \dontrun{
#' format_one_dataset(my_data, "my_experiment")
#' }
#'
#' @importFrom magrittr %>%

fret_format_one_dataset <- function(raw_data, experiment_name) {
    # Generate an Experiment column, and retrieve experiment name here
    raw_data$Experiment <- experiment_name

    # Data formatting pipeline:
    # Generate columns Type and Replicate, based on column content
    raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = stringr::str_split(rlang::UQ(.rfret$metadata$content),
                                                        pattern = stringr::fixed(".")),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate) %>%
        dplyr::ungroup() %>%
        # Generate an Observation column, to label observations across different
        # types of experiments (controls and titration) and replicates
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation = rlang::UQ(.rfret$metadata$concentration) %>%
                          dplyr::desc() %>%
                          dplyr::row_number()
        ) %>%
        dplyr::ungroup() %>%
        # Rename sample type identifyers
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$acceptor_only)),
                                     "acceptor_only")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$donor_only)),
                                     "donor_only")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$titration)),
                                     "titration")) %>%
        # Reorder and rename columns, and return dataset
        dplyr::select(Experiment,
                      Type,
                      Replicate,
                      Observation,
                      fret_channel     = rlang::UQ(.rfret$metadata$fret_channel),
                      acceptor_channel = rlang::UQ(.rfret$metadata$acceptor_channel),
                      donor_channel    = rlang::UQ(.rfret$metadata$donor_channel),
                      concentration    = rlang::UQ(.rfret$metadata$concentration)) %>%
        dplyr::mutate(Replicate = as.integer(Replicate))
}

#' @title Pre-process a single fluorescence polarization or anisotropy dataset
#'
#' @description This internal function pre-processes a single dataframe to
#'     generate the \code{Experiment}, \code{Type}, \code{Replicate}, and
#'     \code{Observation} columns required for subsequent processing by
#'     \code{\link{fp_average_replicates}}.
#' @param raw_data A single dataframe to process.
#' @param experiment_name The name of the corresponding experiment.
#' @return A dataframe containing 8 columns: \code{Experiment}, \code{Type},
#'     \code{Replicate}, \code{Observation}, \code{polarization},
#'     \code{anisotropy}, \code{intensity} and \code{concentration}.
#' @examples
#' \dontrun{
#' fp_format_one_dataset(my_data, "my_experiment")
#' }
#'
#' @importFrom magrittr %>%

fp_format_one_dataset <- function(raw_data, experiment_name) {
    # Generate an Experiment column, and retrieve experiment name here
    raw_data$Experiment <- experiment_name

    # Data formatting pipeline:
    # Generate columns Type and Replicate, based on column Content
    raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = stringr::str_split(rlang::UQ(.rfret$metadata$content),
                                                        pattern = stringr::fixed(".")),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate) %>%
        dplyr::ungroup() %>%
        # Generate an Observation column, to label observations across different
        # types of experiments (baseline and titration) and replicates
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation = rlang::UQ(.rfret$metadata$concentration) %>%
                          dplyr::desc() %>%
                          dplyr::row_number()
        ) %>%
        dplyr::ungroup() %>%
        # Rename sample type identifyers
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$buffer_only)),
                                     "buffer_only")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$baseline)),
                                     "baseline")) %>%
        dplyr::mutate(Type = replace(Type,
                                     Type == rlang::quo_text(rlang::as_quosure(.rfret$metadata$titration)),
                                     "titration")) %>%
        # Reorder columns and return dataset
        dplyr::select(Experiment,
                      Type,
                      Replicate,
                      Observation,
                      polarization  = rlang::UQ(.rfret$metadata$polarization),
                      anisotropy    = rlang::UQ(.rfret$metadata$anisotropy),
                      intensity     = rlang::UQ(.rfret$metadata$intensity),
                      concentration = rlang::UQ(.rfret$metadata$concentration)) %>%
        dplyr::mutate(Replicate = as.integer(Replicate))
}
