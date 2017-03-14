#' @title Average technical replicates
#'
#' @description This function calculates averages of fluorescence values from
#'     an arbitrary number of technical replicates.
#'
#' @param raw_data A dataframe containing the raw fluorescence data. It must
#'     contain at least five columns named \code{Content}, \code{donor_channel},
#'     \code{acceptor_channel}, \code{fret_channel} and \code{concentration}.
#'     The \code{Content} column must contain, for each row, a word describing
#'     which data series or replicate this row belongs to (like
#'     \code{blank_1}, \code{titration_1}, etc.).
#' @param blanks A character vector containing words that identify the blank
#'     experiment series (e.g. \code{blank}). These words must match the ones
#'     in \code{raw_data$Content} (case sensitive).
#' @param titrations A character vector containing words that identify the
#'     titration series (e.g. \code{titration}). These words must match the
#'     ones in \code{raw_data$Content} (case sensitive).
#' @return A dataframe containing the reduced dataset. It contains five columns
#'     named \code{Content}, \code{donor_channel}, \code{acceptor_channel},
#'     \code{fret_channel} and \code{concentration}. The \code{Content} column
#'     contains only two words: \code{"blank"} and \code{"titration"}.
#' @export

average_technical_replicates <- function(raw_data, blanks, titrations){
    # Get concentration column from initial data
    new_concentration <- raw_data$concentration[1:24]

    # Generate a new Content column with only 'blank' and 'titration'
    new_content <- c(rep("blank", 24), rep("titration", 24))

    # Rearrange replicates as columns of dataframes
    donor_blanks_df <- data.frame(raw_data$donor_channel[raw_data$Content == blanks[1]],
                                  raw_data$donor_channel[raw_data$Content == blanks[2]])
    donor_titrations_df <- data.frame(raw_data$donor_channel[raw_data$Content == titrations[1]],
                                      raw_data$donor_channel[raw_data$Content == titrations[2]])

    acceptor_blanks_df <- data.frame(raw_data$acceptor_channel[raw_data$Content == blanks[1]],
                                     raw_data$acceptor_channel[raw_data$Content == blanks[2]])
    acceptor_titrations_df <- data.frame(raw_data$acceptor_channel[raw_data$Content == titrations[1]],
                                         raw_data$acceptor_channel[raw_data$Content == titrations[2]])

    fret_blanks_df <- data.frame(raw_data$fret_channel[raw_data$Content == blanks[1]],
                                 raw_data$fret_channel[raw_data$Content == blanks[2]])
    fret_titrations_df <- data.frame(raw_data$fret_channel[raw_data$Content == titrations[1]],
                                     raw_data$fret_channel[raw_data$Content == titrations[2]])

    # Compute the row-by-row means of replicates and rearrange into vectors
    # with blank followed by titration
    donor_blank_av <- rowMeans(donor_blanks_df)
    donor_titration_av <- rowMeans(donor_titrations_df)
    donor <- c(donor_blank_av, donor_titration_av)

    acceptor_blank_av <- rowMeans(acceptor_blanks_df)
    acceptor_titration_av <- rowMeans(acceptor_titrations_df)
    acceptor <- c(acceptor_blank_av, acceptor_titration_av)

    fret_blank_av <- rowMeans(fret_blanks_df)
    fret_titration_av <- rowMeans(fret_titrations_df)
    fret <- c(fret_blank_av, fret_titration_av)

    # Build a final dataset with average values instead of replicates, and
    # with the same format as the raw data
    reduced_dataset <- data.frame(Content          = new_content,
                                  donor_channel    = donor,
                                  acceptor_channel = acceptor,
                                  fret_channel     = fret,
                                  concentration    = new_concentration)
}
