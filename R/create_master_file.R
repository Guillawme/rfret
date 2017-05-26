# This file, create_master_file.R, contains 2 functions:
# 1) makeMasterFile: this is the main function.
# 2) cleanData: this is an internal function that makeMasterFile calls.

#' @title Clean data from single csv file
#'
#' @description This is an internal function that processes a single csv file and generates the
#'      \code{Experiment}, \code{Type}, \code{Replicate}, and \code{Observation} columns.  One data frame is
#'      returned with the rows of data generated from a single csv file.
#' @param fileName Name of single csv file to process.
#' @param numberOfLinesToSkip Number of lines to skip when reading the csv file, usually containing header information.
#' @return One dataframe is returned, containing 11 columns with the input data from a single file: \code{Experiment},
#' \code{Type}, \code{Replicate}, \code{Observation}, \code{Well Row}, \code{Well Col}, \code{Content},
#' \code{fret_channel}, \code{donor_channel}, \code{acceptor_channel}, \code{concentration}.
#'
#' @examples
#' \dontrun{
#'
#'  cleanData(file, 4)
#'
#'  }
#'

cleanData <- function(fileName, numberOfLinesToSkip){
    raw_data = readr::read_csv(fileName, skip = numberOfLinesToSkip)
    raw_data$Experiment = strsplit(fileName, "[.]")[[1]][1]

    raw_data =
        raw_data %>%
        dplyr::rowwise() %>%
        dplyr::mutate(intermediate = strsplit(Content, "_"),
                      Type = intermediate[1],
                      Replicate = intermediate[2]) %>%
        dplyr::select(-intermediate)

    raw_data = raw_data %>%
        dplyr::group_by(Experiment, Type, Replicate) %>%
        dplyr::mutate(Observation = row_number())

    raw_data <- raw_data[c("Experiment", "Type", "Replicate", "Observation", "Well Row", "Well Col", "Content",
                           "fret_channel", "donor_channel", "acceptor_channel", "concentration")]
    return(raw_data)
}


#' @title Create master data frame from input csv file(s)
#'
#' @description This function creates one 'master' data frame by combining all the input csv files. The
#'      two input variables are 1) list of csv files to process and 2) number of lines in the csv file to skip.
#'      For each input file, this function creates the \code{Experiment} column based on the \code{file_name}
#'      for each file_name.csv provided as input.  The \code{Type} and \code{Replicate} are extrapolated
#'      from the \code{Content} column in the original data.  \code{Type} is either 'blank' or 'titration'.
#'      \code{Replicate} is the number of technical replicate.  For example, \code{Type} = 'blank' and
#'      \code{Replicate} = '1' if \code{Content} = "blank_1".  The data is then grouped by \code{Experiment},
#'      \code{Type}, and \code{Replicate} to determine the \code{Observation} number.  \code{Observation}
#'      number should match the \code{Well Col} number. \code{dplyr::full_join} is used to combine the rows
#'      from each input file.
#' @param listOfInputFiles A list of input csv files to be processed.
#' @param numberOfLinesToSkip The number of lines to skip in the csv file, usually containing header information.
#' @return One dataframe is returned, containing 11 columns with the combined input data: \code{Experiment},
#' \code{Type}, \code{Replicate}, \code{Observation}, \code{Well Row}, \code{Well Col}, \code{Content},
#' \code{fret_channel}, \code{donor_channel}, \code{acceptor_channel}, \code{concentration}.
#' @examples
#' \dontrun{
#'
#' makeMasterFile(files, 4)
#'
#' }
#' @export

makeMasterFile <- function(listOfInputFiles, numberOfLinesToSkip){
    # Start building the master file
    masterFile <- cleanData(listOfInputFiles[1], numberOfLinesToSkip)

    # Handle the case where only one data file is provided
    if (length(listOfInputFiles) == 1) {
        return(masterFile)
    }

    # Keep concatenating data files if there is more than one
    for (i in 2:length(listOfInputFiles)) {
        nextData <- cleanData(listOfInputFiles[i], numberOfLinesToSkip)
        masterFile <- dplyr::full_join(masterFile, nextData)
    }

    return(masterFile)
}
