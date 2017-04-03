context("Batch process")

# Load pre-computed expected results in a named list
# (each list item is named after the original dataset, and contains a dataframe
# that is a pre-computed output of average_technical_replicates)
my_files <- list.files(path = "tests/testthat/correct",
                       pattern = ".csv",
                       full.names = TRUE)
my_filenames <- list.files(path = "tests/testthat/correct",
                           pattern = ".csv",
                           full.names = FALSE)
my_filenames <- sub(my_filenames,
                    pattern = "_exp_corr.csv",
                    replacement = "")
my_expected_results <- lapply(my_files, read.csv)
names(my_expected_results) <- my_filenames

# Run all example datasets from the package through the function
# First, build a list of all datasets
my_datasets <- list.files(path = "data",
                          pattern = ".rda",
                          full.names = TRUE)
my_datasets_names <- list.files(path = "data",
                                pattern = ".rda",
                                full.names = FALSE)
my_datasets_names <- sub(my_datasets_names,
                         pattern = ".rda",
                         replacement = "")
my_loaded_datasets <- lapply(my_datasets, load)
my_loaded_datasets <- lapply(my_loaded_datasets, get)
names(my_loaded_datasets) <- my_datasets_names

# The function will need that to work properly
ttn <- c("titration_1", "titration_2")
blk <- c("blank_1", "blank_2")

my_results <- lapply(my_loaded_datasets, batch_process, ttn, blk)

# Test whether results are identical to pre-computed expected results
test_that("batch_process gives correct results", {
    expect_identical(object = my_results, expected = my_expected_results)
})
