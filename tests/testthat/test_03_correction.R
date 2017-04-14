context("Correct FRET signal")

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

# Run all expected average datasets through the function
my_datasets <- list.files(path = "tests/testthat/average",
                          pattern = ".csv",
                          full.names = TRUE)
my_datasets_names <- list.files(path = "tests/testthat/average",
                                pattern = ".csv",
                                full.names = FALSE)
my_datasets_names <- sub(my_datasets_names,
                         pattern = "_exp_av.csv",
                         replacement = "")
my_loaded_datasets <- lapply(my_datasets, read.csv)
names(my_loaded_datasets) <- my_datasets_names

my_results <- lapply(my_loaded_datasets, correct_fret_signal)

# Test whether results are identical to pre-computed expected results
test_that("correct_fret_signal gives correct results", {
    expect_identical(object = my_results, expected = my_expected_results)
})
