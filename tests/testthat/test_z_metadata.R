context("Metadata handling")

# Load dataset with non-default column names
my_dataset <- readr::read_csv(file = "./metadata/fret_dataset_blah.csv")

# Load expected result from a successful command
my_expected_result <- readr::read_csv(file = "./metadata/expected_output.csv")

# Prepare failing and correct commands
my_failing_command <- rlang::quo(
    my_dataset %>%
        fret_format_data() %>%
        fret_average_replicates() %>%
        fret_correct_signal()
)

my_successful_command <- rlang::quo(
    my_dataset %>%
        fret_format_data(metadata_json = "./metadata/metadata_blah.json") %>%
        fret_average_replicates() %>%
        fret_correct_signal()
)

my_result <- rlang::eval_tidy(my_successful_command)

# Test that an attempt to process the file with default metadata fails
test_that("Pipeline fails without correct metadata", {
    expect_error(object = rlang::eval_tidy(my_failing_command))
})

# Test that an attempt to process the file with correct metadata succeeds
test_that("Pipeline succeeds with correct metadata", {
    expect_silent(object = rlang::eval_tidy(my_successful_command))
    expect_equal(object = my_result, expected = my_expected_result)
})
