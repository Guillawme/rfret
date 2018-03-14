context("Pre-processing")

# Load pre-computed expected results
my_expected_result_fret <-
    readr::read_csv(file = "./fret/fret_binding_data_formatted.csv")
my_expected_result_fp <-
    readr::read_csv(file = "./fp/fp_binding_data_formatted.csv")

# Run example datasets through the formatting function
my_result_fret <- fret_binding_data %>%
    format_data(data_type = "fret")
my_result_fp <- fp_binding_data %>%
    format_data(data_type = "fp")

# Generate a failing command
my_failing_command <- rlang::quo(
    fret_binding_data %>%
        format_data(data_type = "unknown")
)

# Test that results match expected precomputed results
test_that("format_data works", {
    expect_equal(object = my_result_fret, expected = my_expected_result_fret)
    expect_equal(object = my_result_fp, expected = my_expected_result_fp)
})

# Test that an attempt to pre-process a data set with an unknown data type fails
test_that("format_data detects unknown data type", {
    expect_error(object = rlang::eval_tidy(my_failing_command))
})
