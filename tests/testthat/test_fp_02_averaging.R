context("FP - average replicates")

# Load pre-computed expected result
my_expected_result <-
    readr::read_csv(file = "./fp/fp_binding_data_averaged.csv")

# Run example dataset through the formatting and averaging functions
my_result <- fp_binding_data %>%
    fp_format_data() %>%
    fp_average_replicates()

# Test that result matches expected precomputed result
test_that("fp_average_replicates works", {
    expect_equal(object = my_result, expected = my_expected_result)
})
