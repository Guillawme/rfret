context("FP: format data")

# Load pre-computed expected result
my_expected_result <-
    readr::read_csv(file = "./fp/fp_binding_data_formatted.csv")

# Run example dataset through the formatting function
my_result <- fp_binding_data %>%
    fp_format_data()

# Test that result matches expected precomputed result
test_that("fp_format_data works", {
    expect_equal(object = my_result, expected = my_expected_result)
})
