context("FRET - average replicates")

# Load pre-computed expected result
my_expected_result <-
    readr::read_csv(file = "./fret/fret_binding_data_averaged.csv")

# Run example dataset through the formatting and averaging functions
my_result <- fret_binding_data %>%
    fret_format_data() %>%
    fret_average_replicates()

# Test that result matches expected precomputed result
test_that("fret_average_replicates works", {
    expect_equal(object = my_result, expected = my_expected_result)
})
