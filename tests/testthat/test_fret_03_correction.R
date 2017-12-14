context("FRET - correct signal")

# Load pre-computed expected results
my_expected_result_1 <-
    readr::read_csv(file = "./fret/fret_binding_data_corrected.csv")

my_expected_result_2 <-
    readr::read_csv(file = "./fret/fret_less_donor_controls_corrected.csv")

my_expected_result_3 <-
    readr::read_csv("./fret/fret_less_acceptor_controls_corrected.csv")

# Run example datasets and test files through the formatting, averaging and
# correction functions
my_result_1 <- fret_binding_data %>%
    fret_format_data() %>%
    fret_average_replicates() %>%
    fret_correct_signal()

my_result_2_command <- rlang::quo(
    c(fret_less_donor = "./fret/fret_less_donor_controls.csv") %>%
    fret_format_data() %>%
    fret_average_replicates() %>%
    fret_correct_signal()
)

my_result_2 <-  rlang::eval_tidy(my_result_2_command)

my_result_3_command <- rlang::quo(
    c(fret_less_donor = "./fret/fret_less_acceptor_controls.csv") %>%
        fret_format_data() %>%
        fret_average_replicates() %>%
        fret_correct_signal()
)

my_result_3 <-  rlang::eval_tidy(my_result_3_command)

# Test that results match expected precomputed results
test_that("fret_correct_signal works", {
    expect_message(object = rlang::eval_tidy(my_result_2_command), regexp = NULL)
    expect_message(object = rlang::eval_tidy(my_result_3_command), regexp = NULL)
    expect_equal(object = my_result_1, expected = my_expected_result_1)
    expect_equal(object = my_result_2, expected = my_expected_result_2)
    expect_equal(object = my_result_3, expected = my_expected_result_3)
})
