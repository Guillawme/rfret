context("Curve fitting and equations")

# Load pre-computed expected results
load(file = "./fret/expected_fits.rda")

# Run example dataset through the entire pipeline and keep resulting fit objects
my_hyperbolic_fit <- fret_binding_data %>%
    fret_format_data() %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "hyperbolic")

my_hill_fit <- fret_binding_data %>%
    fret_format_data() %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "hill")

my_quadratic_fit <- fret_binding_data %>%
    fret_format_data() %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "quadratic", probe_concentration = 5)

# Test that results matches expected precomputed results
test_that("fit_binding_model gives correct results on fret_binding_data", {
    expect_equal(object = my_hyperbolic_fit,
                 expected = my_expected_hyperbolic_fit)
    expect_equal(object = my_hill_fit,
                 expected = my_expected_hill_fit)
    expect_equal(object = my_quadratic_fit,
                 expected = my_expected_quadratic_fit)
})
