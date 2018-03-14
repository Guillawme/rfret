context("Curve fitting and equations")

# Test FRET pipeline
# Load pre-computed expected results
load(file = "./fret/fret_expected_fits.rda")

# Run example dataset through the entire pipeline and keep resulting fit objects
fret_hyperbolic_fit <- fret_binding_data %>%
    format_data(data_type = "fret") %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "hyperbolic")

fret_hill_fit <- fret_binding_data %>%
    format_data(data_type = "fret") %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "hill")

fret_quadratic_fit <- fret_binding_data %>%
    format_data(data_type = "fret") %>%
    fret_average_replicates() %>%
    fret_correct_signal() %>%
    fit_binding_model(binding_model = "quadratic", probe_concentration = 5)

# Test that results match expected precomputed results
test_that("fit_binding_model gives correct results on fret_binding_data", {
    expect_equal(object = fret_hyperbolic_fit,
                 expected = fret_expected_hyperbolic_fit)
    expect_equal(object = fret_hill_fit,
                 expected = fret_expected_hill_fit)
    expect_equal(object = fret_quadratic_fit,
                 expected = fret_expected_quadratic_fit)
})

# Test FP/FA pipeline
# Load pre-computed expected results
load(file = "./fp/fp_expected_fits.rda")

# Run example dataset through the entire pipeline and keep resulting fit objects
# Using polarization signal
fp_hyperbolic_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("polarization") %>%
    fit_binding_model(binding_model = "hyperbolic")

fp_hill_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("polarization") %>%
    fit_binding_model(binding_model = "hill")

fp_quadratic_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("polarization") %>%
    fit_binding_model(binding_model = "quadratic", probe_concentration = 3)

# Using anisotropy signal
fa_hyperbolic_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("anisotropy") %>%
    fit_binding_model(binding_model = "hyperbolic")

fa_hill_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("anisotropy") %>%
    fit_binding_model(binding_model = "hill")

fa_quadratic_fit <- fp_binding_data %>%
    format_data(data_type = "fp") %>%
    fp_average_replicates() %>%
    fp_use_signal("anisotropy") %>%
    fit_binding_model(binding_model = "quadratic", probe_concentration = 3)

# Test that results match expected precomputed results
test_that("fit_binding_model gives correct results on fp_binding_data", {
    expect_equal(object = fp_hyperbolic_fit,
                 expected = fp_expected_hyperbolic_fit)
    expect_equal(object = fp_hill_fit,
                 expected = fp_expected_hill_fit)
    expect_equal(object = fp_quadratic_fit,
                 expected = fp_expected_quadratic_fit)
    expect_equal(object = fa_hyperbolic_fit,
                 expected = fa_expected_hyperbolic_fit)
    expect_equal(object = fa_hill_fit,
                 expected = fa_expected_hill_fit)
    expect_equal(object = fa_quadratic_fit,
                 expected = fa_expected_quadratic_fit)
})
