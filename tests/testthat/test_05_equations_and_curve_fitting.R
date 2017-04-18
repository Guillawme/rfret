context("Equations and curve fitting")

# Load a known dataset
my_data <- read.csv("./correct/fret_good_exp_corr.csv")

# Guess parameters
my_params <- guess_parameters(my_data)

# Fit using hyperbola and quadratic equations
fit_hyperbola <- fit_binding_model(my_data,
                                   hyperbola,
                                   my_params)
fit_quadratic <- fit_binding_model(my_data,
                                   quadratic,
                                   my_params,
                                   10)

# Get result tables for easy access to numerical values of kd, fret_min
# and fret_max
results_hyperbola <- broom::tidy(fit_hyperbola$fit)
results_quadratic <- broom::tidy(fit_quadratic$fit)

# Test whether computed results are equal to expected values
# Using hyperbola equation
test_that("hyperbola equation and fit_binding_model give correct kd", {
    expect_equal(results_hyperbola$estimate[results_hyperbola$term == "kd"],
                 expected = 54.17532,
                 tolerance = 1e-05)
    expect_equal(results_hyperbola$std.error[results_hyperbola$term == "kd"],
                 expected = 6.29493,
                 tolerance = 1e-05)
})
test_that("hyperbola equation and fit_binding_model give correct fret_min", {
    expect_equal(results_hyperbola$estimate[results_hyperbola$term == "fret_min"],
                 expected = 109.12488,
                 tolerance = 1e-05)
    expect_equal(results_hyperbola$std.error[results_hyperbola$term == "fret_min"],
                 expected = 45.43089,
                 tolerance = 1e-05)
})
test_that("hyperbola equation and fit_binding_model give correct fret_max", {
    expect_equal(results_hyperbola$estimate[results_hyperbola$term == "fret_max"],
                 expected = 4478.20241,
                 tolerance = 1e-05)
    expect_equal(results_hyperbola$std.error[results_hyperbola$term == "fret_max"],
                 expected = 126.99378,
                 tolerance = 1e-05)
})

# Test whether computed results are equal to expected values
# Using quadratic equation
test_that("quadratic equation and fit_binding_model give correct kd", {
    expect_equal(results_quadratic$estimate[results_quadratic$term == "kd"],
                 expected = 48.41484,
                 tolerance = 1e-05)
    expect_equal(results_quadratic$std.error[results_quadratic$term == "kd"],
                 expected = 6.294323,
                 tolerance = 1e-05)
})
test_that("quadratic equation and fit_binding_model give correct fret_min", {
    expect_equal(results_quadratic$estimate[results_quadratic$term == "fret_min"],
                 expected = 116.84492,
                 tolerance = 1e-05)
    expect_equal(results_quadratic$std.error[results_quadratic$term == "fret_min"],
                 expected = 46.089283,
                 tolerance = 1e-05)
})
test_that("quadratic equation and fit_binding_model give correct fret_max", {
    expect_equal(results_quadratic$estimate[results_quadratic$term == "fret_max"],
                 expected = 4437.96239,
                 tolerance = 1e-05)
    expect_equal(results_quadratic$std.error[results_quadratic$term == "fret_max"],
                 expected = 130.070099,
                 tolerance = 1e-05)
})
