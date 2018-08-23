# A unit test to enforce coding style

if (requireNamespace("lintr", quietly = TRUE)) {
    context("Coding style")
    test_that("Lints", {
        lintr::expect_lint_free()
    })
}
