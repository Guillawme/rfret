context("Inspect raw data")

detect_saturated <- inspect_raw_data(fret_saturated_reads, NULL, 260000)

# Test that saturated reads are properly detected
test_that("inspect_raw_data detects saturated fluorescence counts", {
    expect_true(object = detect_saturated$saturated_reads)
    expect_warning(object = inspect_raw_data(fret_saturated_reads, NULL, 260000))
})
