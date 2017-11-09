context("Metadata handling")

# Load dataset with non-default column names
my_dataset <- readr::read_csv(file = "./metadata/fret_dataset_blah.csv")

# Test that an attempt to process the file with default metadata fails
expect_error(object = {
    my_dataset %>%
            fret_format_data() %>%
            fret_average_replicates() %>%
            fret_correct_signal()
})
