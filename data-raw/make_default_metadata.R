# Store default metadata for internal use.

library(magrittr)
library(rlang)
library(jsonlite)
library(devtools)

# Read default metadata from JSON file and populate this environment with
# the default key-value pairs (values are symbols).
metadata <- "./data-raw/default_metadata.json" %>%
    jsonlite::read_json() %>%
    rlang::syms()

# Save environment in the internal data store.
devtools::use_data(internal = TRUE,
                   overwrite = TRUE,
                   metadata)

# Clean up
remove(metadata)
