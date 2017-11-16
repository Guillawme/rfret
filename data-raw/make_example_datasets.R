# Store example datasets to be exposed to the user.

library(readr)
library(devtools)

# Read CSV files
fret_binding_data <- readr::read_csv("./data-raw/fret_binding_data.csv")
fp_binding_data   <- readr::read_csv("./data-raw/fp_binding_data.csv")

# Store datasets in the exported data store.
devtools::use_data(internal = FALSE,
                   overwrite = TRUE,
                   fret_binding_data,
                   fp_binding_data)

# Clean up workspace.
remove(fret_binding_data,
       fp_binding_data)
