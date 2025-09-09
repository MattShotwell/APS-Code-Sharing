# Uncomment to import data from file
# source("R/data.R") # Using RData file instead
# data <- read_data(filepath = "tmp-data/20250731_data.csv")

# Uncomment to load from RData file (loads into global workspace as 'data')
# load(file = "tmp-data/20250731_data.RData")

# Filter for Day 0
# - TODO: Is this needed?
# data_0 <- data[data$event_label == "Day 0",]

# Source variable derivation code
source("R/variables.R")

nb_0 <- derive_neuromuscular_blockade_0(data)



# Cleanup
rm(derive_neuromuscular_blockade_0)
