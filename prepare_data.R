# Data preparation for the GBIF biodiversity observations dataset

# Load packages
library(vroom)
library(data.table)
library(lubridate)
library(stringr)
library(countrycode)
library(arrow)

# Read occurrence data
occurrence_data <- vroom("./data/occurence.csv", col_select = c(
  "id", "continent", "country",
  "scientificName", "vernacularName",
  "eventDate", "longitudeDecimal", "latitudeDecimal"
))

# Convert to data.table and prepare
occurrence_data <- setDT(occurrence_data)[
  # Drop rows with missing vernacularName or eventDate
  !is.na(vernacularName) & !is.na(eventDate)
][
  # Clean columns and compute year
  , `:=`(
    vernacularName = str_to_title(vernacularName),
    eventDate = as.Date(eventDate),
    year = year(eventDate),
    longitudeDecimal = as.numeric(longitudeDecimal),
    latitudeDecimal = as.numeric(latitudeDecimal)
  )
][
  # Fill continent from country
  is.na(continent),
  continent := countrycode(
    country,
    origin = "country.name.en",
    destination = "continent"
  )
][
  # Map region from country
  , region := countrycode(
    country,
    origin = "country.name.en",
    destination = "region"
  )
]

setkey(occurrence_data, id)

# Read media data
media_data <- vroom("./data/multimedia.csv", col_select = c(
  "CoreId", "accessURI", "creator"
))

# Convert to data.table and prepare
media_data <- setDT(media_data)
setnames(media_data, "CoreId", "id")
setkey(media_data, id)

# Perform keyed join to add accessURI and creator to occurrence data
occurrence_data <- media_data[occurrence_data]

# Verify and reorder columns
final_cols <- c(
  "id",
  "continent",
  "region",
  "country",
  "scientificName",
  "vernacularName",
  "eventDate",
  "year",
  "longitudeDecimal",
  "latitudeDecimal",
  "accessURI",
  "creator"
)

occurrence_data <- occurrence_data[, ..final_cols]

# Optional: create lookup table for scientificName to accessURI and creator
# lookup_occurence <- occurrence_data[!is.na(accessURI),
#                                    .(accessURI = first(accessURI), creator = first(creator)),
#                                    by = scientificName
# ]

# Fill NAs in accessURI and creator in occurrence data using lookup table
# occurrence_data[is.na(accessURI), `:=`(
#   accessURI = lookup_occurence[.SD, on = .(scientificName), accessURI],
#   creator   = lookup_occurence[.SD, on = .(scientificName), creator]
# )]

# Inspect data for NAs
na_summary <- colSums(is.na(occurrence_data))
print(na_summary)

# Optional: downsample data
# set.seed(42)
# occurrence_data <- occurrence_data[sample(.N, 50000)]

# Write prepared data Parquet
parquet_path <- "./data/observations.parquet"

if (file.exists(parquet_path)) {
  file.remove(parquet_path)
}

write_parquet(
  occurrence_data,
  parquet_path,
  version = "2.6",
  compression = "snappy"
)

print(paste("Prepared data successfully written to Parquet:", parquet_path))

# Extract unique species data for lookup
species_lookup <- unique(occurrence_data[, .(scientificName, vernacularName)])

# Define species lookup file path
species_lookup_path <- "./app/data/species_lookup.rds"

# Remove existing lookup file if it exists
if (file.exists(species_lookup_path)) {
  file.remove(species_lookup_path)
}

# Write the species lookup data to RDS
saveRDS(species_lookup, species_lookup_path)
print(paste("Species lookup data successfully written to:", species_lookup_path))

# Create default map sample
# Total to sample
total_sample <- min(50000, nrow(occurrence_data))

# One random row per species
species_idx <- occurrence_data[, .(idx = .I[sample(.N, min(10, .N))]), by = scientificName]$idx

# Sample remainder
extra_idx <- setdiff(seq_len(nrow(occurrence_data)), species_idx)
extra_idx <- sample(extra_idx, max(0, total_sample - length(species_idx)))

# Combine and fetch columns
default_map <- occurrence_data[c(species_idx, extra_idx), ..final_cols]

# Define default map data file path
default_map_path <- "./app/data/default_map.rds"

# Remove existing default map data file if it exists
if (file.exists(default_map_path)) {
  file.remove(default_map_path)
}

# Write the default map data to RDS
saveRDS(default_map, default_map_path)
print(paste("Default map data successfully written to:", default_map_path))

# Create default timeline data
default_timeline <- occurrence_data[!is.na(eventDate), .(N = as.integer(.N)), by = year]

# Define default timeline data file path
default_timeline_path <- "./app/data/default_timeline.rds"

# Remove existing default map data file if it exists
if (file.exists(default_timeline_path)) {
  file.remove(default_timeline_path)
}

# Write the default map data to RDS
saveRDS(default_timeline, default_timeline_path)
print(paste("Default timeline data successfully written to:", default_timeline_path))
