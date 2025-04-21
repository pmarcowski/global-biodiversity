# Title: Processing of biodiversity data
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-02-14
# Copyright (c) 2024 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This script performs data preparation of the biodiversity dataset.

# Load packages
library(vroom)
library(data.table)
library(lubridate)
library(countrycode)
library(duckdb)
library(DBI)
library(stringr)

# Define database path
db_path <- "./app/data/biodiversity.duckdb"

# Read occurrence data
occurence_data <- vroom("./data/occurence.csv", col_select = c(
  "id", "continent", "country",
  "scientificName", "vernacularName",
  "eventDate", "longitudeDecimal", "latitudeDecimal"
))

# Convert to data.table and prepare
occurence_data <- as.data.table(occurence_data)
occurence_data <- occurence_data[!is.na(vernacularName)]

# Read media data
media_data <- vroom("./data/multimedia.csv", col_select = c(
  "CoreId", "accessURI", "creator"
))

# Convert to data.table and prepare
media_data <- as.data.table(media_data)
setnames(media_data, "CoreId", "id")

# Perform left join to add accessURI and creator to occurrence data
occurence_data <- merge(occurence_data, media_data, by = "id", all.x = TRUE)

# Create lookup table from occurence_data for scientificName to accessURI and creator
lookup <- occurence_data[!is.na(accessURI),
  .(accessURI = first(accessURI), creator = first(creator)),
  by = scientificName
]

# Fill NAs in accessURI and creator in occurrence data using lookup table
occurence_data[is.na(accessURI), `:=`(
  accessURI = lookup[.SD, on = .(scientificName), accessURI],
  creator = lookup[.SD, on = .(scientificName), creator]
)]

# Inspect data for NAs
na_rows <- occurence_data[is.na(accessURI) | is.na(creator)]
print(na_rows)

# Format vernaculaName as title case
occurence_data[, vernacularName := str_to_title(vernacularName)]

# Remove existing DB file if it exists to ensure fresh import
if (file.exists(db_path)) {
  file.remove(db_path)
}

# Connect to DuckDB database
con <- dbConnect(duckdb(), dbdir = db_path, read_only = FALSE)

# Write the data.table to DuckDB
dbWriteTable(con, "occurrences", occurence_data, overwrite = TRUE)

# Create indexes for faster querying
dbExecute(con, "CREATE INDEX idx_scientificName ON occurrences (scientificName)")
dbExecute(con, "CREATE INDEX idx_vernacularName ON occurrences (vernacularName)")
dbExecute(con, "CREATE INDEX idx_eventDate ON occurrences (eventDate)")

# Disconnect from the database
dbDisconnect(con, shutdown = TRUE)

print(paste("Data successfully written to DuckDB database:", db_path))
