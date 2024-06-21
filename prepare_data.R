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
library(fst)

# Define countries to include
selected_countries <- c(
  "Poland", "Germany", "France", "United Kingdom", "Italy",
  "Spain", "Netherlands", "Belgium", "Sweden", "Switzerland", "Austria"
)

# Define years to include
selected_years <- c(2010, 2020)

# Read occurrence data
occurence_data <- vroom("./data/occurence.csv", col_select = c(
  "id", "continent", "country",
  "scientificName", "vernacularName",
  "eventDate", "longitudeDecimal", "latitudeDecimal"
))

# Convert to data.table and prepare
occurence_data <- as.data.table(occurence_data)
occurence_data <- occurence_data[country %chin% selected_countries]
occurence_data <- occurence_data[year(eventDate) %between% selected_years]
occurence_data <- occurence_data[!is.na(vernacularName)]

# Downsample observations by country
occurence_data <- occurence_data[, .SD[sample(.N, max(1, .N * 0.2))], by = country]

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

# Save prepared occurrence data to app directory
write_fst(occurence_data, "./app/data/occurence_prepared.fst")
