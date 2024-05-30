# Title: Utility functions for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This script contains utility functions used in the biodiversity
# observations Shiny app.

search_species <- function(search_query, dt) {
  # Searches for species in the occurrence data table based on a search query.
  # It looks for matches in both the vernacular and scientific names of the species.
  # The function is case-insensitive and handles leading/trailing whitespaces.
  # If the search query is empty or consists only of whitespaces, an empty data.table 
  # is returned.
  #
  # Args:
  #   search_query: A character string containing the search query.
  #   dt: A data.table containing the occurrence data.
  #
  # Returns:
  #   A data.table filtered to include only records that match the search query.
  #   If the search query is empty or consists only of whitespaces, an empty 
  # data.table is returned.
  search_query <- trimws(search_query)
  
  if (search_query == "") {
    return(dt[0])
  }
  
  dt[grepl(search_query, vernacularName, ignore.case = TRUE) |
       grepl(search_query, scientificName, ignore.case = TRUE)]
}

filter_species <- function(dt, species) {
  # Filters the occurrence data table to include only records for the specified species.
  #
  # Args:
  #   dt: A data.table containing the occurrence data.
  #   species: A character string specifying the scientific name of the species.
  #
  # Returns:
  #   A data.table filtered to include only records for the specified species.
  dt[scientificName == species]
}

count_by_year <- function(dt) {
  # Counts the number of occurrences per year in the occurrence data table.
  #
  # Args:
  #   dt: A data.table containing the occurrence data.
  #
  # Returns:
  #   A data.table with the count of occurrences for each year.
  dt[, .N, by = .(year = year(eventDate))]
}

render_map <- function(map_data, color_palette) {
  # Creates a leaflet map to visualize species occurrences.
  #
  # Args:
  #   map_data: A data.table containing the map data.
  #   color_palette: A color palette function for species coloring.
  #
  # Returns:
  #   A leaflet map object.
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      data = map_data,
      lng = ~longitudeDecimal, lat = ~latitudeDecimal,
      popup = ~paste0(
        ifelse(!is.na(accessURI), paste("<img src='", accessURI, "' height='300'><br>"), "No image available<br>"),
        "<i>", scientificName, "</i><br>", tolower(vernacularName),
        "<br>", eventDate,
        ifelse(!is.na(creator), paste0("<br>Credit: ", creator), "")
      ),
      radius = 5, stroke = FALSE, color = ~color_palette(scientificName),
      fillOpacity = 0.5
    ) %>%
    setView(lng = 19.1344, lat = 51.9194, zoom = 6)
}

render_timeline <- function(timeline_data, color) {
  # Creates a Plotly bar chart to visualize the timeline of species occurrences.
  #
  # Args:
  #   timeline_data: A data.table containing the timeline data.
  #   color: A character string specifying the color for the bars.
  #
  # Returns:
  #   A Plotly bar chart object.
  plot_ly(
    data = timeline_data,
    x = ~year,
    y = ~N,
    type = "bar",
    text = ~N,
    textposition = "outside",
    marker = list(color = color),
    hoverinfo = "none",
    opacity = 0.7
  ) %>%
    layout(
      xaxis = list(title = "Year", dtick = 1),
      yaxis = list(title = "Count"),
      showlegend = FALSE,
      dragmode = FALSE,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent"
    ) %>%
    config(displayModeBar = FALSE)
}
