# Title: Utility functions for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This script contains utility functions used in the biodiversity
# observations Shiny app.

preprocess_query <- function(query) {
  # Preprocesses species name query by removing diacritics, converting to
  # lowercase, and removing any non-alphanumeric characters, such as special
  # characters, punctuation marks, and whitespaces.
  #
  # Args:
  #   query: A character string or vector of character strings to be preprocessed.
  #
  # Returns:
  #   The preprocessed query with diacritics removed, converted to lowercase,
  #   and non-alphanumeric characters removed.
  query <- iconv(query, to = "ASCII//TRANSLIT")
  query <- tolower(query)
  query <- gsub("[^[:alnum:]]", "", query)
  query
}

search_species <- function(search_query, dt) {
  # Searches for species in the occurrence data table based on a search query.
  # It looks for matches in both the vernacular and scientific names of the
  # species. The function is case-insensitive. If the search query is empty,
  # an empty data.table is returned. The search query is preprocessed using
  # the preprocess_query function to remove non-alphanumeric characters.
  #
  # Args:
  #   search_query: A character string containing the search query.
  #   dt: A data.table containing the occurrence data.
  #
  # Returns:
  #   A data.table filtered to include only records that match the search query.
  #   If the search query is empty or consists only of whitespaces, an empty
  #   data.table is returned.
  search_query <- preprocess_query(search_query)

  if (search_query == "") {
    return(dt[0])
  }

  dt[grepl(search_query, vernacularName, ignore.case = TRUE) |
    grepl(search_query, scientificName, ignore.case = TRUE)]
}

filter_species <- function(dt, species) {
  # Filters the occurrence data table to include only records for the specified
  # species. It uses data.table's binary search feature for faster filtering.
  #
  # Args:
  #   dt: A data.table containing the occurrence data.
  #   species: A character string specifying the scientific name of the species.
  #
  # Returns:
  #   A data.table filtered to include only records for the specified species.
  dt[scientificName == species, on = "scientificName"]
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

render_map <- function(map_data, color, initial_view) {
  # Creates a leaflet map to visualize species occurrences.
  #
  # Args:
  #   map_data: A data.table containing the map data.
  #   color: A color for species coloring.
  #
  # Returns:
  #   A leaflet map object.
  leaflet() %>%
    addProviderTiles(
      providers$CartoDB.Positron,
      options = providerTileOptions(minZoom = 4)
    ) %>%
    addCircleMarkers(
      data = map_data,
      lng = ~longitudeDecimal, lat = ~latitudeDecimal,
      popup = ~ paste0(
        ifelse(!is.na(accessURI), paste("<img src='", accessURI, "' height='300'><br><br>"), "No image available<br><br>"),
        "<div style='white-space: nowrap;'>",
        "<strong>Scientific name: </strong><em>", scientificName, "</em><br>",
        "<strong>Vernacular name: </strong>", tolower(vernacularName), "<br>",
        "<strong>Date: </strong>", ifelse(!is.na(eventDate), as.character(eventDate), "No date available"), "<br>",
        "<strong>Observer: </strong>", ifelse(!is.na(creator), creator, "No observer available"),
        "</div>"
      ),
      radius = 10, stroke = FALSE, color = color,
      fillOpacity = 0.7,
      clusterOptions = markerClusterOptions()
    ) %>%
    setView(lng = initial_view$lng, lat = initial_view$lat, zoom = initial_view$zoom)
}

render_timeline <- function(timeline_data, color) {
  # Creates a Plotly bar chart to visualize the timeline of species occurrences.
  #
  # Args:
  #   timeline_data: A data.table containing the timeline data.
  #   color: The color for the bars.
  #
  # Returns:
  #   A Plotly bar chart object.
  plot_ly(data = timeline_data) %>%
    add_bars(
      x = ~year,
      y = ~N,
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
