# Utility functions for the biodiversity observations Shiny app

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

call_openrouter_api <- function(prompt, model_id = NULL, max_output_tokens = NULL) {
  # Get API key from environment variable
  api_key <- Sys.getenv("OPENROUTER_API_KEY")

  # Return early if API key is not set
  if (api_key == "") {
    return(list(error = "OpenRouter API key not configured. Please set the OPENROUTER_API_KEY environment variable."))
  }

  # Use environment variables for parameters
  if (is.null(model_id)) {
    model_id <- Sys.getenv("OPENROUTER_MODEL")
  }

  if (is.null(max_output_tokens)) {
    max_output_tokens <- as.numeric(Sys.getenv("OPENROUTER_TOKEN_LIMIT"))
  }

  # Make API request
  tryCatch({
    response <- request('https://openrouter.ai/api/v1/chat/completions') %>%
      req_headers(
        'Authorization' = paste('Bearer', api_key),
        'Content-Type' = 'application/json'
      ) %>%
      req_body_json(list(
        model = model_id,
        messages = list(list(role = 'user', content = prompt)),
        max_tokens = max_output_tokens
      )) %>%
      req_perform()

    # Parse and return content
    data <- resp_body_json(response)
    list(content = data$choices[[1]]$message$content)
  },
  error = function(e) {
    list(error = paste('API call failed:', e$message))
  })
}

search_species <- function(search_query, db_con) {
  # Searches for distinct species in the database based on a search query.
  # It looks for matches in both the vernacular and scientific names using SQL LIKE.
  # The search query is preprocessed using the preprocess_query function.
  # Returns a limited number of matches to avoid overwhelming the UI.
  #
  # Args:
  #   search_query: A character string containing the search query.
  #   db_con: A DBI connection object to the DuckDB database.
  #
  # Returns:
  #   A data frame with distinct 'scientificName' and 'vernacularName' that match
  #   the query. Returns an empty data frame if the
  #   search query is empty after preprocessing.

  processed_query <- preprocess_query(search_query)

  if (processed_query == "") {
    # Return an empty data frame with the expected columns
    return(data.frame(scientificName = character(0), vernacularName = character(0)))
  }

  # Use SQL LIKE for pattern matching, case-insensitive via lower()
  # Add wildcards (%) around the query term
  like_pattern <- paste0("%", processed_query, "%")

  # Construct the SQL query using DBI::sqlInterpolate for safety
  query <- DBI::sqlInterpolate(
    db_con,
    "SELECT DISTINCT scientificName, vernacularName
     FROM occurrences
     WHERE lower(vernacularName) LIKE ? OR lower(scientificName) LIKE ?",
    like_pattern,
    like_pattern
  )

  # Execute the query
  result <- DBI::dbGetQuery(db_con, query)

  # Ensure the result is a data frame (dbGetQuery should return one)
  if (!is.data.frame(result)) {
     warning("Database query did not return a data frame in search_species.")
     return(data.frame(scientificName = character(0), vernacularName = character(0)))
  }

  return(result)
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
leaflet(options = leafletOptions(
      worldCopyJump = FALSE, # Prevent map repeating horizontally
      maxBounds = list(list(-90, -180), list(90, 180)) # Limit panning
    )) %>%
    addProviderTiles(
      providers$OpenStreetMap.Mapnik,
      options = providerTileOptions(minZoom = 4)
    ) %>%
    addCircleMarkers(
      data = map_data,
      lng = ~longitudeDecimal, lat = ~latitudeDecimal,
      popup = ~ paste0(
        ifelse(!is.na(accessURI), paste("<img src='", accessURI, "' height='300'><br><br>"), "Image Unavailable<br><br>"),
        "<div style='white-space: nowrap;'>",
        "<strong>Vernacular Name: </strong>", vernacularName, "<br>",
        "<strong>Scientific Name: </strong><em>", scientificName, "</em><br>",
        "<strong>Date: </strong>", ifelse(!is.na(eventDate), as.character(eventDate), "Date Unavailable"), "<br>",
        "<strong>Observer: </strong>", ifelse(!is.na(creator), creator, "Observer Unavailable"),
        "</div>"
      ),
      radius = 8, stroke = FALSE, color = color,
      fillOpacity = 0.8,
      clusterOptions = markerClusterOptions()
    ) %>%
    setView(lng = initial_view$lng, lat = initial_view$lat, zoom = initial_view$zoom)
}

render_timeline <- function(timeline_data, color, secondary_color = NULL) {
  # Creates a Plotly bar chart to visualize the timeline of species occurrences.
  #
  # Args:
  #   timeline_data: A data.table containing the timeline data.
  #   color: The color for the bars.
  #   secondary_color: The color for the trend line. If NULL, no trend line is shown.
  #
  # Returns:
  #   A Plotly bar chart object.
  # Create initial plot with bars
  p <- plot_ly(data = timeline_data) %>%
    add_bars(
      x = ~year,
      y = ~N,
      text = ~N,
      textposition = "outside",
      marker = list(color = color),
      hoverinfo = "none",
      opacity = 0.8
    )

  # Add trend line if we have enough data points
  if (nrow(timeline_data) >= 5) {
    # Create a linear model for the trend line
    model <- lm(N ~ as.numeric(year), data = timeline_data)
    # Create predicted values
    predicted <- predict(model, timeline_data)
    predicted[predicted < 0] <- 0

    # Add the trend line
    p <- p %>% add_lines(
      x = ~year,
      y = ~predicted,
      line = list(color = secondary_color, width = 2),
      hoverinfo = "none"
    )
  }

  # Complete plot with layout and config
  p %>% layout(
      xaxis = list(title = "Year"),
      yaxis = list(title = "Count"),
      showlegend = FALSE,
      dragmode = FALSE,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      modebar = list(bgcolor = "transparent", color = "#666666", activecolor = color)
    ) %>%
    config(
      # Enable display mode bar with only specific buttons
      displayModeBar = TRUE,
      # Only show the download button
      modeBarButtons = list(list("toImage")),
      # Custom appearance
      displaylogo = FALSE,
      # Customize the download options
      toImageButtonOptions = list(
        format = 'png',
        filename = 'biodiversity_timeline',
        height = 500,
        width = 700
      )
    )
}
