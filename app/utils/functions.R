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

search_species <- function(search_query, lookup_data) {
  # Searches for distinct species using the in-memory lookup table.
  # It looks for matches in both the vernacular and scientific names.
  # The search query is preprocessed using the preprocess_query function.
  #
  # Args:
  #   search_query: A character string containing the search query.
  #
  # Returns:
  #   A data frame with 'scientificName' and 'vernacularName' that match
  #   the query. Returns an empty data frame if the search query is empty after preprocessing.
  processed_query <- preprocess_query(search_query)

  if (processed_query == "") {
    # Return an empty data frame with the expected columns
    return(data.frame(scientificName = character(0), vernacularName = character(0)))
  }

  # Create pattern for case-insensitive matching
  pattern <- paste0(".*", processed_query, ".*")

  # Use data.table filtering for efficiency
  result <- lookup_data[
    grepl(pattern, tolower(vernacularName)) |
      grepl(pattern, tolower(scientificName))
  ]

  return(result)
}

get_map_data <- function(species, table, cols, GCP_PROJECT) {
# Constructs and executes a SQL query against the specified
# BigQuery table to fetch occurrence data (selected columns) for a
# particular species.
#
# Args:
#   species: Character string. The scientific name of the species to query.
#   table: Character string. The fully-qualified BigQuery table name.
#   cols: Character vector. A list of column names to select from the table.
#   GCP_PROJECT: Character string. The Google Cloud Project ID.
#
# Returns:
#   A data.table (result of bq_table_download) containing the map data for the
#   specified species, including the columns requested.
  query <- sprintf(
    "SELECT %s FROM %s WHERE scientificName = @species",
    paste(cols, collapse = ", "),
    table
  )

  job <- bq_project_query(
    GCP_PROJECT,
    query,
    parameters = list(species = bq_param(species, "STRING")),
    use_query_cache = TRUE
  )
  bq_table_download(job)
}

get_timeline_data <- function(species, table, GCP_PROJECT) {
  # Constructs and executes a SQL query against the specified
  # BigQuery table. It counts the number of occurrences (N) for each year
  # for a particular species, excluding records where the year is NULL.
  # The results are ordered by year.
  #
  # Args:
  #   species: Character string. The scientific name of the species to query.
  #   table: Character string. The fully-qualified BigQuery table name.
  #   GCP_PROJECT: Character string. The Google Cloud Project ID.
  #
  # Returns:
  #   A data.table (result of bq_table_download) with two columns: 'year' and 'N'.
  query <- sprintf(
    "SELECT
         year,
         COUNT(*) AS N
       FROM %s
       WHERE scientificName = @species
         AND year IS NOT NULL
       GROUP BY year
       ORDER BY year",
    table
  )
  
  job <- bq_project_query(
    GCP_PROJECT,
    query,
    parameters = list(species = bq_param(species, "STRING")),
    use_query_cache = TRUE
  )
  bq_table_download(job)
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
        "<strong>Location: </strong>", country, ifelse(!is.na(region), paste0(" (", as.character(region), ")"), ""), "<br>",
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
      xaxis = list(
        title = "Year", 
        type = "date", 
        tickformat = "%Y"
        ),
      yaxis = list(title = "Count"),
      showlegend = FALSE,
      dragmode = FALSE,
      plot_bgcolor = "transparent",
      paper_bgcolor = "transparent",
      modebar = list(
        bgcolor = "transparent", 
        color = "#666666", 
        activecolor = color
        )
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

call_openrouter_api <- function(prompt, model_id = NULL, max_output_tokens = NULL) {
# Calls the OpenRouter API with a given prompt to get a model-generated response.
#
# Args:
#   prompt: Character string. The input prompt to send to the AI model.
#   model_id: Character string (optional). The specific model ID to use via OpenRouter.
#             If NULL, OpenRouter's default or configured model will be used.
#   max_output_tokens: Integer (optional). The maximum number of tokens the model
#                      should generate in the response. If NULL, the API's default will be used.
#
# Returns:
#   A list containing the API response.

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
