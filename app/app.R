# Title: Biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# This Shiny application visualizes biodiversity observations from the
# Global Biodiversity Information Facility (GBIF). It allows users to
# explore species occurrences on a map and view the observation timeline.

# Load packages
library(shiny)
library(bslib)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(lubridate)
library(duckdb)
library(DBI)

# Load modules
source("modules/species_search.R")
source("modules/map_viz.R")
source("modules/timeline_viz.R")

# Load utility functions
source("utils/functions.R")

# Database connection
db_path <- "data/biodiversity.duckdb"

# Establish connection to DuckDB
if (!file.exists(db_path)) {
  stop("Database file not found. Please run prepare_data.R first.")
}

# Read-only for the app
db_con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)

# Close the database connection when the app stops
shiny::onStop(function() {
  dbDisconnect(db_con, shutdown = TRUE)
  print("Database connection closed.")
})

# Define color palette
primary_color <- "#27ae60"
secondary_color <- "#3498db"

# Define initial map view
initial_view <- list(lng = 10, lat = 50, zoom = 4)

# Define UI logic
ui <- page_fillable(

  # Use custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Set page title
  title = "Global Biodiversity",

  # Set theme options
  theme = bs_theme(
    primary = primary_color,
    base_font = "Segoe UI",
    heading_font = "Segoe UI"
  ),

  # Call map module
  map_ui("map"),

  # Add search panel in top left corner
  absolutePanel(
    card(
      card_title(
        span(
          strong("Global Biodiversity"),
          # Add info button
          actionLink(
            "info_modal",
            label = icon("info-circle"),
            class = "info-button"
          )
        )
      ),
      # Add species search UI component
      species_search_ui("species_search")
    ),
    top = "3vh", left = "3vw", height = "auto", fixed = TRUE
  ),

  # Add timeline panel at bottom
  absolutePanel(
    card(card_title(strong("Observation Timeline")), timeline_ui("timeline"), max_height = "30vh"),
    bottom = "4vh", left = "25vw", width = "50vw", fixed = TRUE
  )
)

# Define server logic
server <- function(input, output, session) {
  # Modal for info
  observeEvent(input$info_modal, {
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = "modal-header",
            tags$strong(class = "modal-title", "Global Biodiversity"),
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              onclick = "Shiny.setInputValue('close_modal', true, {priority: 'event'});",
              icon("times"), " Close"
            )
          )
        ),
        tags$div(
          class = "modal-body",
          p("Welcome to the Global Biodiversity app! I'm Przemek, a San Diego-based
            researcher and data scientist with a passion for using data to make things
            more interesting."),
          p(HTML("You can explore my other work <a href='https://przemyslawmarcowski.com' target='_blank'>here</a>.")),
          strong("About the project"),
          p("This Shiny app visualizes biodiversity observations from the
            Global Biodiversity Information Facility (GBIF). It allows you to
            explore species occurrences on a map and view the observation timeline."),
          strong("Usage"),
          p("To get started, the app shows all observations globally.
            You can search for specific species using the search functionality
            and select a species to view its occurrences."),
          strong("Dataset information"),
          p("The dataset used in this app contains occurrence records of
            various species and comes from the GBIF."),
          tags$a(
            href = "https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165",
            class = "dataset-link",
            target = "_blank",
            icon("link"), "Dataset on GBIF"
          )
        ),
        size = "m",
        footer = NULL,
        easyClose = TRUE
      )
    )
  })

  # Close modal when clicking close_modal
  observeEvent(input$close_modal, {
    removeModal()
  })

  # Call species search module, passing the database connection
  selected_species <- species_search_server("species_search", db_con)

  # Create reactive expression for map data by querying the database
  map_data <- reactive({
    species <- selected_species()

    # Base columns needed for the map
    select_cols <- c("scientificName", "vernacularName", "eventDate",
                     "longitudeDecimal", "latitudeDecimal", "accessURI", "creator")

    if (is.null(species)) {
      # No species selected: Show a limited sample (e.g., 50k records)
      # Adjust LIMIT as needed for performance vs. initial view
      query <- paste("SELECT", paste(select_cols, collapse = ", "),
                     "FROM occurrences LIMIT 50000")
      dbGetQuery(db_con, query)
    } else {
      # Species selected: Filter by scientificName
      query <- DBI::sqlInterpolate(
        db_con,
        paste("SELECT", paste(select_cols, collapse = ", "),
              "FROM occurrences WHERE scientificName = ?"),
        species
      )
      dbGetQuery(db_con, query)
    }
  })

  # Create reactive expression for timeline data by querying the database
  timeline_data <- reactive({
    species <- selected_species()

    if (is.null(species)) {
      # No species selected: Aggregate all data
      query <- "SELECT strftime(eventDate, '%Y') as year, COUNT(*) as N
                FROM occurrences
                WHERE year IS NOT NULL
                GROUP BY year ORDER BY year"
      dbGetQuery(db_con, query)
    } else {
      # Species selected: Filter and aggregate
      query <- DBI::sqlInterpolate(
        db_con,
        "SELECT strftime(eventDate, '%Y') as year, COUNT(*) as N
         FROM occurrences
         WHERE scientificName = ? AND year IS NOT NULL
         GROUP BY year ORDER BY year",
        species
      )
      dbGetQuery(db_con, query)
    }
  })

  # Call map module
  map_server("map", map_data, secondary_color, initial_view)

  # Call timeline module
  timeline_server("timeline", timeline_data, primary_color)
}

# Run app
shinyApp(ui = ui, server = server)
