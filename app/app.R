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

# Load utility functions
source("utils/functions.R")

# Load modules
source("modules/species_search.R")
source("modules/map_viz.R")
source("modules/timeline_viz.R")

# Load prepared data
occurence <- readRDS("data/occurence_prepared.Rds")
setDT(occurence)

# Indexing for faster access
setkey(occurence, id)
setindex(occurence, scientificName)
setindex(occurence, vernacularName)
setindex(occurence, eventDate)
setindex(occurence, longitudeDecimal)
setindex(occurence, latitudeDecimal)
setindex(occurence, country)
setindex(occurence, accessURI)

# Define primary color
primary_color <- "#27ae60"

# Define initial map view
initial_view <- list(lng = 10, lat = 50, zoom = 4)

# UI -------------------------------------------------------------------

ui <- page_fillable(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  title = "Biodiversity observations",
  theme = bs_theme(
    primary = primary_color,
    base_font = "Segoe UI",
    heading_font = "Segoe UI"
  ),
  map_ui("map"),
  absolutePanel(
    card(
      card_title(
        span(
          strong("Biodiversity observations"),
          actionLink(
            "info_modal",
            label = icon("info-circle"),
            class = "info-button"
          )
        )
      ),
      species_search_ui("species_search")
    ),
    top = "4vh", left = "4vw", height = "auto", fixed = TRUE
  ),
  absolutePanel(
    card(card_title(strong("Observation timeline")), timeline_ui("timeline"), max_height = "30vh"),
    bottom = "4vh", left = "25vw", width = "50vw", fixed = TRUE
  )
)

# Server ---------------------------------------------------------------

server <- function(input, output, session) {
  # Modal for info
  observeEvent(input$info_modal, {
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = "modal-header",
            tags$strong(class = "modal-title", "Biodiversity observations"),
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

  # Call species search module
  selected_species <- species_search_server("species_search", occurence)

  # Create reactive expression for map data
  map_data <- reactive({
    if (is.null(selected_species())) {
      occurence
    } else {
      filter_species(occurence, selected_species())
    }
  })

  # Create reactive expression for timeline data
  timeline_data <- reactive({
    if (is.null(selected_species())) {
      count_by_year(occurence)
    } else {
      count_by_year(filter_species(occurence, selected_species()))
    }
  })

  # Call map module
  map_server("map", map_data, primary_color, initial_view)

  # Call timeline module
  timeline_server("timeline", timeline_data, primary_color)
}

# Run app
shinyApp(ui = ui, server = server)
