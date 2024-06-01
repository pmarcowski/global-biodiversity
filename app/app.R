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
library(stringi)
library(leaflet)
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

# Generate color palette for all species
base_green <- c("#2ecc71", "#27ae60", "#229954", "#1e8449", "#196f3d")
species <- unique(occurence$scientificName)
color_palette <- colorFactor(colorRampPalette(base_green)(length(species)), domain = species)
first_green <- color_palette(species[1])

# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  title = tagList(
    tags$div(
      "Biodiversity Observations in Poland (1984-2020)",
      class = "title-header"
    ),
    actionLink("info_modal", label = icon("info-circle"), class = "info-button")
  ),
  window_title = "Biodiversity Observations",
  theme = bs_theme(
    primary = first_green,
    base_font = "Segoe UI",
    heading_font = "Segoe UI"
  ),
  sidebar = sidebar(
    width = 300,
    speciesSearchUI("species_search")
  ),
  card(
    card_title(strong("Observation map")),
    mapUI("map")
  ),
  card(
    card_title(strong("Observation timeline")),
    timelineUI("timeline")
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  # Modal for info
  observeEvent(input$info_modal, {
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = "modal-header",
            tags$strong(class = "modal-title", "Biodiversity"),
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              onclick = "Shiny.setInputValue('close_modal', true, {priority: 'event'});",
              icon("times"), ""
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
          explore species occurrences on a map and view the observation
          timeline."),
          strong("Usage"),
          p("To use this app, enter a species name in the search box to find
          matching species. Select a species from the list to view its
          occurrences on the map and timeline."),
          strong("Dataset information"),
          p("The dataset used in this app contains occurrence records of
          various species and comes from the Global Biodiversity Information
          Facility."),
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
  selected_species <- speciesSearchServer("species_search", occurence)

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
  mapServer("map", map_data, color_palette)

  # Call timeline module
  timelineServer("timeline", timeline_data, first_green)
}

# Run app
shinyApp(ui = ui, server = server)
