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
    tags$style(HTML("
      .leaflet-popup-content {
        width: auto !important;
        height: auto !important;
      }
      .navbar {
        min-height: 50px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .card {
        border-radius: 0 !important;
      }
      .info-button {
        font-size: 16px;
        color: #2DC86F;
        cursor: pointer;
        margin-right: 15px;
        background: none;
        border: none;
        padding: 0;
      }
      .info-button:hover,
      .info-button:active,
      .info-button:focus {
        color: #2DC86F;
        background: none;
        border: none;
        outline: none;
        box-shadow: none;
      }
      .modal-content {
        border-radius: 0;
      }
      .modal-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .close {
        font-size: 16px;
        color: #2DC86F;
        background: none;
        border: none;
        padding: 0;
        margin: 0;
      }
      .close:hover {
        color: #27ae60;
        background: none;
        border: none;
      }
      .dataset-link {
        display: inline-block;
        padding: 8px 12px;
        background-color: #2DC86F;
        border: none;
        text-decoration: none;
        color: #000;
        margin-top: 10px;
      }
      .radio-inline {
        margin-bottom: 10px;
      }
      .radio-inline input[type='radio'] {
        display: none;
      }
      .radio-inline input[type='radio'] + span {
        display: inline-block;
        width: 16px;
        height: 16px;
        margin: -1px 4px 0 0;
        vertical-align: middle;
        cursor: pointer;
        border-radius: 50%;
        border: 2px solid #ccc;
        background-color: #fff;
      }
      .radio-inline input[type='radio']:checked + span {
        background-color: #2DC86F;
        border-color: #2DC86F;
      }
      .title-header {
        font-size: 18px;
        font-weight: bold;
      }
    "))
  ),
  title = tagList(
    tags$div(
      "Biodiversity Observations in Poland (1984-2020)",
      class = "title-header"
    ),
    tags$button(
      icon("info-circle", class = "info-button"),
      onclick = "$('#info_modal').modal('show')",
      style = "background: none; border: none; padding: 0; cursor: pointer;"
    )
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
  ),
  tags$div(
    id = "info_modal",
    class = "modal fade",
    tabindex = "-1",
    `aria-hidden` = "true",
    tags$div(
      class = "modal-dialog",
      tags$div(
        class = "modal-content",
        tags$div(
          class = "modal-header",
          tags$strong(class = "modal-title", "Biodiversity"),
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            onclick = "$('#info_modal').modal('hide')",
            icon("times"), "Close"
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
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
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
