# Title: Map visualization module for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This module provides the UI and server logic for the map visualization
# functionality in the biodiversity observations Shiny app.

# UI function for map module
map_ui <- function(id) {
  ns <- NS(id)
  leafletOutput(ns("map"))
}

# Server function for map module
map_server <- function(id, map_data, color, initial_view) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$map <- renderLeaflet({
      req(map_data())
      render_map(map_data(), color, initial_view)
    })
  })
}
