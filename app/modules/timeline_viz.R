# Title: Timeline visualization module for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This module provides the UI and server logic for the timeline visualization
# functionality in the biodiversity observations Shiny app.

# UI function for timeline module
timeline_ui <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("timeline"))
}

# Server function for timeline module
timeline_server <- function(id, timeline_data, color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$timeline <- renderPlotly({
      req(timeline_data())
      render_timeline(timeline_data(), color)
    })
  })
}
