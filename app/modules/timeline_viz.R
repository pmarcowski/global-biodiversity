# Title: Timeline visualization module for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This module provides the UI and server logic for the timeline visualization
# functionality in the biodiversity observations Shiny app.

# UI function for the timeline module
timelineUI <- function(id) {
  ns <- NS(id)
  plotlyOutput(ns("timeline"))
}

# Server function for the timeline module
timelineServer <- function(id, timeline_data, first_green) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$timeline <- renderPlotly({
      req(timeline_data())
      render_timeline(timeline_data(), first_green)
    })
  })
}
