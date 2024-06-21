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
  # Create Plotly output element for timeline
  plotlyOutput(ns("timeline"))
}

# Server function for timeline module
timeline_server <- function(id, timeline_data, color) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Render Plotly timeline
    output$timeline <- renderPlotly({
      # Wait for timeline_data to be available
      req(timeline_data())
      # Call render_timeline function with current data and color
      render_timeline(timeline_data(), color)
    })
  })
}
