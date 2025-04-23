# Timeline visualization module for the biodiversity observations Shiny app

# UI function for timeline module
timeline_ui <- function(id) {
  ns <- NS(id)
  # Create Plotly output element for timeline
  plotlyOutput(ns("timeline"))
}

# Server function for timeline module
timeline_server <- function(id, timeline_data, color, secondary_color = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render Plotly timeline
    output$timeline <- renderPlotly({
      # Wait for timeline_data to be available
      req(timeline_data())
      # Call render_timeline function with current data and colors
      render_timeline(timeline_data(), color, secondary_color)
    })
  })
}
