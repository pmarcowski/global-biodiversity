# Map visualization module for the biodiversity observations Shiny app

# UI function for map module
map_ui <- function(id) {
  ns <- NS(id)
  # Create Leaflet output element
  leafletOutput(ns("map"))
}

# Server function for map module
map_server <- function(id, map_data, color, initial_view) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Render Leaflet map
    output$map <- renderLeaflet({
      # Wait for map_data to be available
      req(map_data())
      # Call render_map function with current data and settings
      render_map(map_data(), color, initial_view)
    })
  })
}
