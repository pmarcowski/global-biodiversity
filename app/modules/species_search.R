# Title: Species search module for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This module provides the UI and server logic for the species search
# functionality in the biodiversity observations Shiny app.

# UI function for the species search module
speciesSearchUI <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("query_species"), strong("Search Species"), ""),
    input_task_button(ns("search_btn"), strong("Search")),
    actionButton(ns("reset_btn"), strong("Show all species")),
    uiOutput(ns("species_ui"))
  )
}

# Server function for the species search module
speciesSearchServer <- function(id, occurence) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the selected species
    selected_species <- reactiveVal(NULL)

    # Display species choices based on query
    observeEvent(input$search_btn, {
      search_query <- input$query_species
      if (search_query != "") {
        filtered_species <- search_species(search_query, occurence)

        if (nrow(filtered_species) == 0) {
          output$species_ui <- renderUI({
            p("No matching species found")
          })
        } else {
          species_choices <- unique(filtered_species$scientificName)
          output$species_ui <- renderUI({
            radioButtons(ns("select_species"), strong("Select Species"),
              choices = species_choices, inline = FALSE
            )
          })
        }
      }
    })

    # Update selected species based on radio buttons
    observeEvent(input$select_species, {
      selected_species(input$select_species)
    })

    # Reset to show all species
    observeEvent(input$reset_btn, {
      selected_species(NULL)
      updateTextInput(session, "query_species", value = "")
      output$species_ui <- renderUI({
        NULL
      })
    })

    return(selected_species)
  })
}
