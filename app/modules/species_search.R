# Title: Species search module for the biodiversity observations Shiny app
# Author: Przemyslaw Marcowski, PhD
# Email: p.marcowski@gmail.com
# Date: 2024-05-28
# Copyright (c) 2024 Przemyslaw Marcowski

# This module provides the UI and server logic for the species search
# functionality in the biodiversity observations Shiny app.

# UI function for species search module
species_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    textInput(ns("query_species"), strong("Search species"), "", width = "auto"),
    input_task_button(ns("search_btn"), strong("Search")),
    actionButton(ns("reset_btn"), strong("Reset selection")),
    uiOutput(ns("species_label")),
    div(
      uiOutput(ns("species_ui")),
      class = "scrollable"
    )
  )
}

# Server function for species search module
species_search_server <- function(id, occurence) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_species <- reactiveVal(NULL)

    observeEvent(input$search_btn, {
      search_query <- input$query_species
      if (search_query != "") {
        filtered_species <- search_species(search_query, occurence)
        if (nrow(filtered_species) == 0) {
          output$species_label <- renderUI({
            NULL
          })
          output$species_ui <- renderUI({
            p("No matching species found")
          })
        } else {
          species_choices <- unique(filtered_species[, .(scientificName, vernacularName)])
          species_choices[, choice_name := paste0(tolower(vernacularName), "<br><em>", scientificName, "</em>")]
          output$species_label <- renderUI({
            strong("Select species")
          })
          output$species_ui <- renderUI({
            radioButtons(ns("select_species"), NULL,
              choiceNames = lapply(species_choices$choice_name, HTML),
              choiceValues = species_choices$scientificName,
              inline = FALSE
            )
          })
        }
      } else {
        output$species_label <- renderUI({
          NULL
        })
        output$species_ui <- renderUI({
          NULL
        })
      }
    })

    observeEvent(input$select_species, {
      selected_species(input$select_species)
    })

    observeEvent(input$reset_btn, {
      selected_species(NULL)
      updateTextInput(session, "query_species", value = "")
      output$species_label <- renderUI({
        NULL
      })
      output$species_ui <- renderUI({
        NULL
      })
    })

    selected_species
  })
}
