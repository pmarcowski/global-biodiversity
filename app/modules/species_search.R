# Species search module for the biodiversity observations Shiny app

# UI function for species search module
species_search_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Search input field
    textInput(ns("query_species"), strong("Search Species"), "", width = "auto"),
    # Search button
    input_task_button(ns("search_btn"), strong("Search"), type = "primary"),
    # Reset button
    input_task_button(ns("reset_btn"), strong("Reset Selection"), type = "dark"),
    # AI Summary button
    input_task_button(ns("summarize_btn"), strong("AI Summary"), type = "secondary"),
    # Label for species selection (dynamically rendered)
    uiOutput(ns("species_label")),
    # Scrollable div for species list
    div(
      uiOutput(ns("species_ui")),
      class = "scrollable"
    )
  )
}

# Server function for species search module
species_search_server <- function(id, db_con) { # Changed occurence to db_con
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    # Reactive value to store selected species
    selected_species <- reactiveVal(NULL)

    # Observe search button click
    observeEvent(input$search_btn, {
      search_query <- input$query_species
      if (search_query != "") {
        # Search species in the database
        filtered_species_df <- search_species(search_query, db_con) # Use db_con

        # Convert result to data.table for subsequent operations
        filtered_species <- setDT(filtered_species_df)

        if (nrow(filtered_species) == 0) {
          # Display message if no species found
          output$species_label <- renderUI({ NULL })
          output$species_ui <- renderUI({ p("No matching species found") })
        } else {
          # Prepare and display list of found species
          species_choices <- unique(filtered_species[, .(scientificName, vernacularName)])
          species_choices[, choice_name := paste0(vernacularName, "<br><em>", scientificName, "</em>")]
          output$species_label <- renderUI({ strong("Select Species") })
          output$species_ui <- renderUI({
            radioButtons(ns("select_species"), NULL,
                         choiceNames = lapply(species_choices$choice_name, HTML),
                         choiceValues = species_choices$scientificName,
                         inline = FALSE
            )
          })
        }
      } else {
        # Clear species list if search query is empty
        output$species_label <- renderUI({ NULL })
        output$species_ui <- renderUI({ NULL })
      }
    })

    # Update selected species when user makes selection
    observeEvent(input$select_species, {
      selected_species(input$select_species)
    })

    # Reset selection and clear search
    observeEvent(input$reset_btn, {
      selected_species(NULL)
      updateTextInput(session, "query_species", value = "")
      output$species_label <- renderUI({ NULL })
      output$species_ui <- renderUI({ NULL })
    })

    # Return reactive value containing selected species
    selected_species
  })
}
