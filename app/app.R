# This Shiny application visualizes biodiversity observations from the
# Global Biodiversity Information Facility (GBIF). It allows users to
# explore species occurrences on a map and view the observation timeline.

# Load packages
library(shiny)
library(bslib)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(lubridate)
library(duckdb)
library(DBI)
library(httr2)

# Load modules
source("modules/species_search.R")
source("modules/map_viz.R")
source("modules/timeline_viz.R")

# Load utility functions
source("utils/functions.R")

# Get environment variables
readRenviron(".Renviron")

# Database connection
db_path <- "data/biodiversity.duckdb"

# Establish connection to DuckDB
if (!file.exists(db_path)) {
  stop("Database file not found. Please run prepare_data.R first.")
}

# Read-only for the app
db_con <- dbConnect(duckdb(), dbdir = db_path, read_only = TRUE)

# Close the database connection when the app stops
shiny::onStop(function() {
  dbDisconnect(db_con, shutdown = TRUE)
  print("Database connection closed.")
})

# Define color palette
primary_color <- "#27ae60"
secondary_color <- "#3498db"

# Define initial map view
initial_view <- list(lng = 10, lat = 50, zoom = 4)

# Define UI logic
ui <- page_fillable(

  # Use custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  # Set page title
  title = "Global Biodiversity",

  # Set theme options
  theme = bs_theme(
    primary = primary_color,
    secondary = secondary_color,
    base_font = "Segoe UI",
    heading_font = "Segoe UI"
  ),

  # Call map module
  map_ui("map"),

  # Add search panel in top left corner
  absolutePanel(
    card(
      card_title(
        span(
          strong("Global Biodiversity"),
          # Add info button
          actionLink(
            "info_modal",
            label = icon("info-circle"),
            class = "info-button"
          )
        )
      ),
      # Add species search UI component
      species_search_ui("species_search")
    ),
    draggable = TRUE,
    top = "3vh", left = "3vw", height = "auto", fixed = TRUE
  ),

  # Add timeline panel at bottom
  absolutePanel(
    card(
      card_title(uiOutput("timeline_title")),
      timeline_ui("timeline"),
      max_height = "30vh"
    ),
    draggable = TRUE,
    bottom = "4vh", left = "25vw", width = "50vw", fixed = TRUE
  )
)

# Define server logic
server <- function(input, output, session) {
  # Modal for info
  observeEvent(input$info_modal, {
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = "modal-header",
            tags$strong(class = "modal-title", "Global Biodiversity"),
            tags$button(
              type = "button",
              class = "close",
              `data-dismiss` = "modal",
              onclick = "Shiny.setInputValue('close_modal', true, {priority: 'event'});",
              icon("times"), " Close"
            )
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
            explore species occurrences on a map and view the observation timeline."),
          strong("Usage"),
          p("To get started, the app shows all observations globally.
            You can search for specific species using the search functionality
            and select a species to view its occurrences. The search and timeline panels can both be dragged.
            Use the AI Summary button to generate an overview and fun fact about the currently displayed data."),
          strong("Dataset information"),
          p("The dataset used in this app contains occurrence records of
            various species and comes from the GBIF."),
          tags$a(
            href = "https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165",
            class = "dataset-link",
            target = "_blank",
            icon("link"), "Dataset on GBIF"
          )
        ),
        size = "m",
        footer = NULL,
        easyClose = TRUE
      )
    )
  })

  # Close modal when clicking close_modal
  observeEvent(input$close_modal, {
    removeModal()
  })

  # Call species search module, passing the database connection
  selected_species <- species_search_server("species_search", db_con)

  # Create reactive expression for map data by querying the database
  map_data <- reactive({
    species <- selected_species()

    # Base columns needed for the map
    select_cols <- c("scientificName", "vernacularName", "eventDate", "country",
                     "longitudeDecimal", "latitudeDecimal", "accessURI", "creator")

    if (is.null(species)) {
      # No species selected: Show a limited sample (e.g., 50k records)
      # Adjust LIMIT as needed for performance vs. initial view
      query <- paste("SELECT", paste(select_cols, collapse = ", "),
                     "FROM occurrences LIMIT 50000")
      dbGetQuery(db_con, query)
    } else {
      # Species selected: Filter by scientificName
      query <- DBI::sqlInterpolate(
        db_con,
        paste("SELECT", paste(select_cols, collapse = ", "),
              "FROM occurrences WHERE scientificName = ?"),
        species
      )
      dbGetQuery(db_con, query)
    }
  })

  # Create reactive expression for timeline data by querying the database
  timeline_data <- reactive({
    species <- selected_species()

    if (is.null(species)) {
      # No species selected: Aggregate all data
      query <- "SELECT strftime(eventDate, '%Y') as year, COUNT(*) as N
                FROM occurrences
                WHERE year IS NOT NULL
                GROUP BY year ORDER BY year"
      dbGetQuery(db_con, query)
    } else {
      # Species selected: Filter and aggregate
      query <- DBI::sqlInterpolate(
        db_con,
        "SELECT strftime(eventDate, '%Y') as year, COUNT(*) as N
         FROM occurrences
         WHERE scientificName = ? AND year IS NOT NULL
         GROUP BY year ORDER BY year",
        species
      )
      dbGetQuery(db_con, query)
    }
  })

  # Call map module
  map_server("map", map_data, primary_color, initial_view)

  # Render dynamic timeline title
  output$timeline_title <- renderUI({
    species <- selected_species()

    if (is.null(species)) {
      strong("Observation Timeline: All Observations")
    } else {
      # Get the vernacular name for the selected species from the map data
      species_data <- subset(map_data(), scientificName == species)
      if (nrow(species_data) > 0) {
        vernacular <- species_data$vernacularName[1]
        strong(paste0("Observation Timeline: ", vernacular, " (", species, ")"))
      } else {
        strong(paste0("Observation Timeline: ", species))
      }
    }
  })

  # Call timeline module
  timeline_server("timeline", timeline_data, primary_color, secondary_color)

  # Handle AI summary button
  observeEvent(input[["species_search-summarize_btn"]], {
    # Get data
    species <- selected_species()
    current_data <- map_data()

    # Exit if no data
    if (nrow(current_data) == 0) {
      return()
    }

    # Calculate minimal summary
    n_records <- nrow(current_data)
    min_date <- min(as.Date(current_data$eventDate), na.rm = TRUE)
    max_date <- max(as.Date(current_data$eventDate), na.rm = TRUE)
    countries <- unique(current_data$country)
    countries <- countries[!is.na(countries) & countries != '']
    n_countries <- length(countries)

    country_info <- if(n_countries > 0) {
    if(n_countries > 10) {
      country_list <- paste(head(countries, 5), collapse=", ")
      paste(n_countries, 'countries/regions including', country_list, 'and others')
    } else {
      country_list <- paste(countries, collapse=", ")
      paste(n_countries, 'countries/regions:', country_list)
    }
  } else 'limited geographic data'

    # Variables based on species/global context
    if (is.null(species)) {
      # Global view
      species_count <- length(unique(current_data$scientificName))
      data_summary_text <- glue::glue(
        'Global biodiversity data: {n_records} observation records across {species_count} unique species from {format(min_date, "%Y-%m-%d")} to {format(max_date, "%Y-%m-%d")}, spanning {country_info}.'
      )
      prompt_subject <- 'global biodiversity data'
      fact_subject <- 'biodiversity monitoring or citizen science'
      modal_title <- 'Global Biodiversity Summary'
      
      ai_prompt <- glue::glue(
        'You are a biodiversity assistant providing information about {prompt_subject}.
        
        Create a concise and engaging response with the following two parts:

        1.  **OVERVIEW:** Based ONLY on this data summary, provide a short 1-2 sentence overview: "{data_summary_text}" # Constraint scoped to overview, length specified
        2.  **FUN FACT:** Using your general knowledge, provide a single interesting fact about {fact_subject}. # Explicitly mentions general knowledge

        Format clearly with "OVERVIEW:" and "FUN FACT:" headers. Keep the total response under 500 words.'
      )
      
    } else {
      # Species view
      vernacular <- current_data$vernacularName[1]
      vernacular_display <- if(is.na(vernacular) || vernacular == '') 'No common name available' else vernacular

      data_summary_text <- glue::glue(
        'Data for {vernacular_display} ({species}): {n_records} records from {format(min_date, "%Y-%m-%d")} to {format(max_date, "%Y-%m-%d")}, spanning {country_info}.'
      )
      
      prompt_subject <- paste('the species', species) 
      fact_subject <- paste('the species', species)
      modal_title <- paste('AI Summary for:', species)
      
      ai_prompt <- glue::glue(
        'You are a biodiversity assistant providing information about {prompt_subject}.
        # Instruction modified for clarity, conciseness, engagement, and THREE parts
        Create a concise and engaging response with the following three parts:
    
        1.  **SPECIES INFO:** Using your general knowledge, provide a very brief (1-2 sentence) introduction to {prompt_subject}. # NEW section added
        2.  **OVERVIEW:** Based ONLY on this data summary, provide a short 1-2 sentence overview: "{data_summary_text}" # Constraint scoped to overview, length specified
        3.  **FUN FACT:** Using your general knowledge, provide a single interesting fact about {fact_subject}. # Explicitly mentions general knowledge
    
        # Formatting instruction updated for 3 headers, word count reduced
        Format clearly with "SPECIES INFO:", "OVERVIEW:", and "FUN FACT:" headers. Keep the total response under 500 words.'
      )
    }

    # Show modal
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = 'modal-header',
            tags$strong(class = 'modal-title', modal_title),
            tags$button(
              type = 'button', class = 'close', `data-dismiss` = 'modal',
              onclick = 'Shiny.setInputValue("close_modal", true, {priority: "event"});',
              icon('times'), ' Close'
            )
          )
        ),
        tags$div(
          class = 'modal-body',
          'Generating summary...'
        ),
        size = 'xl', footer = NULL, easyClose = TRUE
      )
    )

    # Call API
    api_result <- call_openrouter_api(ai_prompt)

    # Show result
    showModal(
      modalDialog(
        titlePanel(
          tags$div(
            class = 'modal-header',
            tags$strong(class = 'modal-title', modal_title),
            tags$button(
              type = 'button', class = 'close', `data-dismiss` = 'modal',
              onclick = 'Shiny.setInputValue("close_modal", true, {priority: "event"});',
              icon('times'), ' Close'
            )
          )
        ),
        tags$div(
          class = 'modal-body',
          if (!is.null(api_result$error)) {
            tags$div(
              tags$p('Error generating summary:'),
              tags$pre(api_result$error)
            )
          } else {
            # Style content
            content <- api_result$content
            # Replace heading markers with formatted
            content <- gsub('SPECIES INFO:', strong('Species:'), content)
            content <- gsub('OVERVIEW:', strong('Overview:'), content)
            content <- gsub('FUN FACT:', strong('Fun Fact:'), content)
            content <- gsub('\\*\\*', '', content)
            # Convert newlines to <br> tags
            HTML(gsub('\n', '<br>', content))
          }
        ),
        size = 'xl', footer = NULL, easyClose = TRUE
      )
    )
  })
}

# Run app
shinyApp(ui = ui, server = server)
