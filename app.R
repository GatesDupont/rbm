library(shiny)
library(tidyverse)
library(vroom)
library(lubridate)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)
library(httr)
library(jsonlite)
source("global_functions_variables.R")


# ---- UI Setup ----

# User interface
ui <- bootstrapPage(
  
  # Enable Bookmarking
  # enableBookmarking("url"),
  
  # SETTINGS: map to full-screen
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # OUTPUT: Leaflet map
  leafletOutput("map", width = "100%", height = "100%"),
  
  # SETTINGS: title
  absolutePanel(top = 1, left = 50, draggable = F, 
                h3("eBird Rarity Viewer"),
                h5("by Gates Dupont")),
  
  # SELECT: Days back
  absolutePanel(bottom = 10, left = 45, width = NULL, draggable = F,
                style = "background-color: rgba(255, 255, 255, 0.75); border-radius: 5px; padding: 10px;",
                sliderInput("backInput", "Days Back",
                            min = 1, max = 30, value = 3, round = T)),

  # SELECT: Region code
  absolutePanel(top = 10, right = 45, draggable = F,
                selectizeInput("regionInput", "County, State, or Country", 
                               choices = NULL,
                               width = 210,
                               multiple = FALSE, 
                               options = NULL)),

  # SELECT: Species
  absolutePanel(top = 70, right = 45, draggable = FALSE,
                selectInput("speciesInput", "Species", choices = c("All Species" = ""),
                            selected = "All Species")),
  
  # BUTTON: Reset to all species
  conditionalPanel(
    condition = "input.speciesInput !== ''",
    absolutePanel(top = 140, right = 45, draggable = FALSE,
                  actionButton("resetSpecies", "Show All Species"))),
  
  # NOTE: about app loading time
  absolutePanel(top = "auto", right = 10, bottom = 100, draggable = TRUE, 
                style = "background-color: rgba(255, 255, 255, 0.9); padding: 5px; width: 139px; height: auto; max-height: 100px; overflow-y: auto; z-index: 1000;",
                p("Map loading may take a few seconds; a blank screen indicates no reports."),
                id = "notePanel"
  )
  
)


# ---- Server Function ----

# Server
server <- function(input, output, session) {
  
  # SELECTIZE
  updateSelectizeInput(session, 'regionInput', 
                       selected = "Middlesex, MA, US", 
                       choices = region_choices, 
                       server = TRUE)
  
  # FETCH data
  ebd_data_reactive <- reactive({
    region_code <- region_name_to_code(input$regionInput)
    back <- input$backInput
    ebd_data <- ebd_api_fetch(api_key, region_code, back)
    
    if(!is.null(ebd_data)){
      
      # Fetch the taxonomic information
      species_df <- ebd_data %>%
        dplyr::select(species_code = speciesCode, comName) %>%
        distinct()
      species_codes <- species_df$species_code
      taxonomy_df <- get_taxon_order(species_codes) %>%
        arrange(taxon_order) %>%
        left_join(species_df, by = join_by(species_code))

      # Update species selection input based on the taxonomically sorted data
      species_choices <- taxonomy_df$comName
      
      # Update species selection input based on the fetched data
      updateSelectInput(session, "speciesInput",
                        choices = c("All Species" = "", species_choices),
                        selected = "All Species")
    } else {
      
      # Update species selection input based on the fetched data
      updateSelectInput(session, "speciesInput", 
                        choices = c("All Species" = "", unique(ebd_data$comName)),
                        selected = "All Species")
      
    }
    
    return(ebd_data)
  })
  
  # MAP
  output$map <- renderLeaflet({
    
    # Pull in the eBird data
    ebd_data <- ebd_data_reactive()
    
    # Filter data if a specific species is selected
    if (!is.null(input$speciesInput) && input$speciesInput != "") {
      ebd_data <- ebd_data %>% filter(comName == input$speciesInput)
    }
    
    # Check if ebd_data is NULL or empty
    if (is.null(ebd_data) || nrow(ebd_data) == 0) {
      
      # Return a blank map
      return(leaflet(options = leafletOptions(zoomDelta = 0.25)) %>%
               addProviderTiles(providers$CartoDB.Positron,
                                options = tileOptions(opacity = 0)))
    } else {
      
      # Create the leaflet map
      # leaflet(ebd_data, options = leafletOptions(zoomDelta = 0.25, zoomControl = F)) %>%
      leaflet(ebd_data, options = leafletOptions(zoomDelta = 0.5, zoomControl = T)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addAwesomeMarkers(
          lat = ~lat, lng = ~lng,
          clusterOptions = markerClusterOptions(
            spiderfyDistanceMultiplier = 1.5,
            maxClusterRadius = 5, 
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = FALSE),
          icon = icons,
          popup = ~paste("<b>", comName, "</b>", "<br>",
                         locName, "<br>",
                         "Most recent checklist:", create_eBird_link(link), "<br>", 
                         "Last seen:", last_seen, "<br>",
                         "Records:", n_records, "<br>",
                         "Days:", n_days),
          label = ~comName) %>%
        addLegend(position = "bottomright", 
                  colors = c("#5AA6D6", "#9AD8FB"), 
                  labels = c("Accepted", "Awaiting review"),
                  title = "Review status", opacity = 1) 
      
    }
    
  })
  
  # Observer for Reset Species Button
  observeEvent(input$resetSpecies, {
    updateSelectInput(session, "speciesInput", selected = "All Species")
  })
  
  # UPDATING URL
  
}


# ---- Shiny App Initialization ----

# Pull it together!
shinyApp(ui = ui, server = server)

