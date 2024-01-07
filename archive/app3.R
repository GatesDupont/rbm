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
source("global_functions_variables2.R")


# ---- UI Setup ----

# User interface
ui <- bootstrapPage(
  
  # SETTINGS: map to full-screen
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # OUTPUT: Leaflet map
  # leafletOutput("map", width = "100%", height = "100%"),
  uiOutput("dynamicMapUI"),
  
  # SETTINGS: title
  absolutePanel(top = 1, left = 50, draggable = F, 
                h3("eBird Rarity Viewer"),
                h5("by Gates Dupont")),
  
  # SELECT: Region code
  absolutePanel(top = 10, right = 45, draggable = F,
                selectizeInput("regionInput", "Region Code", 
                               choices = NULL,
                               width = 210,
                               multiple = FALSE, 
                               options = NULL)),
  
  # SELECT: Days back
  absolutePanel(bottom = 10, left = 45, width = NULL, draggable = F,
                sliderInput("backInput", "Days Back",
                            min = 1, max = 30, value = 2, round = T)),
  
  # SELECT: Species
  absolutePanel(top = 70, right = 45, draggable = FALSE,
                selectInput("speciesInput", "Species", choices = c("All Species" = ""),
                            selected = "All Species")),
  
  # BUTTON: Reset to all species
  conditionalPanel(
    condition = "input.speciesInput !== ''",
    absolutePanel(top = 140, right = 45, draggable = FALSE,
                  actionButton("resetSpecies", "Show All Species"))
  )
  
)


# ---- Server Function ----

# Server
server <- function(input, output, session) {
  
  # Reactive value to track data state ('loading', 'data', 'noData', 'error')
  dataState <- reactiveVal("loading")
  
  # SELECTIZE
  updateSelectizeInput(session, 'regionInput', 
                       selected = "Middlesex, MA, US", 
                       choices = region_choices, 
                       server = TRUE)
  
  # FETCH data
  ebd_data_reactive <- reactive({
    
    
    if(input$regionInput == ""){
      
      updateSelectInput(session, "speciesInput",
                        choices = c("All Species" = ""),
                        selected = "All Species")
      
      return(2)
      
    }
    
    region_code <- region_name_to_code(input$regionInput)
    back <- input$backInput
    ebd_data <- ebd_api_fetch(api_key, region_code, back)
    
    if(is_tibble(ebd_data)){
      
      # Fetch the taxonomic information
      species_df <- ebd_data %>%
        dplyr::select(species_code = speciesCode, comName) %>%
        distinct()
      species_codes <- species_df$species_code
      taxonomy_df <- get_taxon_order(api_key, species_codes) %>%
        arrange(taxonOrder) %>%
        left_join(species_df, by = join_by(species_code))
      
      # Update species selection input based on the taxonomically sorted data
      species_choices <- taxonomy_df$comName
      
      # Update species selection input based on the fetched data
      updateSelectInput(session, "speciesInput",
                        choices = c("All Species" = "", species_choices),
                        selected = "All Species")
      
      return(ebd_data)
      
    }
    
    if(is.null(ebd_data)){
      
      updateSelectInput(session, "speciesInput",
                        choices = c("All Species" = ""),
                        selected = "All Species")
      
      return("No rarities reported")
      
    }
    
  })
  
  # MAP
  output$map <- renderLeaflet({
    
    # Pull in the eBird data
    ebd_data <- ebd_data_reactive()
    
    # Still loading (selectize lag issue)
    if(is.numeric(ebd_data)){

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = tileOptions(opacity = 0)) %>%
        addLabelOnlyMarkers(lng = -71.34894745998565, lat = 42.46038149269142,
                            label = "Loading",
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = 'center'))

    }
    
    # No rarities in that region
    if(is.character(ebd_data)){

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = tileOptions(opacity = 0)) %>%
        addLabelOnlyMarkers(lng = -71.34894745998565, lat = 42.46038149269142,
                            label = "No rare birds to report. Change selections.",
                            labelOptions = labelOptions(noHide = TRUE,
                                                        direction = 'center'))

    }
    
    # Normal operation
    if(is_tibble(ebd_data)){
      
      # Filter data if a specific species is selected
      if (!is.null(input$speciesInput) && input$speciesInput != "") {
        ebd_data <- ebd_data %>% filter(comName == input$speciesInput)
      }
      
      # Create the leaflet map
      leaflet(ebd_data, options = leafletOptions(zoomDelta = 0.25)) %>%
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
  
}


# ---- Shiny App Initialization ----

# Pull it together!
shinyApp(ui = ui, server = server)

