library(shiny)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)
source("global_functions_variables.R")


# ---- UI Setup ----
ui <- bootstrapPage(
  
  # SETTINGS: map to full-screen
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # OUTPUT: Leaflet map
  leafletOutput("map", width = "100%", height = "100%"),
  
  # SETTINGS: title
  absolutePanel(top = 1, left = 50, draggable = F, 
                h3("eBird Rarity Viewer"),
                h5("by Gates Dupont")),

  # SELECT: Region code
  absolutePanel(top = 10, right = 45, draggable = F,
                selectizeInput("regionInput", "Region Code", 
                               # selected = "Massachusetts, US", 
                               choices = NULL,
                               width = 210,
                               multiple = FALSE, 
                               options = NULL)),

  
  # SELECT: Days back
  absolutePanel(bottom = 10, left = 45, draggable = F,
                sliderInput("backInput", "Days Back",
                            min = 1, max = 30, value = 2, round = T))
)


# ---- Server Function ----
server <- function(input, output, session) {
  
  # SELECTIZE
  updateSelectizeInput(session, 'regionInput', 
                       selected = "Massachusetts, US", 
                       choices = region_choices, 
                       server = TRUE)
  
  # MAP
  output$map <- renderLeaflet({
    
    # Update inputs
    region_code <- region_name_to_code(input$regionInput)
    back <- input$backInput
    ebd_data <- ebd_api_fetch2(api_key, region_code, back)
    
    # Check if ebd_data is NULL or empty
    if (is.null(ebd_data) || nrow(ebd_data) == 0) {
      
      # Return a blank map
      return(leaflet(options = leafletOptions(zoomDelta = 0.25)) %>%
               addProviderTiles(providers$CartoDB.Positron))
    } else {
      
      # Create the leaflet map
      leaflet(ebd_data, options = leafletOptions(zoomDelta = 0.25)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addAwesomeMarkers(
          lat = ~lat, lng = ~lng,
          clusterOptions = markerClusterOptions(
            maxClusterRadius = 5, 
            spiderfyOnMaxZoom = TRUE,
            zoomToBoundsOnClick = FALSE),
          icon = icons,
          popup = ~paste(comName, "<br>",
                         locName, "<br>",
                         "Most recent checklist:", create_eBird_link(link), "<br>", 
                         "Records:", n_records, "<br>",
                         "Days:", n_days),
          label = ~comName) %>%
        addLegend(position = "bottomright", 
                  colors = c("#5AA6D6", "#9AD8FB"), 
                  labels = c("Accepted", "Awaiting review"),
                  title = "Review status", opacity = 1) 
      
    }
    
  })
}


# ---- Shiny App Initialization ----
shinyApp(ui = ui, server = server)
