library(shiny)
library(leaflet)


# Fetching custom map tiles and adding citation
custom_map = "https://api.mapbox.com/styles/v1/heliornis/cjboo3dac64w02srud7535eec/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGVsaW9ybmlzIiwiYSI6ImNqNGtjZjE1cjBoYmcycXAzbmlmNmJieGEifQ.ERz0DjQKEE1PBd7myLKwZA"
mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> © <a href='http://ebird.org/content/ebird/about/'>eBird / Cornell Lab of Ornithology</a> © <a href='https://www.gatesdupont.com/'>Gates Dupont</a>"


### USER INTERFACE ### -------------------------------------------------------------------
ui <- bootstrapPage(
  
  # Setting map to FULL-SCREEN
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # Initializing LEAFLET output
  leafletOutput("myMap", width="100%", height="100%"),
)


### SERVER ### ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  
  ## -------------------------------------------------------------------------------------
  # Leaflet map
  output$myMap = renderLeaflet({
    
    data <- data.frame(lat = 42.487682,
                       lng = -71.393756)
    
    leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
      addTiles(urlTemplate = custom_map, attribution = mb_attribution) %>%
      addCircleMarkers(data = data)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

