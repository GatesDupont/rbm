
# TODO:
"Region NAME - figure out how to translate from region name input to region code"

### GLOBAL SPACE ### ---------------------------------------------------------------------

library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(tidyverse)
library(vroom)


# Opening connection to pull functions from external file
source('./Functions.R')

# Pulling region code choices from external file
# choices <- read.csv("./data/choices.csv") %>%
#   pull(x) %>%
#   as.character()
region_descriptors <- vroom("files/region_descriptors.csv", delim = ",")
choices <- region_descriptors %>%
  pull(FULL)

# Fetching custom map tiles and adding citation
custom_map = "https://api.mapbox.com/styles/v1/heliornis/cjboo3dac64w02srud7535eec/tiles/256/{z}/{x}/{y}?access_token=pk.eyJ1IjoiaGVsaW9ybmlzIiwiYSI6ImNqNGtjZjE1cjBoYmcycXAzbmlmNmJieGEifQ.ERz0DjQKEE1PBd7myLKwZA"
mb_attribution <- "© <a href='https://www.mapbox.com/map-feedback/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> © <a href='http://ebird.org/content/ebird/about/'>eBird / Cornell Lab of Ornithology</a> © <a href='https://www.gatesdupont.com/'>Gates Dupont</a>"

# Making my location icon
uloc = makeIcon(iconUrl = "./uloc.png", iconHeight = 25, iconWidth = 25)


### USER INTERFACE ### -------------------------------------------------------------------
ui <- bootstrapPage(
  
  # Adding dynamically updating USER LOC
  tags$script(geoloc()),
  
  # Add Google Analytics data
  tags$head(HTML(gtag())),
  
  # Setting THEME
  theme = shinytheme("superhero"),
  
  # Setting map to FULL-SCREEN
  tags$style(type="text/css", "html, body {width:100%;height:100%}"),
  
  # Initializing LEAFLET output
  leafletOutput("myMap", width="100%", height="100%"),
  
  # Adding TITLE overlayed on leaflet map
  absolutePanel(top = 1, left = 50, draggable = F, 
                h4("eBird Rarity Viewer"),
                h6("by Gates Dupont")),
  
  # Adding SLIDER input overlayed on leaflet map
  absolutePanel(bottom = 1, left = 45, draggable = F, 
                sliderInput("slider_in", "Days Back", 
                            min = 1, max = 30, value = 3, round = T)),
  
  # Adding REGION INPUT overlayed on leaflet map
  # absolutePanel(top = 1, right = 45, draggable = F,
  #               selectInput("region_in", "Region Code", choices = choices,
  #                           selected = "Massachusetts, US", multiple = F, width  = 210)),
  # absolutePanel(top = 1, right = 45, draggable = FALSE,
  #               selectizeInput("region_in", "Region Code", choices = NULL,
  #                              selected = "Massachusetts, US", multiple = FALSE, width = "210px")),
  absolutePanel(top = 1, right = 45, draggable = FALSE,
                selectizeInput("region_in", "Region Code", choices = NULL,
                               multiple = FALSE, width = "210px")),
  
  # Adding SELECT SPECIES INPUT overlayed on leaflet map
  absolutePanel(top = 70, right = 45, width = NULL, draggable = T,
                selectInput("species_in", "Species", choices = "",
                            selected = "", multiple = F, width  = 210)),
  
  # Adding ALL SEPECIES BUTTON overlayed on leaflet map
  conditionalPanel(condition = "input.species_in != ''", absolutePanel(top = 160, right = 45, width = NULL, draggable = T,
                                                                       actionButton("allspp", "All species", width  = 210)))
)


### SERVER ### ---------------------------------------------------------------------------
server <- function(input, output, session) {
  
  observe({
    updateSelectizeInput(session, "region_in", choices = choices, server = TRUE, selected = "Massachusetts, US")
  })
  
  ## -------------------------------------------------------------------------------------
  # Rendering data frame from API with slider input 
  APIdata <- reactive({
    
    # Match region name to region code
    region_id <- region_descriptors %>%
      filter(FULL == input$region_in) %>%
      pull(REGION_CODE) %>%
      as.character()
    
    # Initial fetch of data from eBird API, with conditionals to reject errant input
    a <- try(api2(regionCode = region_id, 
                  back = as.numeric(input$slider_in)))
    if(class(a) == "try-error" ||length(a) == 0){return(NULL)}
    
    return(a)
  })
  
  ## -------------------------------------------------------------------------------------
  # Separating API call and further data work
  APIdata2 <- reactive({
    
    a <- APIdata()
    
    # Jittering lat/lon points to fix point overlap
    a$lat = jitter(a$lat, amount = .001) # Amount instead of factor, data-dependent jitter
    
    # Changing review status from logical to numeric
    cols <- sapply(a, is.logical)
    a[,cols] <- lapply(a[,cols], as.numeric)
    
    # Initializing new date column
    a["date"] <- format(strptime(a$obsDt, format = "%Y-%m-%d"), "%b %d")
    
    # Initializing new color grouping column
    a["group"] <- NA
    
    # Assigning colors by review status
    idx<-  (a$obsReviewed == F) & (a$obsValid == F)# Not reviewed
    a$group[idx] <- "white"
    idx<- (a$obsReviewed == F) & (a$obsValid == T) # Accepted
    a$group[idx] <- "green"
    idx<- (a$obsReviewed == T) & (a$obsValid == T) # Accepted
    a$group[idx] <- "green"
    
    
    # Adding url for list popups
    a["url"] <- NA
    a$url = sapply(a$subId, subIDurl)
    
    return(a)
  })
  
  ## -------------------------------------------------------------------------------------
  # Doing more to the data frame
  APIdata3 <- reactive({
    
    a <- APIdata2()
    
    # Species search filtering
    if(input$species_in %in% a$comName){
      #a = subset(a, a$comName == as.character(input$species_in))
      a = a[a$comName == as.character(input$species_in),]
      return(a)
    }else{return(a)}
    
    return(a)
  })
  
  ## -------------------------------------------------------------------------------------
  # Updating species input selection
  observeEvent({APIdata()},{
    updateSelectInput(session, "species_in", choices = taxify(unique(APIdata()[["comName"]])), selected = "")
  })
  
  ## -------------------------------------------------------------------------------------
  # Add a button to jump back to all species, tied to conditional panel
  observeEvent(input$allspp,{
    updateSelectInput(session, "species_in", selected = "") # removed choices = taxify(unique(APIdata()[["comName"]])),
  })
  
  ## -------------------------------------------------------------------------------------
  # Dynamically updating user location
  observe({
    if(!is.null(input$lat)){
      
      ulat <- input$lat
      ulng <- input$long
      acc <- input$accuracy
      time <- input$time
      
      proxy <- leafletProxy("myMap")
      
      proxy  %>% 
        clearGroup(group="pos") %>% 
        addMarkers(icon = uloc,lng=ulng, lat=ulat, label = "My Location", 
                   popup=paste("My location is:","<br>", 
                               ulng,"Longitude","<br>", ulat,"Latitude", 
                               "<br>", "My accuracy is:",  "<br>", acc, "meters"), 
                   group="pos") %>%
        addCircles(lng=ulng, lat=ulat, radius=0, group="pos")
      #addCircles(lng=ulng, lat=ulat, radius=acc, group="pos")
      #addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
      #                         onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    }
  })
  
  ## -------------------------------------------------------------------------------------
  # Leaflet map
  output$myMap = renderLeaflet({

    if(!is.null(APIdata()))
    {
      # Splitting up by review status in order to show reviewed on top
      # notReviewed = APIdata3()[APIdata3()$group == "white",]
      # accepted = APIdata3()[APIdata3()$group == "green",]
      notReviewed = APIdata3()[APIdata3()$group == "white",] %>%
        as_tibble() %>%
        select(subId, lat, lng, date, comName, locName, checklistId, url) %>%
        group_by(comName, checklistId) %>%
        slice_head(n = 1) %>%
        ungroup()
      accepted = APIdata3()[APIdata3()$group == "green",] %>%
        as_tibble() %>%
        select(subId, lat, lng, date, comName, locName, checklistId, url) %>%
        group_by(comName, checklistId) %>%
        slice_head(n = 1) %>%
        ungroup()
      
      # Rendering leaflet map
      leaflet(options = leafletOptions(zoomDelta = 0.25, zoomSnap = 0.25)) %>% 
        addTiles(urlTemplate = custom_map, attribution = mb_attribution) %>%
        addCircleMarkers(group = "Not reviewed", data = notReviewed, 
                         color = "#ffffff", opacity = 0.7, popup = notReviewed$url,
                         label = paste(notReviewed$comName,", ",notReviewed$date, ", ",
                                       notReviewed$locName,sep = "")) %>%
        addCircleMarkers(group = "Accepted", data = accepted, 
                         color = "#00FF33", opacity = 0.7, popup = accepted$url, 
                         label = paste(accepted$comName,", ",accepted$date, ", ", 
                                       accepted$locName, sep = "")) %>%
        addLegend(position = "bottomright", 
                  colors = c("#ffffff", "#00FF33"), 
                  labels = c("Not reviewed", "Accepted"),
                  title = "Review status", opacity = 1) %>%
        addLayersControl(overlayGroups = c("Not reviewed", "Accepted"), position = "bottomright")
      #addEasyButton(easyButton(icon="fa-crosshairs", title="Locate Me",
      #                         onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
      
      
    }
    else
    {
      
      
      # Rendering leaflet map
      return({
        
        leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
          addTiles(urlTemplate = custom_map, attribution = mb_attribution,
                   options = tileOptions(opacity = 0))

        })
    }

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

