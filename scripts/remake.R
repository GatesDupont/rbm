library(tidyverse)
library(lubridate)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)

# ---- Custom functions ----

# Fix the mutate conflict
mutate <- dplyr::mutate

# Take subID and make hyperlink
create_eBird_link <- function(subid) {
  base_url <- "https://ebird.org/checklist/"
  full_url <- paste0(base_url, subid)
  html_link <- paste0('<a href="', full_url, '">click here</a>')
  return(html_link)
}

# Special icon for leaflet map 
icons <- makeAwesomeIcon(
  text = fa("crow"),
  markerColor = ~color)

# Not a function, but just a global variable, it's my API key
api_key <- "phvesltiq609"


# ---- Example input ----

# The user will be able to adjust these.
# They will be able to select a region code from
#   a list of options called "region_choices
# They will be able to select the back integer
#   based on a slider from 1 to 30

# Won't need these later
region_code <- "US-MA-017"
back <- 3


# ---- API function ----

# Fetch data from the eBird API
ebd_api_fetch <- function(api_key, region_code, back){
  
  # Concatenate the url for the API call
  url <- paste0(
    'https://api.ebird.org/v2/data/obs/', region_code, 
    '/recent/notable?detail=full&key=', api_key, 
    '&back=', back)
  
  # Data: get, scrape, organize, remove duplicated
  result <- url %>%
    # Scrape the data
    readLines(warn = F) %>%
    fromJSON() %>%
    # Organize the data
    as_tibble() %>%
    select(comName, locName, obsDt, lat, lng,
           obsValid, obsReviewed,
           subId) %>%
    # Note the reivew status, keep non-rejects
    mutate(status = case_when(
      obsValid == TRUE & obsReviewed == TRUE ~ "Accepted",
      obsValid == TRUE & obsReviewed == FALSE ~ "Accepted",
      obsValid == FALSE & obsReviewed == TRUE ~ "Rejected",
      obsValid == FALSE & obsReviewed == FALSE ~ "Awaiting review")) %>%
    filter(status != "Rejected") %>%
    select(-obsValid, -obsReviewed) %>%
    # Keep only unique rows
    distinct() %>%
    # Split up the date
    mutate(obsDt = ymd_hm(obsDt)) %>%
    # Add a checklist ID
    arrange(locName) %>%
    arrange(comName) %>%
    group_by(comName, locName, obsDt) %>%
    mutate(listID = cur_group_id()) %>%
    ungroup() %>%
    # Add a bird ID
    group_by(comName, locName) %>%
    mutate(birdID = cur_group_id()) %>%
    ungroup() %>%
    # Record: latest list, number of days, number of records, review status
    group_by(comName, locName, birdID) %>%
    arrange(desc(obsDt)) %>%
    arrange(status) %>%
    arrange(birdID) %>%
    mutate(link = first(subId),
           n_days = 1 + as.numeric(max(as.Date(obsDt)) - min(as.Date(obsDt))),
           n_records = n(),
           bird_status = if_else(
             any(status == "Accepted"), 
             "Accepted",
             "Awaiting review")) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    mutate(color = case_when(
      bird_status == "Accepted" ~ "blue",
      bird_status == "Awaiting review" ~ "lightblue"))
  
  return(result)
  
}


# ---- Leaflet map ----

# Fetch the data from eBird based on the region_code and back
ebd_data_for_map <- ebd_api_fetch(api_key, region_code, back)

# Create the leaflet map
leaflet(ebd_data_for_map, options = leafletOptions(zoomDelta = 0.25)) %>%
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

