library(shiny)
library(tidyverse)
library(lubridate)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(fontawesome)
# Fix the mutate conflict
mutate <- dplyr::mutate

# ---- Global variables ----

# Not a function, but just a global variable, it's my API key
api_key <- "phvesltiq609"


# ---- Small custom functions ----

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


# ---- Region choices ----

region_descriptors <- vroom("files/region_descriptors.csv", delim = ",")
region_choices <- region_descriptors %>%
  pull(FULL)


# ---- Switch from region name to code ----

# Match region name to region code
region_name_to_code <- function(region_name){
  
  result <- region_descriptors %>%
    filter(FULL == region_name) %>%
    pull(REGION_CODE) %>%
    as.character()
  
  return(result)
}



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


ebd_api_fetch2 <- function(api_key, region_code, back){
  tryCatch({
    
    suppressWarnings({
      
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
      
    })
    
    }, error = function(e) {
      # Handle errors by returning NULL or an empty data frame
      return(NULL)})
}
