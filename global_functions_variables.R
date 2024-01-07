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
# Fix the mutate conflict
mutate <- dplyr::mutate

# ---- Global variables ----

# My API key
api_key <- "phvesltiq609"


# ---- Small custom functions ----

create_eBird_link <- function(subid) {
  base_url <- "https://ebird.org/checklist/"
  full_url <- paste0(base_url, subid)
  html_link <- paste0('<a href="', full_url, '" target="_blank">click here</a>')
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

region_code_to_name <- function(region_code) {
  result <- region_descriptors %>%
    filter(REGION_CODE == region_code) %>%
    pull(FULL) %>%
    as.character()
  
  return(result)
}


# ---- API function ----

# Fetch data from eBird API
ebd_api_fetch <- function(api_key, region_code, back){
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
        dplyr::select(speciesCode, comName, 
               locName, obsDt, lat, lng,
               obsValid, obsReviewed,
               subId) %>%
        # Note the reivew status, keep non-rejects
        mutate(status = case_when(
          obsValid == TRUE & obsReviewed == TRUE ~ "Accepted",
          obsValid == TRUE & obsReviewed == FALSE ~ "Accepted",
          obsValid == FALSE & obsReviewed == TRUE ~ "Rejected",
          obsValid == FALSE & obsReviewed == FALSE ~ "Awaiting review")) %>%
        filter(status != "Rejected") %>%
        dplyr::select(-obsValid, -obsReviewed) %>%
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
               last_seen = format(max(obsDt), "%b. %d, %Y"),
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


# ---- Get taxonomic information ----

# Deprecated this because it was slow
# # Get taxonomic order by using the taxonomy API
# get_taxon_order <- function(api_key, species_codes) {
#   base_url <- "https://api.ebird.org/v2/ref/taxonomy/ebird"
#   
#   # Data frame to store the results
#   results_df <- tibble(species_code = species_codes, taxonOrder = integer(length(species_codes)))
#   
#   for (i in seq_along(species_codes)) {
#     # Prepare the query with the species code and request JSON format
#     query <- list(species = species_codes[i], fmt = "json")
#     
#     # Prepare the header with the API key
#     headers <- add_headers(`X-eBirdApiToken` = api_key)
#     
#     # Make the API request
#     response <- GET(url = base_url, query = query, headers)
#     
#     # Check if the response was successful
#     if (response$status_code == 200) {
#       # Parse the response as JSON
#       species_data <- fromJSON(content(response, "text"), flatten = TRUE)
#       
#       # If the response is not empty, extract taxonOrder
#       if (length(species_data) > 0 && "taxonOrder" %in% names(species_data)) {
#         results_df$taxonOrder[i] <- species_data$taxonOrder
#       } else {
#         # If the response is empty or taxonOrder is not found, set taxonOrder to 0
#         results_df$taxonOrder[i] <- 0
#       }
#     } else {
#       warning(paste("Failed to retrieve data for species code:", species_codes[i]))
#       # Set taxonOrder to 0 if the API call failed
#       results_df$taxonOrder[i] <- 0
#     }
#   }
#   
#   return(results_df)
# }

# Define a function to get taxonomic order for given species codes
get_taxon_order <- function(species_codes){
  
  # Read the taxonomy data from the CSV file and select required columns
  full_taxonomy <- vroom("files/taxonomy.csv") %>%
    select(species_code = SPECIES_CODE, taxon_order = TAXON_ORDER)
  
  # Filter the full taxonomy for the species codes provided as input
  result <- full_taxonomy %>%
    filter(species_code %in% species_codes)
  
  # Identify species codes not present in the taxonomy data
  species_not_in_taxonomy_id <- !(species_codes %in% full_taxonomy$species_code)
  
  # Check if there are any missing species codes
  if (sum(species_not_in_taxonomy_id) > 0) {
    
    # Extract the missing species codes
    missing_species_codes <- species_codes[species_not_in_taxonomy_id]
    
    # Create a dataframe for missing species with taxon_order set to 0
    missing_species_df <- data.frame(species_code = missing_species_codes, 
                                     taxon_order = 0)
    
    # Combine the result with missing species dataframe
    # and arrange by taxon_order
    result <- rbind(result, missing_species_df) %>%
      arrange(taxon_order)
  }
  
  # Return the final result
  return(result)
}


