library(tidyverse)
library(vroom)


# Read country codes
# https://api.ebird.org/v2/ref/region/list/country/world?key=phvesltiq609&fmt=json
countries <- vroom("files/country_codes.csv")

# Initialize an empty data frame
df <- data.frame()

# Progress bar setup
pb <- progress_estimated(nrow(countries))

# Loop through countries and get sub1 and sub2 region descriptors
for(i in 1:nrow(countries)){
  
  # Update the progress bar
  pb$tick()$print()
  
  # Select country i
  country_code <- countries$REGION_CODE[i]
  
  # Make urls for subnational 1 and 2 levels
  url_start <- "https://api.ebird.org/v2/ref/region/list/subnational"
  url_end <- "?key=phvesltiq609&fmt=csv"
  url_sn1 <- paste0(url_start, "1/", country_code, url_end)
  url_sn2 <- paste0(url_start, "2/", country_code, url_end)
  
  # Try reading the URL with vroom, skip if an error occurs - sub1
  temp_df_1 <- tryCatch({
    vroom(url_sn1, show_col_types = FALSE, delim = ",")
  }, error = function(e) {
    message(paste("Skipping URL due to error for", url_sn1))
    return(NULL)
  })
  
  # Try reading the URL with vroom, skip if an error occurs - sub2
  temp_df_2 <- tryCatch({
    vroom(url_sn2, show_col_types = FALSE, delim = ",")
  }, error = function(e) {
    message(paste("Skipping URL due to error for", url_sn2))
    return(NULL)
  })
  
  # Only rbind if temp_df is not NULL
  if (!is.null(temp_df_1)) {
    
    temp_df_1.2 <- temp_df_1 %>%
      mutate(GREATER = country_code) %>%
      mutate(FULL = paste0(REGION_NAME, ", ", GREATER))
    
    df <- rbind(df, temp_df_1.2)
  }
  if (!is.null(temp_df_2)) {
    
    greater <- paste0()
    
    temp_df_2.2 <- temp_df_2 %>%
      mutate(GREATER = str_extract(REGION_CODE, "(?<=-)[^-]+")) %>%
      mutate(GREATER = paste0(GREATER, ", ", country_code)) %>%
      mutate(FULL = paste0(REGION_NAME, ", ", GREATER))
    
    df <- rbind(df, temp_df_2.2)
  }
  
}

# Output
vroom_write(df, "files/region_descriptors.csv", delim = ",")
