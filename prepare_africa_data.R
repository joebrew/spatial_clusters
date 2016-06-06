prepare_africa_data <- function(){
  # Libraries
  library(rgdal)
  library(readr)
  library(dplyr)
  
  # Read in the africa settlements file
  africa <- readOGR('data/african_settlements/', 'afpv1')
  
  # Convert to a dataframe
  africa_df <- data.frame(africa)
  
  # Get the number of points in each country
  africa_df <-
    africa_df %>%
    left_join(africa_df %>%
                group_by(COUNTRY) %>%
                tally(),
              by = 'COUNTRY')
  
  # Keep only those countries with 50 or more points
  africa_df <- 
    africa_df %>%
    filter(n >= 50)
  
  # Create standard latitude / longitude / id named columns
  africa_df$id <- 1:nrow(africa_df)
  africa_df$lat <- africa_df$LATITUDE
  africa_df$lon <- africa_df$LONGITUDE
  
  # Make spatial again
  coordinates(africa_df) <- ~LONGITUDE + LATITUDE
  proj4string(africa_df) <- proj4string(africa)
  
  # Return the dataframe
  return(africa_df)
}