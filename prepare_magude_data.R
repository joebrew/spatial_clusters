prepare_magude_data <- function(){
  
  # Packages 
  library(xtable)
  library(knitr)
  library(Hmisc)
  library(raster)
  library(maptools)
  library(rgeos)
  library(plyr)
  library(dplyr)
  library(sp)
  library(readr)
  
  # # # Get a spatialpolygonsdataframe of mozambique
  # moz <- getData('GADM', country = 'MOZ', level = 2)
  # 
  # # Subset to just include manhica
  # # man <- moz[moz@data$NAME_2 == 'Manhiça', ]
  # mag <- moz[moz@data$NAME_2 == 'Magude', ]
  # # Visualize
  # plot(mag)
  
  ## FAKE DATA
  # # Create a dataframe named locations for storing our points in
  # locations <- data.frame(id = 1:1000, 
  #                         lat = NA,
  #                         lon = NA)
  
  ## REAL DATA
  # Read in bednets data
  if (Sys.info()["user"] == "Lucia") {
    real_data <- '/Users/Lucia/Dropbox/MALTEM Entomology/8. Scientific Studies/3. VECE- Efficacy/4. sample_size_house_selection/House selection bednets/updated_db/households_list_reshaped_2016-04-24.csv'
    
    
  } else {
    real_data <- '/media/joebrew/TOSHIBA/households_list_reshaped_2016-04-24.csv'
    
  }
  
  locations <- read_csv(real_data)
  
  # Create columns which are named appropriately for use with our funciton
  locations$lat <- locations$LAT
  locations$long <- locations$LNG
  locations$id <- locations$FAMILY_ID
  
  # # Get the range for Magude
  # bb <- bbox(mag)
  # 
  # # Populate locations
  # for (i in 1:nrow(locations)){
  #   
  #   # Create a placeholder to indicate whether our point is in maghica
  #   in_mag <- FALSE
  #   
  #   # If not in maghica, create a point in maghica
  #   while(!in_mag){
  #     # Create a point
  #     lon <- sample(seq(bb[1,1],
  #                       bb[1,2],
  #                       length = 10000),
  #                   1)
  #     lat <- sample(seq(bb[2,1],
  #                       bb[2,2],
  #                       length = 10000),
  #                   1)
  #     # Put that point into a spatial dataframe
  #     the_point <- data.frame(lon = lon,
  #                             lat = lat)
  #     coordinates(the_point) <- ~lon+lat
  #     proj4string(the_point) <- proj4string(mag)
  #     
  #     # Check to see if it's in the boundaries of maghica
  #     in_mag <- as.logical(!is.na(over(mag, the_point)))
  #   }
  #   # having confirmed that the point is in maghica, 
  #   # pop it into the dataframe
  #   locations$lon[i] <- lon
  #   locations$lat[i] <- lat
  # }
  
  # Make locations a spatial points dataframe and plot
  ## We do this because otherwise lat and lon will not be columns any more after transforming into a geo-point data set.
  # locations$x <- locations$lon
  # locations$y <- locations$lat
  # locations <- data.frame(locations)
  # # Remove any NA points
  # locations<-locations[-which(is.na(locations$lat)),]
  # coordinates(locations) <- ~x +y
  # proj4string(locations) <- proj4string(mag)
  # plot(mag)
  # points(locations, 
  #        col = adjustcolor('red', alpha.f = 0.3), 
  #        pch = 3)
  
  # Return the dataframe
  return(locations)
}

