# Libraries
library(leaflet) ### check here https://leaflet-extras.github.io/leaflet-providers/preview/
##for more tiles options
library(ggmap)
library(raster)
library(RColorBrewer)
library(htmlwidgets)

# Get canada shapefile
moz3 <- getData('GADM', country = 'MOZ', level = 3)

# Subset to just include area of Manhica and Magude
man <- moz3[moz3@data$NAME_1 == 'Maputo', ]

# cols
cols0 <- colorRampPalette(brewer.pal(9, 'Spectral'))(nrow(man))
### This functions builds a vector of colors from any combination : USEFUL!!!!!!
### So that every area is a dif color.

# Geocode our location address


meetup <- geocode(location = 'Centro de Investigação em Saude de Manhiça',
                  source = 'google') ### SUPER USEFUL!!!!

m <- leaflet(man) %>%
        addProviderTiles("Esri.DeLorme") %>%
   # addProviderTiles("OpenStreetMap.BlackAndWhite") %>%
  # addProviderTiles('Stamen.Watercolor') %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.9, smoothFactor = 0.5,
    # color = ~colorQuantile("YlOrRd", moz$ID_1)(ID_1)
    color = cols0) %>%
  addMarkers(lng = meetup$lon,
             lat = meetup$lat,
             popup = 'Here we are')

## Create a vector of places
places<- c("Game", "Praça da OMM")
types<-c("supermarket", "square")
places<-paste0(places,", Mozambique")

geocoded_places<-geocode(location=places,source="google")

ex<-leaflet() %>%
        addProviderTiles("OpenStreetMap.HOT") %>%
        # addMarkers(lng=geocoded_places$lon, lat=geocoded_places$lat,
        #            popup=types)
        

# Save the widget to an html file
saveWidget(m, file="~/Desktop/map_which_you_can_put_in_iframe_if_u_want.html")