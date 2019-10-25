########################
##  Vacation Places   ##
## Leaflet/Google API ##
########################

# setup libraries and working directory
library(leaflet)
library(mapsapi)
library(tidyverse)
library(readxl)
library(htmltools)

setwd("C:/Users/seewa/OneDrive/Data Ana lysis/Places to Visit")
setwd("C:/Users/Drew/OneDrive/Data Analysis/Places to Visit")

# load excel file with a single column of names
places <- read_excel("places to go.xlsx")
places <- data.frame(places)

G_Auth_Key <- readRDS("G_Auth_Key.rds")

# take names and use mapsapi package to access Google Maps API to get location information for each place
geo_obs <- mp_geocode(addresses = c(places$Location), key = G_Auth_Key)

points <- mp_get_points(geo_obs)

# extract long/lat of each point

points_only <- NULL
for (i in 1:nrow(places)) {
	points_only <- rbind(points_only, points$pnt[[i]])
}

points_only <- as.data.frame(points_only)
names(points_only) <- c("long", "lat")

# extract addresses of each point

addresses_only <- NULL
for(i in 1:nrow(places)) {
	addresses_only <- rbind(addresses_only, points$address_google[[i]])
}

addresses_only <- as.data.frame(addresses_only)
names(addresses_only) <- c("address")

# Add everything back to the original dataframe

places <- cbind(places, points_only, addresses_only)

# Create different icons for each place

getColor <- function(places) {
	sapply(places$Gone, function(Gone) {
	if(Gone == 0) {
		"red"
	} else if(Gone == 1) {
		"blue"
	} else {
		"green"
	} })
}

icons <- awesomeIcons(
	icon = 'fa-map-marker-alt',
	iconColor = 'black',
	library = 'fa',
	markerColor = getColor(places)
)

# build the content popups after all info is concatenated

content <- paste(sep = "", # Tells R what to put between each variable/entry
								 "<b>", places$Location, "</b>", "<br>",
								 places$address
) # Creates variable content which contains all the information that R puts into each popup box

# Make Map Title
title <- tags$div(
	HTML('<h3>Vacation Places Map</h3> <p> <a href="http://www-personal.umd.umich.edu/~atseewal/"> Return to home </a>')
)


#### Create Map ####


# map all places to a leaflet map
vaca_map <- leaflet(places, options = leafletOptions(minZoom = 2)) %>%
	addProviderTiles(providers$OpenStreetMap) %>% #providers$Stamen.TonerLite, group = "Toner Lite" <- bw tiles
	#addMarkers(lng = ~long, lat = ~lat, label = ~Location, group = "places", popup = content, clusterOptions = markerClusterOptions()) %>%
	addAwesomeMarkers(lng = ~long, lat = ~lat, label = ~Location, group = "places", popup = content, icon = icons, clusterOptions = markerClusterOptions()) %>%
	setView(lng = -84.31327, lat = 43.25400, zoom = 4) %>%
	setMaxBounds(lng1 = -180, lat1 = -80, lng2 = 180, lat2 = 90) %>%
	addLegend(position = "bottomright", colors = c("red", "blue", "green"), labels = c("Never Visited", "1 Visited", "Both Visited"), title = "Legend") %>%
	addControl(title, position = 'topright')





#### Doesn't Work ####
# browsable(
# 	tagList(list(tags$head(
# 		tags$meta(
# 			name="viewport", content="width=device-width, height=device-height, initial-scale=1.0, maximum-scale=1.0, user-scalable=no"
# 		)
# 	),
# 		vaca_map
# )))

# Add <style .leaflet .legend {width:125px; height: 75px;} />	
# ADD <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" /> to the head of the exported html for things to be readable on mobile.

##### To Do List #####

# Legend/Controls to turn on/off each layer of "been."

# use map data to add in driving time to each location, add into popup bubbles

# use rvest to create descriptions, numbers, websites for each place, add into popup bubbles

# Title Box

# Use JSON's because they are easier to work with in R