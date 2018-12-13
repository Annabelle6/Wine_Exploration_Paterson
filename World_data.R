#libraries needed to recreate this code
library(dplyr)
library(tidyverse)
library(ggmap)

#creating the World Map data to use in shiny 

#reading in the csv file created of the world locations that Nz exports
#their wine to 
locations <- read.csv("World.csv", stringsAsFactors = FALSE)

#creating a google API key to be able to get the map coords 
#for use of geocode
register_google(key = "AIzaSyARmTFbYyT98l6XQp0zwCJmbTtL54Oj2cw")

#creating the dots data frame (the cords needed to plot the dots )
dots <- unique(c(locations$From, locations$To))
coords <- geocode(dots)
dots <- data.frame(dot=dots, coords)

#creating a col to order them. 
#1 being country that we export to the most 
dots$order <- seq.int(nrow(dots))

#adding the cords to the location data frame 
#need a start and a end for the line 
#start is always going to be New Zealand 
#end will be other country
locations <- merge(locations, dots, by.x="To", by.y="dot")
locations <- merge(locations, dots, by.x="From", by.y="dot")



#writing the rds file for shiny. This goes to the project file.  
write_rds(dots, "dots_new", compress = "none")
write_rds(locations, "locations_new", compress = "none")
#writing it to my app file 
write_rds(dots, "Paterson_app_final/dots_new", compress = "none")
write_rds(locations, "Paterson_app_final/locations_new", compress = "none")



