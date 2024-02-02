# To include your GBIF credentials in this R session
# 
# library(usethis)
# usethis::edit_r_environ()
# Edit your .Renviron to look like this:
#   
# GBIF_USER="nicolasvanermen"
# GBIF_PWD="************"
# GBIF_EMAIL="nicolas.vanermen@inbo.be"
# 
# Also see: https://docs.ropensci.org/rgbif/articles/gbif_credentials.html

# To get a boundary map of Flanders you need "rnaturalearthhires":
# 
# install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")

library(ggplot2)
library(rgbif)
library(rnaturalearth)
library(sf)
library(mapview)
library(lubridate)
library(stringr)

#Source
source("./Oscibio case study functions.R")

#Execute & test functions

#Define the input variables for download function
species_name <- "Vespa velutina"
selected_species <- name_backbone(str_replace(species_name, " ", "_"))$usageKey
selected_region <- ne_states(country = "Belgium", geounit = "Flemish Region", returnclass = "sf")
start_year <- 2020
end_year <- 2022

#Run the download function
Vespa_velutina_FL <- Download_GBIF_occurrences(selected_species, selected_region, start_year, end_year)

#Tests:
#Test the result graphically:
Flanders <- ne_states(country = "Belgium", geounit = "Flemish Region", returnclass = "sf")
mapview(Vespa_velutina_FL, cex=0.3, col.regions = "black", layer.name='Vespa velutina (GBIF occurrences)') + mapview(Flanders, col.regions = "darkgreen", alpha.regions = 0.1)

#Test the selected species:
unique(Vespa_velutina_FL$species) == species_name

#Test the date range:
sum(ymd(paste(Vespa_velutina_FL$year,Vespa_velutina_FL$month,Vespa_velutina_FL$day)) %within%
      interval(ymd(paste(start_year,"0101")),ymd(paste(end_year,"1231")))) == 
  nrow(Vespa_velutina_FL)

#Test the geographical range:
# Define a bounding box of the selected area
# source:https://geometadatalabs.eu/index.php?title=Land_cover_Belgium_(Flanders)_1m_resolution_2012
North <- 51.49590
East <- 5.92000
South <- 50.67480
West <- 2.56035
#perform test with a 1% relaxation of the geographical limits:
coord <- as.data.frame(st_coordinates(Vespa_velutina_FL))
sum(coord$X < East+0.1 & 
      coord$X > West-0.1 & 
      coord$Y > South-0.1 & 
      coord$Y < North+0.1) == nrow(Vespa_velutina_FL)


#Define the input variables for the graph generator
selected_file <- Vespa_velutina_FL

#Run the graph generator function
List <- Generate_map_and_histogram(Vespa_velutina_FL, selected_region)
Map_Vespa_velutina_FL <- List[[1]]
Map_Vespa_velutina_FL
Map_Vespa_velutina_FL_col_per_yr <- List[[2]]
Map_Vespa_velutina_FL_col_per_yr
Hist_Vespa_velutina_FL <- List[[3]]
Hist_Vespa_velutina_FL