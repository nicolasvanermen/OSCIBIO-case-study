require(ggplot2)
require(rgbif)
require(sf)

Download_GBIF_occurrences <- function(species, region, first_year, last_year)
{
download_summary <- occ_download(pred("taxonKey", species), 
                                 pred_gte("year", first_year),
                                 pred_lte("year", last_year),
                                 pred("country", "BE"),
                                 format = "SIMPLE_CSV")

occ_download_wait(download_summary)

Data <- occ_download_get(download_summary) %>% occ_download_import()
Data <- st_as_sf(Data, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

Data_select <- Data %>%
  st_join(region,
          join = st_within, 
          left = FALSE)
return(Data_select)
}


Generate_graphs <- function(file, region)
{
  Map1 <- 
    ggplot(region) + 
    geom_sf(color = "black", fill = "white") + 
    geom_sf(data = file, col = "darkorange2", cex = 0.3) + 
    labs(title = paste(file$species, "(GBIF occurrences)", sep = " ")) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0, size = 10), 
          axis.text = element_text(size = 8))
  
  Map2 <- 
    ggplot(region) + 
    geom_sf(color = "black", fill = "white") + 
    geom_sf(data = file, aes(colour = year), cex = 0.3) + 
    labs(title = paste(file$species, "(GBIF occurrences)", sep = " ")) + 
    theme_bw() + 
    theme(plot.title = element_text(hjust = 0, size = 10), 
          axis.text = element_text(size = 8),
          legend.title = element_blank(),
          legend.text = element_text(size = 8)) + 
    scale_color_continuous(breaks = seq(start_year, end_year, by = 1))
  
  Barplot <- 
    ggplot(file, aes(as.factor(year))) + 
    geom_bar(fill = "darkorange2", color = "black") + 
    theme_bw() +
    labs(title = file$species, y = "Number of GBIF occurrences", x = "") +
    theme(plot.title = element_text(hjust = 0, size = 10), 
          axis.text = element_text(size = 8))
  
  List_of_graphs <- list(Map1, Map2, Barplot)
  
  return(List_of_graphs)
}




