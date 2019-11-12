library(sf)
library(dplyr)
library(data.table)
library(tidyverse)

## Find observation points from eBird that lie inside IBA polygons
## Remove all observations recorded outside IBA polygons


# Reading and cleaning data
polygons <- st_read("G:/IBA_Update/GIS/Bounding/US_IBAs_2018Feb.shp")
  polygons$STATE_DB <- gsub("US-","",as.character(polygons$STATE_DB)) # state abbreviation. remove US-

# loop: grab the right file that corresponds with the state and site_ID of a polygon
for (i in 1:nrow(polygons)){
  poly1 <- polygons[i,]
  state <- poly1$STATE_DB[1]
  siteID <- poly1$SITE_ID[1]
  ebd_site_dir <- paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/",state,"_ebd_sitenames/")
  site_files <- list.files(ebd_site_dir)
  site_filename <- as.character(paste0("ebd_",siteID,".csv"))

  if (site_filename %in% site_files){
    site <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/",state,"_ebd_sitenames/ebd_",siteID,".csv"))
    # site <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/ID_ebd_sitenames/ebd_3.csv"))
    
    if (site[1,2] == "GLOBAL UNIQUE IDENTIFIER"){
      next
    }
  
  # Convert points df to sf object then transform to same crs code as poly1
  site <- st_as_sf(site, coords = c("LONGITUDE","LATITUDE"), crs = 4326)
  site <- st_transform(site, crs=3857)

  # intersect each eBird observation with IBA site, filter out those that don't intersect
  pointss <- site %>% 
    filter(st_intersects(x=.,y=poly1, sparse=FALSE))

write.csv(pointss, 
          paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/",state,"_ebd_sitenames/intersect_",siteID,".csv"))

}
}
