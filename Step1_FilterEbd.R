## This is the first step in the process to update the IBA database with eBird. 
## The eBird database is massive, filtering IBA sites directly from it would take weeks.


## Solution: Filter by state first
##    Break it up into smaller files according to state and apply
##    a filter to only get observations from the last 10 years. 
##
## Input: eBird Database (txt file)
##    F:/eBird/Dec2018Clean/ebd_relDec-2018.txt 
##
## Output: 50 files (1 for each state)
##    G:/IBA_Update/Ebird_Filtering/Outputs/State_Filters/ebd_10yrs_US-STATE.txt

## Before you run this script: You might change the states and year filter. 
##    Check file path is still correct for ebd database. Change output filepath.

## Tip: Run over a weekend... its gonna be awhile :)


library(tidyverse)
library(auk)
library(data.table)

# Set temporary directory
write("TMP = 'G:/Temp'", file=file.path(Sys.getenv('R_USER'), '.Renviron'))

# set working directory and see what's inside
ebd_dir <- "F:/eBird/Dec2018Clean"
setwd(ebd_dir)
dir()

# set ebird ebd count file
# Note: there is another file with sampling info, but for the sake of an IBA update we don't need it.
f_ebd <- file.path(ebd_dir, "ebd_relDec-2018.txt")

# create your filters that will filter the ebd and sampling files
states = c("US-AL","US-AK","US-AZ","US-AR","US-CA","US-CO","US-CT","US-DE","US-FL","US-GA",
           "US-HI","US-ID","US-IL","US-IN","US_IA","US-KS","US-KY","US-LA","US-ME","US-MD",
           "US-MA","US-MI","US-MN","US-MS","US-MO","US-MT","US-NE","US-NV","US-NH","US-NJ",
           "US-NM","US-NY","US-NC","US-ND","US-OH","US-OK","US-OR","US-PA","US-RI","US-SC",
           "US-SD","US-TN","US-TX","US-UT","US-VT","US-VA","US-WA","US-WV","US-WI","US-WY")
i = 1
for (i in 1:length(states)) {
  # define the stuff you want to keep
  target_bbox <- c()
  target_bcr <- c()
  target_country <- c("US")
  target_date <- c("2009-01-01", "2019-01-01") # date beginning, date end
  target_distance <- c()
  target_duration <- c()
  target_extent <- c()
  target_protocol <- c()
  target_species <- c()
  target_state <- c(states[i]) # call your state filter
  target_time <- c()
  
  ebd_filters <- auk_ebd(file = f_ebd, sep = "\t") %>%
    auk_country(country=target_country) %>% 
    auk_date(date=target_date) %>% 
    auk_state(state=target_state) %>%
    auk_complete()
  
  # define the filtered output files and run the filtering step
  f_out_ebd <- paste0("G:/IBA_Update/Ebird_Filtering/Outputs/State_Filters/ebd_10yrs_",states[i],".txt")
  
  ebd_sed_filtered <- auk_filter(ebd_filters, file=f_out_ebd, 
                                 overwrite = TRUE)
}


##################################################### END STEP 1: STATE FILTER ######################################################
##################################################### END STEP 1: STATE FILTER ######################################################
##################################################### END STEP 1: STATE FILTER ######################################################











##################################################### START STEP 2: SITE FILTER #####################################################
##################################################### START STEP 2: SITE FILTER #####################################################
##################################################### START STEP 2: SITE FILTER #####################################################

## This is the second step in the process to update the IBA database with eBird. 
## Now that eBird is a little easier to filter (file sizes are smaller), we can 
## now organize all observations into files according to site ID. 
##
##
## Input: 50 files (1 for each state)
##    G:/IBA_Update/Ebird_Filtering/Outputs/State_Filters/ebd_10yrs_US-STATE.txt
##      
##        
##
## Output: 
##
## Before you run this script: Change filepaths if need be

## Tip: Run over a weekend... its gonna be awhile :)

State <- c("ID","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY",
           "NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT","VA","WA",
           "WV","WI","WY")
i = 1
y = 1

for (i in 1:length(State)){
  # Load and clean GIS Bounding Box Data
  setwd("G:/IBA_Update/Ebird_Filtering/Bbox_StateGroups")
  GIS_Data_Unclean <- read.csv(paste0("BoundingGeometry_",State[i],".csv"))    # CHANGE FOR EACH STATE LOCATION!!!
  GIS_Data_Cleaned <- GIS_Data_Unclean[,-c(1:35)]   # Remove columns we don't need
  
  # Define variables that will be used in ebird bbox filter
  lng_min <- GIS_Data_Cleaned[,1]
  lng_max <- GIS_Data_Cleaned[,3]
  lat_min <- GIS_Data_Cleaned[,2]
  lat_max <- GIS_Data_Cleaned[,4]
  
  
  # set working directory and see what's inside
  ebd_dir <- "G:/IBA_Update/Ebird_Filtering/Outputs/State_Filters"
  setwd(ebd_dir)
  f_ebd <- file.path(ebd_dir, paste0("ebd_10yrs_US-",State[i],".txt"))  # ebird state filtered data: CHANGE FOR EACH STATE!!!!
  
  for (y in 1:nrow(GIS_Data_Cleaned)) {
    # create your filters that will filter the ebd files that have already been filtered by state
    # define the stuff you want to keep
    target_bbox <- c(lng_min[y],lat_min[y],lng_max[y],lat_max[y])
    target_bcr <- c()
    target_country <- c()
    target_date <- c()
    target_distance <- c()
    target_duration <- c()
    target_extent <- c()
    target_protocol <- c()
    target_species <- c()
    target_state <- c()
    target_time <- c()
    
    ebd_filters <- auk_ebd(file = f_ebd, sep = "\t") %>%
      auk_bbox(bbox = target_bbox) %>% 
      auk_complete()
    
    # define the filtered output files and run the filtering step
    f_out_ebd <- paste0("G:/IBA_Update/Ebird_Filtering/Outputs/Bbox_Filtered/",State[i],"_Sites/ebd_10yrs_",State[i],y,".txt")
    
    ebd_sed_filtered <- auk_filter(ebd_filters, file=f_out_ebd, 
                                   overwrite = TRUE)
    
    
    y = y + 1
  }
  
  i = i + 1
} 



##################################################### END STEP 2: SITE FILTER ######################################################
##################################################### END STEP 2: SITE FILTER ######################################################
##################################################### END STEP 2: SITE FILTER ######################################################






########################################### START STEP 3: LABEL FILES BY SITE ID ####################################################
########################################### START STEP 3: LABEL FILES BY SITE ID ####################################################
########################################### START STEP 3: LABEL FILES BY SITE ID ####################################################

## Filepaths for data used to update ebird
# GIS IBA data: G:/IBA_Update/Ebird_Filtering/Bbox_StateGroups
# Ebird entries filtered by site:G: /IBA_Update/Ebird_Filtering/Outputs/Bbox_Filtered/AK_Sites
# IBA data from MS Access: G:/IBA_Update/Ebird_Filtering/IBA_Input


#NOTE: Can't call filenames by site name because of too many special characters so have to do by site ID.
################################## FOR LOOP: STATE LEVEL ##################################

state = c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA","KS","KY","LA","ME","MD",
          "MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR",
          "PA","RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

for (y in 1:length(state)){
  #### READ IN DATA ####
  ## IBA GIS info for each site
  GIS_IBAs <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/Bbox_StateGroups/BoundingGeometry_",state[y],".csv"), header=TRUE, sep=",")
  
  ## IBA Microsoft Access data: Already filtered for A1 sites
  IBA_data <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/IBA_A1_Input.csv", header=TRUE, sep=",")
  
  
  ################################## Make CSVs with Site as Filename ##################################
  
  i = 1
  for (i in 1:nrow(GIS_IBAs)){
    ## species information filtered for each IBA site (ebd output)
    ebd_sites <- fread(paste0("G:/IBA_Update/Ebird_Filtering/Outputs/Bbox_Filtered/",state[y],"_Sites/ebd_10yrs_",state[y],i,".txt"), quote = "")
    
    #### Add row to site_obsv (ebd output) with IBA Site name ####
    SiteName <- as.character(GIS_IBAs[i,4])
    SiteID <- GIS_IBAs[i,2]
    ebd_sites$Site_Name <- as.character(rep(SiteName,nrow(ebd_sites)))
    ebd_sites$Site_ID <- as.double(rep(SiteID,nrow(ebd_sites)))
    
    #### Save the ebd output with site name as the filename ####
    write.csv(ebd_sites, paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/",state[y],"_ebd_sitenames/ebd_",SiteID,".csv"), row.names = TRUE)
    
    i = i + 1
  }
  y = y + 1
}


## Find the ebd output file that corresponds to the site name in a given row in the IBA_data 
y <- as.character(IBA_data$National_name[252])
site_filename <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/ebd_sites/ebd_",y,".csv"))

