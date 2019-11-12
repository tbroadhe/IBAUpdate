library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(base)

# This script created IBA_pre2008.csv (all original information). 
# Now, we need to create a whole new csv with only ebird obs.
# Below, we search each IBA site in ebird. From there, we see if ANY ebird 
# species listed for that IBA site qualify as IBA 
# species. Testing on A1 and A4, if the species is on the redlist, 
# meets vulnerable threshold, or meets population requirements,
# it will be copied onto IBA_post2009.csv.


# ## Organize Pre-2008 File
# IBA <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/Species_Sites_Counts.csv") # IBA sites with species information
# IBA$Assessment_Dt <-  year(as.Date(IBA$Assessment_Dt, format = "%m/%d/%Y")) # Standardize Dates
# write.csv(IBA, "G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv")

# # Load in IBA and just get a list of sites and their corresponding states
# IBA <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv")
# cond1 <- grep("^A",IBA[,8]) # Select all A criteria. Make sure the column number is correct!!
# IBA <- IBA[cond1,]
# IBA <- IBA[ ,c("Site_ID", "Abbreviation")]
# IBA <- IBA[!duplicated(IBA$Site_ID),]

############## Load in IUCN/GIS data and calculate GIS half length ############## 

Criteria <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/IBA_Criteria_US_2018.csv", header=TRUE, sep=",") # BLI Data
IUCN <- read.csv("G:/IBA_Update/Ebird_Filtering/IUCN_Names_Category.csv", header=TRUE, sep=",")
  IUCN <- IUCN[!duplicated(IUCN[2:3]),]
# GIS <- read.csv("G:/IBA_Update/Ebird_Filtering/GIS_BBOX_Lengths.csv")
# GIS$MBG_Length <- GIS$MBG_Length/1000
# GIS["Half_Length"] <- 0
# GIS$Half_Length <- GIS$MBG_Length/2


## Create IBA post 2009 csv to be populated 
IBA_post2009 <- setNames(data.frame(matrix(ncol = 9, nrow = 0)), c("Common_Name","Site_ID","Abbreviation","Date",
                                                                   "Count","Code","RedList_Category","Threshold","GlobalPop"))

# Function to search each ebd_species on the IUCN Redlist
IUCN_Search <- function(spp_name){
  Category <- IUCN[which(IUCN$Common_Name == toString(spp_name)), "RedList_Category"]
  return (toString(factor(Category)))
}

threshold <- function(spp_name){
  threshold_value <- Criteria[which(tolower(Criteria$Common.name) == toString(tolower(spp_name))), "A1_Thresholds"]
  return (toString(factor(threshold_value)))
}

globalpop <- function(spp_name){
  pop_value <- Criteria[which(tolower(Criteria$Common.name) == toString(tolower(spp_name))), "OnePercent_GlobalPop"]
  return (toString(factor(pop_value))) 
}

RangeRestricted <- function(spp_name){
  RR <- Criteria[which(tolower(Criteria$Common.name) == toString(tolower(spp_name))), "RRspp"]
  return (toString(factor(RR)))
}

BiomeRestricted <- function(spp_name){
  BR_thresh <- Criteria[which(tolower(Criteria$Common.name) == toString(tolower(spp_name))), "Biome_NationalThreshold"]
  return (toString(factor(BR_thresh)))
}

Biomefind <- function(spp_name){
  Biome_name <- Criteria[which(tolower(Criteria$Common.name) == toString(tolower(spp_name))), "Biome"]
  return (toString(factor(Biome_name)))
}


#######################################

   
state = c("AK","AL","AZ","AR","CA","CO","CT","DE","FL","GA","HI",
          "ID","IL","IN","IA","KS","KY","LA","ME","MD","MA","MI",
          "MN","MS","MO","MT","NE","NV","NH","NJ","NM","NY","NC",
          "ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT",
          "VT","VA","WA","WV","WI","WY")

for (y in 1:length((state))){
  dir <- paste0("G:/IBA_Update/Ebird_Filtering/ebd_intersected_sites/",state[y],"_ebd_intersected_sites/")
  setwd(dir)
  files <- list.files(dir, pattern = ".csv")


  for (i in 1:length(files)){
    ebd <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/ebd_intersected_sites/",state[y],"_ebd_intersected_sites/",files[i]))
    # ebd <- read.csv(paste0("G:/IBA_Update/Ebird_Filtering/ebd_intersected_sites/AK_ebd_intersected_sites/intersect_989.csv"))
    
    # For some reason all col names are wrong: rename them
    colnames(ebd)[colnames(ebd)=="CATEGORY"] <- "COMMON.NAME" 
    colnames(ebd)[colnames(ebd)=="SUBSPECIES.SCIENTIFIC.NAME"] <- "OBSERVATION.COUNT" 
    colnames(ebd)[colnames(ebd)=="COUNTRY.CODE"] <- "STATE" 
    colnames(ebd)[colnames(ebd)=="LOCALITY.TYPE"] <- "OBSERVATION.DATE" 
    colnames(ebd)[colnames(ebd)=="SAMPLING.EVENT.IDENTIFIER"] <- "PROTOCOL.TYPE" 
    colnames(ebd)[colnames(ebd)=="DURATION.MINUTES"] <- "EFFORT.DISTANCE.KM"
    colnames(ebd)[colnames(ebd)=="V47"] <- "Site_Name" 
    colnames(ebd)[colnames(ebd)=="Site_Name"] <- "Site_ID" 
    ebd = ebd[,c(6,10,16,27,31,35,47,48)]
    
    # Get the spp observation with highest observed count
    ebd$OBSERVATION.COUNT <- tolower(ebd$OBSERVATION.COUNT)
    ebd$OBSERVATION.COUNT[ebd$OBSERVATION.COUNT == "x"] <- 1    # Sub Xs for 1s
    ebd$EFFORT.DISTANCE.KM[is.na(ebd$EFFORT.DISTANCE.KM)] <- 0
    ebd$OBSERVATION.COUNT <- as.numeric(ebd$OBSERVATION.COUNT)
    ind = order(ebd$COMMON.NAME, ebd$OBSERVATION.COUNT, decreasing=TRUE)     # Get row for species with highest count
    ebd = ebd[ind,]
    ebd = ebd[!duplicated(ebd$COMMON.NAME),]

    # if file is empty, move on
    spp_name <- as.character(ebd$COMMON.NAME)
    if (is_empty(spp_name)){
      next
    }
    
    # Get info from other csv sheets for each species
    ebd$RedList <- sapply(spp_name, IUCN_Search)
    ebd$GlobalPop <- sapply(spp_name, globalpop)
    ebd$Threshold <- sapply(spp_name, threshold)
    ebd$RangeRestrict <- sapply(spp_name, RangeRestricted)
    ebd$Biome <- sapply(spp_name, Biomefind)
    ebd$BiomeRestrict <- sapply(spp_name, BiomeRestricted)
    ebd$Threshold <- as.numeric(as.character(ebd$Threshold))
    ebd$GlobalPop <- as.numeric(as.character(ebd$GlobalPop))
    ebd$OBSERVATION.COUNT <- as.numeric(as.character(ebd$OBSERVATION.COUNT))
    ebd$BiomeRestrict <- as.numeric(as.character(ebd$BiomeRestrict))
    

############################## APPLY ALL THE A CRITERIA TESTS ##############################
# A1, A2, A3, A4 made separately then combined so if a species belongs to two or more, it will get two lines
    
    # A1
    # If endangered or critically endangered or if vulnerable and if count > threshold, it gets A1 label
    ebdA1 <- subset(ebd)
    ebdA1$Code <- "A1"
    ebdA1 <- subset(ebdA1, RedList == 'Endangered'  | (RedList == 'Vulnerable' & OBSERVATION.COUNT >= Threshold )
                    | RedList == 'Critically Endangered')
    
    
    #A2
    # If a site has at least two RR spp (Y), and one has count > 1% global pop, it is A2
    ebdA2 <- subset(ebd)
    ebdA2$Code <- "A2"
    ebdA2$RangeRestrict[ebdA2$RangeRestrict == ""] <- NA
    ebdA2 <- subset(ebdA2, ebdA2$RangeRestrict == "Y")
    ebdA2 <- subset(ebdA2,!is.na(ebdA2$OBSERVATION.COUNT))
    ebdA2 <- subset(ebdA2,!is.na(ebdA2$GlobalPop))
    ebdA2 <- subset(ebdA2,(nrow(ebdA2)>=2 & any(ebdA2$OBSERVATION.COUNT >= ebdA2$GlobalPop)))
    # if (nrow(ebdA2) >=2){
    #   if (any(ebdA2$OBSERVATION.COUNT >= ebdA2$GlobalPop)){
    #   ebdA2 <- ebdA2
    #   }
    # }else{
    #   ebdA2 <- ebdA2[0,]
    # }
    
    
    # A3
    # If the number of species in a particular biome exceed the threshold number of species needed (BiomeRestrict), it is A3
    ebdA3 <- subset(ebd)
    ebdA3$Code <- "A3"
    ebdA3$Biome[ebdA3$Biome == ""] <- NA
    ebdA3 <- subset(ebdA3,!is.na(ebdA3$Biome))
    ebdA3 <- add_count(ebdA3, ebdA3$Biome)
    ebdA3 <- subset(ebdA3, ebdA3$n >= ebdA3$BiomeRestrict)
    ebdA3 <- ebdA3[,-c(16,17)]
    
    
    # A4
    # If count > 1% Global Population, it gets A4 label
    ebdA4 <- subset(ebd)
    ebdA4$Code <- "A4"
    ebdA4 <- subset(ebdA4, OBSERVATION.COUNT >= GlobalPop)

    
    # Combine all results
    combined <- bind_rows(ebdA1, ebdA3, ebdA2, ebdA4)
    IBA_post2009 <- rbind(IBA_post2009, combined)
  
  }

}

write.csv(IBA_post2009, "G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_Intersected.csv")
