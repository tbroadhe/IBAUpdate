## This is the first step in the process to update the IBA database with eBird. 
## Figure out how many IBAs we can/can't update using eBird,


## Solution: Compare number of unique sites in each criteria category in pre2008 vs post 2009.
##
## Input: IBA access database of sites with species info and with criteria info only (no spp info), 
##        and the IBA_post 2009 csv created in step 2.
##    G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_intersected.csv
##    G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv (IBA in access database with spp info)
##    G:/IBA_Update/Ebird_Filtering/IBA_Input/Site_Status_Criteria.csv (IBAs that have a criteria code but no spp info)
##
## Output: Figures
## Before you run this script: Check files and file paths




library(plyr)
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyverse)
library(base)



###############################  How Many Global IBAs in the IBA Access Database? ###################################
#####################################################################################################################
#####################################################################################################################
## In the pre2008 dataframe, we want all IBA sites (with and without species information) that are already 
## classified as global IBAs. We do not want IBAs that have a rejected, delisted, or merged status 
## (these aren't in the GIS data that was used to filter eBird by site anyway).

## Solution: 1. Make one pre 2008 dataframe that contains all verified global IBAs (those with and without spp info).
##           Number of unique site IDs = number of Global IBAs in IBA database
##           2. Exclude the rejected, delisted, merged sites and get unique site ID for each criteria code.
##           Number of unique site IDs per criteria code = number of Global IBAs in each criteria code.
##           Important: If you add up the number of sites in each criteria code it will be greater than
##           the total number of global IBAs BECAUSE some sites have more than one criteria code. 


# Get and clean IBA data with species info:
pre2008 <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv", header=TRUE, sep=",") 
  cond1 <- grep("^A",pre2008[,8]) # Select all A criteria
  pre2008 <- pre2008[cond1,]
  pre2008$Code <- as.character(pre2008$Code)
  pre2008$dbo_Status_Description <- as.character(pre2008$dbo_Status_Description)
  pre2008$Code[pre2008$Code == "A4i"] <- "A4"
  pre2008$Code[pre2008$Code == "A4ii"] <- "A4"
  pre2008$Code[pre2008$Code == "A4iii"] <- "A4"
  pre2008$Code[pre2008$Code == "A4iv"] <- "A4"
  pre2008 <- pre2008[c(3,7,8)]
  colnames(pre2008)[colnames(pre2008)=="dbo_Status_Description"] <- "Status"
  colnames(pre2008)[colnames(pre2008)=="Code"] <- "Old_Code"

# Get and clean IBA data that has criteria codes but no species info:
sitecriteria <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/Site_Status_Criteria.csv", header=TRUE,sep=",") 
  cond1 <- grep("^A",sitecriteria[,3]) # Select all A criteria
  sitecriteria <- sitecriteria[cond1,]
  sitecriteria$Code <- as.character(sitecriteria$Code)
  sitecriteria$Code[sitecriteria$Code == "A4i"] <- "A4"
  sitecriteria$Code[sitecriteria$Code == "A4ii"] <- "A4"
  sitecriteria$Code[sitecriteria$Code == "A4iii"] <- "A4"
  sitecriteria$Code[sitecriteria$Code == "A4iv"] <- "A4"
  colnames(sitecriteria)[colnames(sitecriteria)=="Code"] <- "Old_Code"
  colnames(sitecriteria)[colnames(sitecriteria)=="Description"] <- "Status"

# Now we are ready to merge sitecriteria (IBAs with only criteria info) and pre2008 (IBA with species info)
# This is the dataframe that contains all verified global IBAs with no sites that have been rejected, merged, or delisted
# This is the dataframe we will just refer to as pre2008 and use in our comparisons to eBird (post2009) data
pre2008 <- rbind(pre2008, sitecriteria) #combo of sites with and without species information


# Q1: What is the status of all global sites in the IBA database (with and without spp info) 
pre2008_status <- pre2008[!duplicated(pre2008[c(1,3)]),] # Remove duplicated values in site and criteria column: sites have more than one criteria code
  pre2008_status <- pre2008_status[,-c(1)]
  pre2008_status <- table(cbind(pre2008_status[1], Status = unlist(pre2008_status[-1],use.names=FALSE)))
  pre2008_status <- data.frame(pre2008_status)
  colnames(pre2008_status)[colnames(pre2008_status)=="Freq"] <- "Counts"
  colnames(pre2008_status)[colnames(pre2008_status)=="Status.1"] <- "Code"

# ANSWER: 626 Recognized A1, 20 recognized A2, 57 recognized A3, and 372 recognized A4 
ggplot(pre2008_status, aes(x=Code,y=Counts, fill = Status)) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + 
  geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.25) +
  # scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  # scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  xlab("Criteria Code") + ylab("Number of Unique Sites") + 
  ggtitle("All Global IBAs: Status and Criteria Codes") +
  theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))



# Q2: How many total unique global IBA sites (excluding delisted, rejected, merged) are in the IBA access database?
pre2008 <- subset(pre2008, !Status=="Delisted" & !Status=="Rejected" & 
                    !Status=="Merged") # Remove the IBAs that were rejected, delisted, and merged so we only get those that are accepted, potential, pending, etc.

pre2008_sites <- pre2008[!duplicated(pre2008[1]),] # Remove all duplicated values in site column: just want to know how many unique (not by criteria)
length(unique(pre2008$Site_ID)) # ANSWER: There are 1170 total unique sites in IBA database with global criteria


  

# Q3: How many global IBA sites (excluding delisted, rejected, merged) are in each criteria code in the IBA access database?
pre2008_criteria <- pre2008[!duplicated(pre2008[c(1,3)]),] # Remove duplicated values in site and criteria column: sites have more than one criteria code
  
  # Reshape data to plot
  df <- pre2008_criteria %>%
    group_by(Old_Code) %>%
    summarise(counts=n())
  df
  
# ANSWER: 740 in A1, 22 in A2, 64 in A3, and 582 in A4 **THIS IS MOST IMPORTANT COUNT FOR PRE2008** 
ggplot(df, aes(x=Old_Code, y = counts)) +
  geom_bar(fill = "#0073C2FF", stat = "identity") +
  geom_text(aes(label = counts), vjust = -0.3) +
  xlab("Status") + ylab("Number of Sites") + ggtitle("IBA Database: Number of Global Sites in Each Criteria Code") +
  theme(plot.title = element_text(size = 16, face = "bold"), axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8)) +
  theme(plot.title = element_text(hjust = 0.5))
    




#############################  How Many IBAs can we Update with eBird Species Data? ################################# 
#####################################################################################################################
#####################################################################################################################
## Compare the pre2008_criteria sheet created in the step above to the post2009_intersected.csv created from finding
## every species that meets global requirements observed at a site in eBird. 

## Solution: Make one post 2009 dataframe that only contains eBird observations from sites that are verified 
##           global IBAs (i.e. they are assigned A criteria in the IBA database - these went through committee approval).


# Get the eBird data: every species found at each site (intersected with polygons). 
post2009 <- read.csv("G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_intersected.csv", header=TRUE, sep=",") # Made in SearchEbd.R

# TRAVELING DISTANCE FILTER: REMOVE THOSE GREATER THAN 1 KM OR 3 KM!!!!
# post2009 <- subset(post2009, post2009$EFFORT.DISTANCE.KM<=3.0)
post2009 <- subset(post2009, post2009$EFFORT.DISTANCE.KM<=1.0)

# Get the eBird data that only includes IBAs that have been verified as A criteria (they are A criteria in the IBA db).
colnames(post2009)[colnames(post2009)=="Site_ID"] <- "Site_Name"
colnames(post2009)[colnames(post2009)=="Site_Name.1"] <- "Site_ID"
post2009_verified <- merge(pre2008_criteria, post2009, by="Site_ID") # 708 sites in post2009 that are in pre2008

# number of sites summary for post2009 at a 1 km traveling distance filter
length(unique(post2009$Site_ID)) # There are 1824 unique sites with global qualifying spp on eBird (global, state, continental included)
length(unique(post2009_verified$Site_ID)) # there are 708 unique sites with A spp found on ebird (sites that were already designated global IBA)
newsites <- data.frame(anti_join(post2009, pre2008_criteria, by="Site_ID")) # sites in post 2009 that aren't in pre2008
length(unique(newsites$Site_ID)) # There are 1116 unique sites in ebird that are not listed in IBA database



###### How many sites can/cannot be updated? ######
# Sites that can't be updated will be those that are in pre2008, but not post 2009
# Sites that can be updated will be those that are in both pre2008 and post 2009

# Put label next to each pre2008 site that indicates whether it can be updated (has species info in post 2009-ebird data)
# or cannot be updated (the site ID from an IBA database site is not found in the ebird search results-post2009)
UpdateStatus <- pre2008_criteria
UpdateStatus <- UpdateStatus[!duplicated(UpdateStatus[1]),] # has 1170 rows - number of unique global sites in IBA DB
for (i in 1:nrow(UpdateStatus)){
  if(UpdateStatus$Site_ID[i] %in% post2009_verified$Site_ID){
    UpdateStatus$status[i] <- "Can be Updated"
  } else {
    UpdateStatus$status[i] <- "Cannot be Updated"
  }
}

# # Create dataframe that has site ID, old code (pre2008), and label of can/cannot update
# A unique site either can or cannot be updated (not both). Removing duplicated site ID rows tells how many 
# unique sites that were verified global IBAs have species that can be found in ebird

CanUpdate <- sum(UpdateStatus$status == "Can be Updated")
CannotUpdate <- sum(UpdateStatus$status == "Cannot be Updated")
UpdateStatus$rowdummy <- 1

# Stacked bar plot of all unique sites that either can or cannot be updated using ebird
ggplot(UpdateStatus, aes(x=UpdateStatus$rowdummy, frequency(UpdateStatus$status), 
                                fill = UpdateStatus$status)) + 
  xlab("Dummy Variable") + ylab("Number of Unique Sites") + ggtitle("Update Status of all A listed IBAs") +
  theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x=element_blank(),
        axis.title.y = element_text(size = 14)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(title="Status"))+
  theme(plot.title = element_text(hjust = 0.5))

# Side by side bar graph of all sites that can/cannot be updated using eBird
  # Reshape Data
  UpdateStatus_Table <- table(UpdateStatus$status)
  UpdateStatus_Table <- data.frame(UpdateStatus_Table)
  colnames(UpdateStatus_Table)[colnames(UpdateStatus_Table)=="Freq"] <- "Counts"
  colnames(UpdateStatus_Table)[colnames(UpdateStatus_Table)=="Var1"] <- "Status"

# Q4: How many global IBAs can/cannot be updated with eBird species data?
# ANSWER: 708/1170 CAN be updated, 462/1170 CANNOT be updated!
#         Counts add up to the total number of global IBAs in the IBA Database - 1170
ggplot(UpdateStatus_Table, aes(x=Status,y=Counts, fill = Status)) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + 
  geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.25) +
  scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  xlab("Status") + ylab("Number of Unique Sites") + 
  ggtitle("Update Status of All Global IBAs") +
  theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14))






#########################  How Many Continental/State IBAs Qualify for Global Criteria? ############################## 
#####################################################################################################################
#####################################################################################################################
## Find total number of IBAs (those with global, continental, and state criteria) that can qualify for global status.

## Solution: 1. Use the IBA_post2009_intersected.csv we created in step 2. This took all IBAs (global, continental, and state)
##           and returned species that met global criteria. Therefore, IBA_post2009_intersected.csv contains all IBAs
##           that can qualify for global status. We look at the new designation (criteria species from eBird) in post2009
##           and compare to pre2008 to get number for how many sites in each category qualify for global status. So ultimately,
##           this approach tells us how many new Global IBAs we found.



# At a 1 KM filter for traveling distance, we learned that there are 1824 unique sites found on eBird that have
# species meeting requirements for global IBAs, 708 of which were previously designated global IBAs and 1116
# that were not previously designated global status (state, continental).
# NOte to me: there can be no more than 1116 total state and continental IBAs that qualify for global status.


# Make a dataframe of ALL IBAs (with and without spp info, state, continental, global status)
  # Get and clean IBA data with species info:
  pre2008 <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv", header=TRUE, sep=",") 
  pre2008$Code <- as.character(pre2008$Code)
  pre2008$dbo_Status_Description <- as.character(pre2008$dbo_Status_Description)
    pre2008$Code[pre2008$Code == "A4i"] <- "A4"
    pre2008$Code[pre2008$Code == "A4ii"] <- "A4"
    pre2008$Code[pre2008$Code == "A4iii"] <- "A4"
    pre2008$Code[pre2008$Code == "A4iv"] <- "A4"
    pre2008$Code[pre2008$Code == "D4i"] <- "D4"
    pre2008$Code[pre2008$Code == "D4ii"] <- "D4"
    pre2008$Code[pre2008$Code == "D4iii"] <- "D4"
    pre2008$Code[pre2008$Code == "D4iv"] <- "D4"
    pre2008$Code[pre2008$Code == "D4v"] <- "D4"
    pre2008$Code[pre2008$Code == "D4vi"] <- "D4"
    pre2008$Code[pre2008$Code == "D4vii"] <- "D4"
    pre2008$Code[pre2008$Code == "B4i"] <- "B4"
    pre2008$Code[pre2008$Code == "B4ii"] <- "B4"
    pre2008$Code[pre2008$Code == "B4iv"] <- "B4"
  pre2008 <- pre2008[c(3,7,8)]
  colnames(pre2008)[colnames(pre2008)=="dbo_Status_Description"] <- "Status"
  colnames(pre2008)[colnames(pre2008)=="Code"] <- "Old_Code"
  
  # Get and clean IBA data that has criteria codes but no species info:
  sitecriteria <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/Site_Status_Criteria.csv", header=TRUE,sep=",") 
  sitecriteria$Code <- as.character(sitecriteria$Code)
    sitecriteria$Code[sitecriteria$Code == "A4i"] <- "A4"
    sitecriteria$Code[sitecriteria$Code == "A4ii"] <- "A4"
    sitecriteria$Code[sitecriteria$Code == "A4iii"] <- "A4"
    sitecriteria$Code[sitecriteria$Code == "A4iv"] <- "A4"
    sitecriteria$Code[sitecriteria$Code == "D4i"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4ii"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4iii"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4iv"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4v"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4vi"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "D4vii"] <- "D4"
    sitecriteria$Code[sitecriteria$Code == "B4i"] <- "B4"
    sitecriteria$Code[sitecriteria$Code == "B4ii"] <- "B4"
    sitecriteria$Code[sitecriteria$Code == "B4iv"] <- "B4"
  colnames(sitecriteria)[colnames(sitecriteria)=="Code"] <- "Old_Code"
  colnames(sitecriteria)[colnames(sitecriteria)=="Description"] <- "Status"


# Now we are ready to merge sitecriteria (IBAs with only criteria info) and pre2008 (IBA with species info)
# This is the dataframe that contains all verified global IBAs with no sites that have been rejected, merged, or delisted
# This is the dataframe we will just refer to as pre2008 and use in our comparisons to eBird (post2009) data
pre2008 <- rbind(pre2008, sitecriteria) #combo of sites with and without species information
  pre2008 <- pre2008[!duplicated(pre2008[c(1,3)]),] # Get unique site for each criteria

  # Remove IBAs with rejected, delisted, merged status
  pre2008 <- subset(pre2008, !Status=="Delisted" & !Status=="Rejected" & 
                      !Status=="Merged") 
  

# Q5: How many unique sites in global, continental, state criteria? 
# ANSWER: 2652 total, 1170 Global, 542 Continental, 2262 State (some sites have more than one criteria so if you add these up it is greater than the total - 3975)
length(unique(pre2008$Site_ID)) # 2652 UNIQUE sites total in the IBA database (global, continental, and state)
Global <- subset(pre2008, pre2008$Old_Code == "A1"|pre2008$Old_Code =="A2"|pre2008$Old_Code =="A3"|pre2008$Old_Code =="A4")
length(unique(Global$Site_ID)) # 1170 unique Global Sites
Continental <- subset(pre2008, pre2008$Old_Code == "B1"|pre2008$Old_Code =="B2"|pre2008$Old_Code =="B3"|pre2008$Old_Code =="B4")
length(unique(Continental$Site_ID)) # 542 Continental
State <- subset(pre2008, pre2008$Old_Code == "D1"|pre2008$Old_Code =="D2"|pre2008$Old_Code =="D3"|pre2008$Old_Code =="D4"|pre2008$Old_Code=="D5")
length(unique(State$Site_ID)) # 2262 State
pre2008_table <- data.frame(table(pre2008$Old_Code)) # number of sites in each criteria code: note some have multiple criteria


# Q6: How many Continental and State IBAs have global species found on eBird?
  # Solution: make a dataframe with continental,state,global labels then label if it is in eBird results
  # ANSWER: 360/542 Continental have global spp, 1461/2262 State IBA have global spp, 708/1170 Global IBAs have global spp
  Continental <- Continental[!duplicated(Continental[c(1)]),] # Unique sites in continental: need to get number of cont. sites with global ebird species
  Continental$Criteria <- "Continental"
  Continental <- Continental[,-c(2,3)]
  
  State <- State[!duplicated(State[1]),]
  State$Criteria <- "State"
  State <- State[,-c(2,3)]
  
  Global <- Global[!duplicated(Global[c(1)]),] 
  Global$Criteria <- "Global"
  Global <- Global[,-c(2,3)]

  pre2008_labeled <- rbind(Global, Continental, State)
  
  # Read in post2009 ebird results again and format:
  # every species found at each site (intersected with polygons). 
  post2009 <- read.csv("G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_intersected.csv", header=TRUE, sep=",") # Made in SearchEbd.R
  
  # TRAVELING DISTANCE FILTER: REMOVE THOSE GREATER THAN 1 KM OR 3 KM!!!!
  # post2009 <- subset(post2009, post2009$EFFORT.DISTANCE.KM<=3.0)
  post2009 <- subset(post2009, post2009$EFFORT.DISTANCE.KM<=1.0)
  
  # Get the eBird data that only includes IBAs that have been verified as A criteria (they are A criteria in the IBA db).
  colnames(post2009)[colnames(post2009)=="Site_ID"] <- "Site_Name"
  colnames(post2009)[colnames(post2009)=="Site_ID.1"] <- "Site_ID"
  
  for (i in 1:nrow(pre2008_labeled)){
    if(pre2008_labeled$Site_ID[i] %in% post2009$Site_ID){
      pre2008_labeled$Global[i] <- "Meet Global Requirements"
    } else {
      pre2008_labeled$Global[i] <- "Do NOT Meet Global Requirements"
    }
  }

  # Reshape data to plot
  pre2008_labeled_table <- pre2008_labeled[,-c(1)]
  pre2008_labeled_table <- data.frame(table(cbind(pre2008_labeled_table[1], Global = unlist(pre2008_labeled_table[-1],use.names=FALSE))))
  
  ggplot(pre2008_labeled_table, aes(x=Criteria,y=Freq, fill = Global)) +
    geom_bar(
      stat = "identity", position = position_dodge(0.8),
      width = 0.7
    ) + 
    geom_text(aes(label=Freq), position=position_dodge(width=0.9), vjust=-0.25) +
    # scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
    # scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
    xlab("Criteria") + ylab("Number of Sites") + 
    ggtitle("eBird Results: IBAs with Global Species") +
    theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  











  
  
############################ SUPPLEMENTAL FIGURES AND ANALYSIS ############################ 
############################ SUPPLEMENTAL FIGURES AND ANALYSIS ############################ 
############################ SUPPLEMENTAL FIGURES AND ANALYSIS ############################ 
############################ SUPPLEMENTAL FIGURES AND ANALYSIS ############################ 



## Supplemental A1 (endangered spp) Analysis:
###################### A1 EVALUATION ###########################
################################################################
  
## Step 1: We want to know how many sites originally listed as 
## A1 have species that are still on the IUCN Redlist. 
## This will tell us how many A1 sites may no longer qualify - 
## importance of finding new species to qualify the site on eBird. 
  
################################################################
  
# Load and clean/format data: IUCN species names and categories csv and all IBA sites WITH species info (because we need species info here)
IUCN <- read.csv("G:/IBA_Update/Ebird_Filtering/IUCN_Names_Category.csv",header=TRUE,sep=",") # Redlist
  IUCN$Common_Name <- tolower(IUCN$Common_Name) # make common names lower case
  IBA <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv",header=TRUE,sep=",")  
  IBA$Common_Name <- tolower(IBA$Common_Name) # make common names lower case to compare with IUCN common names
  IBA <- IBA[,-c(1)]
  cond1 <- grep("^A1",IBA[,7]) # Get only sites with A1 criteria - those that rely on the Redlist for designation
  IBA <- IBA[cond1,]
  IBA <- IBA[!duplicated(IBA[1:2]),] # Remove duplicates for multiple entries of same species at the same site
  IBA <- subset(IBA, !dbo_Status_Description=="Delisted" & !dbo_Status_Description=="Rejected" & 
                  !dbo_Status_Description=="Merged") # Remove the IBAs that were rejected, delisted, and merged so we only get those that are accepted, potential, pending, etc.
  
  IBA_A1 <- subset(IBA,IBA$Code=="A1")
  length(unique(IBA_A1$Site_ID)) # 721 unique A1 sites in IBA database of sites that have spp data
  
# Get IUCN status for each IBA A1 species (Use only A1 because it is only criteria relevant to redlist)
IBA_Redlist <- merge(IBA, IUCN, by="Common_Name", all.x=TRUE) # Do left outer join to get IUCN category for each species. This will produce duplicates that we remove next line.
  IBA_Redlist <- unique(IBA_Redlist, by=c("Common_Name","Site_ID","RedList_Category")) # remove duplicates
  IBA_Redlist$RedList_Category <- as.character(IBA_Redlist$RedList_Category) # redlist category a character
  IBA_Redlist$RedList_Category[is.na(IBA_Redlist$RedList_Category)] <- "Delisted" # change NAs to Delisted
  
  # ****IMPORTANT****
  # Find number of unique sites in each redlist category: If a site has a species that is delisted AND still listed
  # the entire site will count as still listed, given that it still qualifies as A1 if 
  # it has at least one species on the redlist
  IBA_Redlist$RedList_Category <- factor(IBA_Redlist$RedList_Category, levels = c("Critically Endangered", "Endangered", "Vulnerable", "Delisted"))
  IBA_Redlist <- IBA_Redlist[order(IBA_Redlist$RedList_Category),]
  IBA_Redlist <- IBA_Redlist[!duplicated(IBA_Redlist[2]),] #remove duplicate site_ID, keep the species entry that is still listed on IUCN.
  # Arrange dataframe to graph how many unique sites in each redlist category
  IBA_Redlist <- IBA_Redlist[,c(1,2,10)]
  
  df <- IBA_Redlist %>%
    group_by(RedList_Category) %>%
    summarise(counts=n())
  df
  
# Q1_Supplemental: What is the current status of species on A1 IBAs in the IBA database?
# Only need IBAs WITH species information (not those sites without)
# ANSWER: 13 sites with critically endangered spp, 114 sites with endangered, 227 vulnerable, 367 sites with delisted species
  ggplot(df, aes(x=RedList_Category, y = counts)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    xlab("RedList Category") + ylab("Number of Unique Sites") + ggtitle("Number of IBA A1 Sites in Each Redlist Category") +
    theme(plot.title = element_text(size = 16, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8))  
  
  
  ################### RESURRECTED A1 SITES #######################
  ################################################################
  
  ## Step 2: In Step 1 we determined how many A1 IBAs may no longer  
  ## qualify for an A1 listing on the basis of having their A1 
  ## species removed from the IUCN redlist (delisted). Here, we 
  ## seek to determine which of these sites can still qualify 
  ## with other redlisted species that were recorded on eBird.
  ## Uses: IBA_Redlist dataframe created in step 2. 
  
  ################################################################
  
  # Load in the eBird data (post 2009) and clean
  eBird <- read.csv("G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_Intersected.csv", header=TRUE, sep=",") # 3. Criteria status of all Ebird species
  eBird <- eBird[-c(1)]
  cond1 <- grep("^A1",eBird[,15]) # Get only sites with A1 criteria - those that rely on the Redlist for designation
  eBird <- eBird[cond1,]
  eBird <- eBird[,c(1,8,9,15)] # We just need common name, site ID, and redlist category
  colnames(eBird)[colnames(eBird)=="Site_ID.1"] <- "Site_ID"
  
  # Merge the IBA data (pre2008) with eBird data (post2009) to see which sites
  # that were delisted still qualify for A1 criteria after the update. 
  # We want to keep only the sites with a redlist category of "Delisted" so we can calculate which sites that would otherwise
  # be delisted still qualify.
  A1_Resurrected <- merge(IBA_Redlist, eBird, by="Site_ID", all.x=TRUE) # Uses IBA_Redlist dataframe generated in step 1
  cond1 <- grep("Delisted",A1_Resurrected[,3]) 
  A1_Resurrected <- A1_Resurrected[cond1,]
  A1_Resurrected$Code <- as.character(A1_Resurrected$Code)
  A1_Resurrected$Code[A1_Resurrected$Code == "A1"] <- "Can Update"
  A1_Resurrected$Code[is.na(A1_Resurrected$Code)] <- "Cannot Update"
  A1_Resurrected <- A1_Resurrected[!duplicated(A1_Resurrected[1]),]
  
  Can_Resurrect <- sum(A1_Resurrected$Code == "Can Update")
  Cannot_Ressurect <- sum(A1_Resurrected$Code == "Cannot Update")
  
  # Summarize counts
  df <- A1_Resurrected %>%
    group_by(Code) %>%
    summarise(counts=n())
  df
  
  
  # Q2_Supplemental: How many A1 IBAs can still qualify for Global Status with other species found on eBird
  # ANSWER: Of the 367 A1 sites that had their species delisted, 184 A1 IBAs can still qualify for 
  #         global criteria with other species found on eBird, 183 cannot.
  # Plot of number of unique A1 IBAs with delisted redlist species that can and cannot be saved by eBird
  ggplot(df, aes(x=Code, y = counts)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    xlab("Status of Site") + ylab("Number of Unique Sites") + ggtitle("A1 IBAs that Qualify for Global Criteria with Different Species") +
    theme(plot.title = element_text(size = 16, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    theme(plot.title = element_text(hjust = 0.5))
  
  
################ CAN THOSE THAT HAVE BEEN DELISTED AND CANNOT QUALIFY FOR GLOBAL WITH OTHER SPP QUALIFY FOR OTHER CRIT? #############
  # Do a quick check and see if the ones that cannot be updated can qualify for other criteria
  eBird <- read.csv("G:/IBA_Update/Ebird_Filtering/Intersected_Outputs/IBA_post2009_intersected.csv", header=TRUE, sep=",") # Reload in the eBird (post2009) Data
    eBird <- eBird[-c(1)]
    eBird <- eBird[,c(1,8,9,15)] # We just need common name, site ID, and redlist category and code
    colnames(eBird)[colnames(eBird)=="Site_ID.1"] <- "Site_ID"
  
  # All of the sites in this list are A1 sites from the IBA database with their new criteria codes updated from eBird
  test <- merge(A1_Resurrected, eBird, by="Site_ID", all.x=TRUE)
  test <- test[,c(1:6,9)]
  test <- test[!duplicated(test[c(1,7)]),]
  colnames(test)[colnames(test)=="Code.x"] <- "A1Update_Status" # If site has no redlist spp in ebird, it Cannot be Updated as an A1 site with a different species
  colnames(test)[colnames(test)=="Code.y"] <- "New_Code"
  test <- subset(test, test$A1Update_Status == "Cannot Update") # only want to look at those we can't update with A1 criteria
  test <- test[!duplicated(test[1]),] # remove duplicates to get unique sites: doing so is okay cause a site either can or can't be updated (not both)
  test$New_Code <- as.character(test$New_Code)
  test$Criteria_Update[!is.na(test$New_Code)] <- "Can Update" # if an A1 site has an updated code, it can be updated with another criteria code
  test$Criteria_Update[is.na(test$New_Code)] <- "Cannot Update" # if an A1 site has no updated code from eBird, it can't be updated with another criteria code
  
  
  df <- test %>%
    group_by(New_Code) %>%
    summarise(counts=n())
  df
  
  
  # Q3_Supplemental: Of the A1 sites that cannot be updated with other redlisted species found on eBird,
  #                 how many can qualify for other global criteria? 
  # ANSWER: 104/183 sites that cannot qualify for A1 with other species can qualify for A3 criteria,
  #         4/183 sites no longer qualifying for A1 can qualify for A4 and 75/183 do not qualify for ANY global criteria
  # Plot number of A1 sites that couldn't be updated with other endangered species but CAN qualify for other criteria
  ggplot(df, aes(x=New_Code, y = counts)) +
    geom_bar(fill = "#0073C2FF", stat = "identity") +
    geom_text(aes(label = counts), vjust = -0.3) +
    xlab("Status of Site") + ylab("Number of Unique Sites") + ggtitle("A1 Sites Qualifying for Other Criteria") +
    theme(plot.title = element_text(size = 16, face = "bold"), axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8)) +
    theme(plot.title = element_text(hjust = 0.5))
  

  # Summarize the number of A1 sites that couldn't be updated with other endangered species but CAN qualify for other criteria
  Qualify_OtherCriteria <- sum(test$Criteria_Update == "Can Update")
  NoQualify_AnyGlobalCriteria <- sum(test$Criteria_Update == "Cannot Update")
  
  # Count no. of total A1 IBAs
  IBA <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv",header=TRUE,sep=",")  
  IBA <- IBA[,-c(1)]
  cond1 <- grep("^A1",IBA[,7]) # Get only sites with A1 criteria - those that rely on the Redlist for designation
  IBA <- IBA[cond1,]
  IBA <- IBA[!duplicated(IBA[2]),] # 767 Unique sites listed as A1 Criteria
  A1IBAs <- nrow(IBA)
  
  
 
  
  

############################  What is the status of all IBAs in the access database? ################################ 
#####################################################################################################################
#####################################################################################################################
## How many global IBAs in the access database have species info and how many don't? What is the status of these sites?

## Solution: Find total number of unique IBAs that do and don't have species info, then
##           find and graph the status of each group (with species and without species info) for each criteria code.

# Reading and cleaning the pre2008 csv that contains all IBAs with spp info
IBA_sppinfo <- read.csv("G:/IBA_Update/Ebird_Filtering/Outputs/IBA_pre2008.csv",header=TRUE,sep=",")  
IBA_sppinfo <- IBA_sppinfo[,-c(1)]
cond1 <- grep("^A",IBA_sppinfo[,7]) # index refers to column with criteria codes. ^A - change if you want B or D criteria
IBA_sppinfo <- IBA_sppinfo[cond1,] 
IBA_sppinfo <- IBA_sppinfo[c(2,6,7)] # only need site ID, status, and criteria code
IBA_sppinfo$Code <- as.character(IBA_sppinfo$Code)
IBA_sppinfo$Description <- as.character(IBA_sppinfo$Description)
IBA_sppinfo$Code[IBA_sppinfo$Code == "A4i"] <- "A4"
IBA_sppinfo$Code[IBA_sppinfo$Code == "A4ii"] <- "A4"
IBA_sppinfo$Code[IBA_sppinfo$Code == "A4iii"] <- "A4"
IBA_sppinfo$Code[IBA_sppinfo$Code == "A4iv"] <- "A4"
colnames(IBA_sppinfo)[colnames(IBA_sppinfo)=="dbo_Status_Description"] <- "Description"



# Q1: How many global IBAs with species info are in the access database?
IBA_sppinfo_sitecount <- IBA_sppinfo[!duplicated(IBA_sppinfo[1]),] # Remove duplicated sites: we just want unique sites
nrow(IBA_sppinfo_sitecount) # ANSWER: 1236 unique sites (regardless of site status)


# Q2: How many of these sites are recognized, pending, rejected, delisted, etc.? 
IBA_sppinfo_status <- IBA_sppinfo[!duplicated(IBA_sppinfo[c(1,3)]),] # remove duplicated site and criteria combo: some sites have multiple criteria
IBA_sppinfo_status <- IBA_sppinfo_status[,-c(1)]
IBA_sppinfo_status <- table(cbind(IBA_sppinfo_status[2], Description = unlist(IBA_sppinfo_status[-2],use.names=FALSE)))
IBA_sppinfo_status <- data.frame(IBA_sppinfo_status)
colnames(IBA_sppinfo_status)[colnames(IBA_sppinfo_status)=="Freq"] <- "Counts"

# ANSWER: 616 Recognized A1, 13 recognized A2, 31 recognized A3, and 313 recognized A4 
ggplot(IBA_sppinfo_status, aes(x=Code,y=Counts, fill = Description)) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + 
  geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.25) +
  # scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  # scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  xlab("Criteria Code") + ylab("Number of Unique Sites") + 
  ggtitle("Global IBAs with Species Info: Status and Criteria Codes") +
  theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))



## No spp info:
## Reading and cleaning csv of IBAs that have NO species information in the access database.
sitecriteria <- read.csv("G:/IBA_Update/Ebird_Filtering/IBA_Input/Site_Status_Criteria.csv", header=TRUE,sep=",") # 4. All IBA sites with their criteria listing
cond1 <- grep("^A",sitecriteria[,3]) # Select all A criteria
sitecriteria <- sitecriteria[cond1,]
sitecriteria$Code <- as.character(sitecriteria$Code)
sitecriteria$Description <- as.character(sitecriteria$Description)
sitecriteria$Code[sitecriteria$Code == "A4i"] <- "A4"
sitecriteria$Code[sitecriteria$Code == "A4ii"] <- "A4"
sitecriteria$Code[sitecriteria$Code == "A4iii"] <- "A4"
sitecriteria$Code[sitecriteria$Code == "A4iv"] <- "A4"


# Q3: How many total global IBAs with NO species information are in the access database? 
sitecriteria_sitecount <- sitecriteria[!duplicated(sitecriteria[1]),] # Get Unique sites: 660 unique global sites with criteria info
nrow(sitecriteria_sitecount) # ANSWER: 660 unique global IBAs with no species info in access database


# Q4: How many of the IBA sites with NO spp info are recognized, pending, rejected, delisted, etc. in each criteria code? 
sitecriteria_status <- sitecriteria[!duplicated(sitecriteria[c(1,3)]),] # remove duplicated site and criteria combo: some sites have multiple criteria
sitecriteria_status <- sitecriteria_status[,-c(1)]
sitecriteria_status <- table(cbind(sitecriteria_status[2], Description = unlist(sitecriteria_status[-2],use.names=FALSE)))
sitecriteria_status <- data.frame(sitecriteria_status)
colnames(sitecriteria_status)[colnames(sitecriteria_status)=="Freq"] <- "Counts"

# ANSWER: 134 Recognized A1, 13 recognized A2, 37 recognized A3, and 172 recognized A4 
ggplot(sitecriteria_status, aes(x=Code,y=Counts, fill = Description)) +
  geom_bar(
    stat = "identity", position = position_dodge(0.8),
    width = 0.7
  ) + 
  geom_text(aes(label=Counts), position=position_dodge(width=0.9), vjust=-0.25) +
  # scale_color_manual(values = c("#0073C2FF", "#EFC000FF"))+
  # scale_fill_manual(values = c("#0073C2FF", "#EFC000FF")) +
  xlab("Criteria Code") + ylab("Number of Unique Sites") + 
  ggtitle("Global IBAs with NO Species Info: Status and Criteria Codes") +
  theme(plot.title = element_text(size = 18, face = "bold"), axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14)) +
  theme(plot.title = element_text(hjust = 0.5))

##################################################### END SUPPLEMENTAL PART 1: IBA DB SUMMARY ######################################################
##################################################### END SUPPLEMENTAL PART 1: IBA DB SUMMARY ######################################################
##################################################### END SUPPLEMENTAL PART 1: IBA DB SUMMARY ######################################################

