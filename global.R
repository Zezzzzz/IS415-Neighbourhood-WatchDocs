library(rgdal) 
library(maptools) 
library(raster) 
library(spatstat)
library(sf)
library(tmap)
library(tidyverse)
library(SpatialAcc)
library(leaflet)
library(shiny)
library(tbart)
library(dplyr)
library(rgeos)
library(proj4)
library(rsconnect)
library(pracma)
library(compareDF)

#####################################################################
# Singapore Planning Subzone (MP14_SUBZONE_WEB_PL)
mpsz <- st_read(dsn = "data", layer = "MP14_SUBZONE_WEB_PL")
mpsz <- mpsz[order(mpsz$SUBZONE_N),]

# Singapore Residents by Subzone, Age Group and Sex (June 2017)
population <- read_csv("data/respopagsex2000to2017.csv")

# Residents by Age Group & Type of Dwelling, Annual
popByDwelling <- read_csv("data/residents-by-age-group-and-type-of-dwelling-detailed-categories-annual.csv")

# Total Number of GP Clinics in Singapore
clinics = read_csv("data/clinics_hcidirectory.csv")
clinics_essentials <- c("clinic_name", "address", "LAT", "LONG", "X", "Y")
clinics <- clinics[clinics_essentials]

# Total Number of TCM Clinics in Singapore
tcm = read_csv("data/tcm_tcmboard.csv")
tcm <- tcm[!duplicated(tcm$tcm_place_name),]
tcm_essentials <- c("tcm_place_name", "tcm_address", "LAT", "LONG", "X", "Y")
tcm <- tcm[tcm_essentials]
names(tcm)[names(tcm) == "tcm_place_name"] <- "clinic_name"
names(tcm)[names(tcm) == "tcm_address"] <- "address"

# Total Number of GP and TCM clinics in Singapore
clinics_combined <- rbind(clinics, tcm)
mpsz_clinics <- st_join(st_as_sf(clinics_combined, 
                                 coords = c("X", "Y"), crs = st_crs(mpsz)), mpsz)

# Number of HDB blocks per planning subzone
HDB = read_csv("data/hdb_property_information.csv")

#####################################################################
# GP Clinics sf object
clinics_sf <- st_as_sf(clinics, coords = c("X", "Y"), crs = st_crs(mpsz))
clinics_sf <- st_join(clinics_sf, mpsz)

# TCM Clinics sf object
tcm_sf <- st_as_sf(tcm, coords = c("X", "Y"), crs = st_crs(mpsz))
tcm_sf <- st_join(tcm_sf, mpsz)

# HDB Blocks sf object
HDB_sf <- st_as_sf(HDB, coords = c("X", "Y"), crs = st_crs(mpsz))

# Filter out elderly population by Subzone, Age Group and Year 2017
popage2017pre <- population %>% filter(Time == 2017) %>% 
  filter(AG == "65_to_69" | 
         AG == "70_to_74" | 
         AG == "75_to_79" | 
         AG == "80_to_84" | 
         AG == "85_and_over") %>% group_by(SZ)

# Sum up all elderly population of all age groups, group by SUBZONE 
popage2017pre_added <- aggregate(Pop ~ SZ, popage2017pre, sum)

# Filter out elderly population by Age Group & Type of Dwelling and Year 2017
popByDwelling2017 <- popByDwelling %>% filter(year == 2017) %>% 
  filter(level_1 == "65-69 Years" | 
         level_1 == "85 Years & Over" | 
         level_1 == "70-74 Years" | 
         level_1 == "75-79 Years" | 
         level_1 == "80-84 Years") %>% group_by(level_3)

# Sum up all elderly population of all age groups, group by Type of Dwelling
popByDwelling2017_added <- aggregate(value ~ level_3, popByDwelling2017, sum)

# No. of blocks per subzone
mpsz_HDB <- st_join(HDB_sf,mpsz)

# Calculate total number of units based on dwelling type
mpsz_HDB$`1_2room_total` <- mpsz_HDB$`1room_sold` + mpsz_HDB$`2room_sold` + mpsz_HDB$`1room_rental` + mpsz_HDB$`2room_rental`

mpsz_HDB_1_2_room = aggregate(mpsz_HDB$`1_2room_total`, by=list(SUBZONE=mpsz_HDB$SUBZONE_N), FUN=sum) %>%
  rename('No_of_1_2_room' = 'x') 

mpsz_HDB$`3room_total` <- mpsz_HDB$`3room_sold` + mpsz_HDB$`3room_rental` + mpsz_HDB$`other_room_rental`

mpsz_HDB_3_room_added = aggregate(mpsz_HDB$`3room_total`, by=list(SUBZONE=mpsz_HDB$SUBZONE_N), FUN=sum) %>%
  rename('No_of_3_room' = 'x') 

mpsz_HDB$`4room_total` <- mpsz_HDB$`4room_sold`

mpsz_HDB_4_room_added = aggregate(mpsz_HDB$`4room_total`, by=list(SUBZONE=mpsz_HDB$SUBZONE_N), FUN=sum) %>%
  rename('No_of_4_room' = 'x') 

mpsz_HDB$`5room_exec_total` <- mpsz_HDB$`5room_sold` + mpsz_HDB$`exec_sold`

mpsz_HDB_5_room_exec_added = aggregate(mpsz_HDB$`5room_exec_total`, by=list(SUBZONE=mpsz_HDB$SUBZONE_N), FUN=sum) %>%
  rename('No_of_5_room_exec' = 'x')   

mpsz_HDB_added <- left_join(mpsz_HDB_1_2_room, mpsz_HDB_3_room_added, by ='SUBZONE') %>%
  left_join(., mpsz_HDB_4_room_added, by = 'SUBZONE') %>%
  left_join(., mpsz_HDB_5_room_exec_added, by = 'SUBZONE')

mpsz_HDB_added$`total_units` <- mpsz_HDB_added$`No_of_1_2_room` + mpsz_HDB_added$`No_of_3_room` + mpsz_HDB_added$`No_of_4_room` + mpsz_HDB_added$`No_of_5_room`

total_1_2_room_units <- sum(mpsz_HDB_added$No_of_1_2_room) 
total_3_room_units <- sum(mpsz_HDB_added$No_of_3_room) 
total_4_room_units <- sum(mpsz_HDB_added$No_of_4_room) 
total_5_room_exec_units <- sum(mpsz_HDB_added$No_of_5_room_exec) 

# No. of elderly per block in every subzone
total_1_2_room_units <- sum(mpsz_HDB_added$No_of_1_2_room) 
total_3_room_units <- sum(mpsz_HDB_added$No_of_3_room) 
total_4_room_units <- sum(mpsz_HDB_added$No_of_4_room) 
total_5_room_units <- sum(mpsz_HDB_added$No_of_5_room) 

mpsz_HDB$No_of_Elderly_in_block_1_2 <- ifelse(mpsz_HDB$`1_2room_total` == 0, 0, mpsz_HDB$`1_2room_total`/total_1_2_room_units) * popByDwelling2017_added$value[popByDwelling2017_added$level_3=="HDB 1- And 2-Room Flats"]

mpsz_HDB$No_of_Elderly_in_block_3 <- ifelse(mpsz_HDB$`3room_total` == 0, 0, mpsz_HDB$`3room_total`/total_3_room_units) * popByDwelling2017_added$value[popByDwelling2017_added$level_3=="HDB 3-Room Flats"] 

mpsz_HDB$No_of_Elderly_in_block_4 <- ifelse(mpsz_HDB$`4room_total` == 0, 0, mpsz_HDB$`4room_total`/total_4_room_units) * popByDwelling2017_added$value[popByDwelling2017_added$level_3=="HDB 4-Room Flats"]

mpsz_HDB$No_of_Elderly_in_block_5 <- ifelse(mpsz_HDB$`5room_exec_total` == 0, 0, mpsz_HDB$`5room_exec_total`/total_5_room_exec_units) * popByDwelling2017_added$value[popByDwelling2017_added$level_3=="HDB 5-Room And Executive Flats"]


mpsz_HDB$No_of_Elderly_in_block <- mpsz_HDB$No_of_Elderly_in_block_1_2 + mpsz_HDB$No_of_Elderly_in_block_3 + mpsz_HDB$No_of_Elderly_in_block_4 + mpsz_HDB$No_of_Elderly_in_block_5

mpsz_HDB$No_of_Elderly_in_block <- round(mpsz_HDB$No_of_Elderly_in_block)

excl <- data.frame(x = c(mpsz_clinics$SUBZONE_N))
excl <- unique(excl)

excl1 <- data.frame(x = c(mpsz_HDB$SUBZONE_N))
excl1 <- unique(excl1)

subzone_list <- merge(excl, excl1) %>% rename(subzone = x)
subzone_list <- subzone_list[order(subzone_list$subzone),]
subzone_list <- as.data.frame.vector(subzone_list)

sum_dist_df2 <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("LAT","LONG","SUM_DIST")
colnames(sum_dist_df2) <- x

######################################## ALLOCATION ALGORITHM #######################################
mpsz_HDB_filtered <- mpsz_HDB[mpsz_HDB$SUBZONE_N=="ADMIRALTY", ]
clinics_combined_filtered <- mpsz_clinics[mpsz_clinics$SUBZONE_N=="ADMIRALTY", ]
clinics_combined_filtered <- na.omit(clinics_combined_filtered)

mpsz_HDB_split <- matrix(ncol = 4, nrow = 0)
x <- c("blk_no_street","LAT","LONG","SUBZONE_N")
colnames(mpsz_HDB_split) <- x

for(i in 1:nrow(mpsz_HDB_filtered)){
  n <- mpsz_HDB_filtered$No_of_Elderly_in_block[i]
  lat <- mpsz_HDB_filtered$LAT[i]
  long <- mpsz_HDB_filtered$LONG[i]
  radiusLat <- 0.0001
  radiusLong <- 0.0001
  angle <- 2*pi*rand(n,1)
  rLat <- radiusLat*sqrt(rand(n,1))
  rLong <- radiusLong*sqrt(rand(n,1))
  latToAdd <- rLat*cos(angle)+ lat;
  longToAdd <- rLong*sin(angle)+ long;
  
  for(j in 1:n){
    newRow_df <- data.frame(blk_no_street=mpsz_HDB_filtered$blk_no_street[i],
                            LAT = latToAdd[j],
                            LONG = longToAdd[j],
                            LAT1 = latToAdd[j],
                            LONG1 = longToAdd[j],
                            SUBZONE_N = mpsz_HDB_filtered$SUBZONE_N[i])
    mpsz_HDB_split <- rbind(mpsz_HDB_split, newRow_df)
  }
}

mpsz_HDB_split_sf <- st_as_sf(mpsz_HDB_split, coords = c("LONG1","LAT1"),
                              crs = 4326)

clinics_combined_filtered <- clinics_combined_filtered %>% select(clinic_name, LAT, LONG, SUBZONE_N) %>%
  st_set_geometry(NULL)
clinics_combined_filtered$LAT1 <- clinics_combined_filtered$LAT
clinics_combined_filtered$LONG1 <- clinics_combined_filtered$LONG
clinics_combined_filtered <- st_as_sf(clinics_combined_filtered, coords = c("LONG1","LAT1"),
                                      crs = 4326)

mpsz_HDB_filtered_sp <- as_Spatial(mpsz_HDB_split_sf)
clinics_combined_filtered_sp <- as_Spatial(clinics_combined_filtered)

alloc_results <- allocations(mpsz_HDB_filtered_sp, clinics_combined_filtered_sp, p=nrow(clinics_combined_filtered))
alloc_results <- st_as_sf(alloc_results, coords = c("LONG", "LAT"), crs = st_crs(mpsz))
alloc_results <- alloc_results[order(alloc_results$allocation, alloc_results$allocdist),]

capacity <- 80

subzone_clinics <- st_as_sf(clinics_combined_filtered_sp, coords = c("LONG", "LAT"), crs = st_crs(mpsz)) %>% mutate(capacity = capacity)

# create empty data frame with header columns
total_allocated_elderly <- alloc_results[0,]
total_unallocated_elderly <- alloc_results[0,]

### FIRST RUN OF ALLOCATION ALGORITHM ###
for(i in 1:nrow(subzone_clinics)) {
  clinic_row <- subzone_clinics[i,]
  
  # get the total elderlys allocated to this allocation ID
  clinic_n_allocation <- alloc_results[which(alloc_results$allocation == i), ]
  
  if(nrow(clinic_n_allocation) >= capacity) {
    # capacity for this clinic is maxed out
    subzone_clinics$capacity[i] <- 0
    
    total_allocated_elderly <- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:capacity))
    
    # get the remaining unallocated elderlys, append to total unallocated elderlys
    unallocated_elderly <- clinic_n_allocation %>% slice(capacity+1:nrow(clinic_n_allocation))
    total_unallocated_elderly <- rbind(total_unallocated_elderly, unallocated_elderly)
    
  } else if(nrow(clinic_n_allocation) < capacity) {
    # calculate the remaining capacity
    subzone_clinics$capacity[i] <- capacity - nrow(clinic_n_allocation)
    
    total_allocated_elderly <- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:nrow(clinic_n_allocation)))
    
  }
}

### CONTINUOUS ALLOCATION ALGORITHM ###

while(nrow(subzone_clinics[which(subzone_clinics$capacity > 0), ]) > 0 & nrow(total_unallocated_elderly) > 0) {
  no_of_clinics <- nrow(subzone_clinics[which(subzone_clinics$capacity > 0), ])
  clinics_remaining <- subzone_clinics[which(subzone_clinics$capacity > 0), ]
  
  total_unallocated_elderly <- total_unallocated_elderly %>%
    select("blk_no_street","LAT","LONG","SUBZONE_N")
  
  # convert both data.frames to SpatialPointsDataFrame
  clinics_remaining_sp <- as_Spatial(clinics_remaining)
  total_unallocated_elderly_sp <- as_Spatial(total_unallocated_elderly)
  
  # run ALLOCATION algorithm again
  alloc_results <- allocations(total_unallocated_elderly_sp,
                               clinics_remaining_sp, p=no_of_clinics)
  
  alloc_results <- st_as_sf(alloc_results, coords = c("LONG", "LAT"), crs = st_crs(mpsz))
  alloc_results <- alloc_results[order(alloc_results$allocation, alloc_results$allocdist),]
  
  # create empty data frame with header columns
  total_unallocated_elderly <- alloc_results[0,]
  
  for(i in 1:nrow(clinics_remaining)) {
    clinic_row <- clinics_remaining[i,]
    
    # get the total elderlys allocated to this allocation ID
    clinic_n_allocation <- alloc_results[which(alloc_results$allocation == i), ]
    
    clinic_capacity <- subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name]
    
    if(nrow(clinic_n_allocation) >= clinic_capacity) {
      subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name] = 0
      
      # get the unallocated elderlys, append to total unallocated elderlys
      unallocated_elderly <- clinic_n_allocation %>% slice(clinic_capacity+1:nrow(clinic_n_allocation))
      total_unallocated_elderly <- rbind(total_unallocated_elderly, unallocated_elderly)
      
      total_allocated_elderly <- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:clinic_capacity))
      
    } else if(nrow(clinic_n_allocation) < clinic_capacity) {
      # calculate the remaining capacity
      subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name] = clinic_capacity - nrow(clinic_n_allocation)
      
      total_allocated_elderly <- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:nrow(clinic_n_allocation)))
    }
  }
}

total_unallocated_elderly <- total_unallocated_elderly %>% select("blk_no_street","LAT","LONG","SUBZONE_N")