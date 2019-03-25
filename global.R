library(rgdal) 
library(maptools) 
library(raster) 
library(spatstat)
library(sf)
library(tmap)
library(tidyverse)
library(SpatialAcc)

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