#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
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

# Define server logic
shinyServer(function(input, output, session) {
  
  ## Logic for Interactive Map Tab ##
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 103.8509, lat = 1.3800, zoom = 12)
  })
  
  observeEvent(c(input$subzone),{
    
    # retrieve all the clinic and elderly points in each HDB of the Subzone
    getClinicAndElderlyPointsInSubzone(input$subzone)
    
    subzone <- mpsz[mpsz$SUBZONE_N == input$subzone,]
    p <- as(subzone, 'Spatial')  
    sp_cent <- gCentroid(p, byid = TRUE)
    
    proj4string <- proj4string(sp_cent)
    
    xy <- data.frame(sp_cent)
    
    pj <- project(xy, proj4string, inverse=TRUE)
    
    latlon <- data.frame(lat=pj$y, lon=pj$x)
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% 
        setView(lng = latlon$lon, lat = latlon$lat, zoom = 16)
    })
  })
  
  observeEvent(c(input$map_click, input$Type, input$subzone, input$accMethod, 
                 input$analysisType, input$cCapacity), {
    
    clinic_results <- reactive({
      mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
    })
    
    hdb_results <- reactive({
      mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
    })
    
    acc_results <- reactive({
      clinics_coords <- clinic_results() %>% st_coordinates()
      hdb_coords <- hdb_results() %>% st_coordinates()
       
      # check that reactive expr return value is not empty before proceeding
      if(nrow(clinics_coords) != 0 & nrow(hdb_coords) != 0) {
        capacity <- round(sum(hdb_results()$No_of_Elderly_in_block) / nrow(clinic_results()))
        
        clinics <- clinic_results() %>% mutate(`capacity` = capacity)
        
        dm <- distance(hdb_coords, clinics_coords)
        acc_val <- data.frame(ac(hdb_results()$No_of_Elderly_in_block,
                                    clinics$capacity, dm,
                                    power = 2, family = input$accMethod))
        colnames(acc_val) <- "accVal"
        acc_val <- tbl_df(acc_val)
        acc_val <- lapply(acc_val, function(x) replace(x, !is.finite(x), 0))
        
        HDB_acc <- bind_cols(hdb_results(), acc_val)
      }
    })
    
    if(all(c("clinics_combined", "hdb") %in% input$Type)) {

      if(!is.null(acc_results()) & input$analysisType == 'geoAcc') {
        sum_dist_df2 <<- data.frame(matrix(ncol = 3, nrow = 0))
        x <<- c("LAT","LONG","SUM_DIST")
        
        colnames(sum_dist_df2) <<- x
        #pal = colorQuantile("Purples", n = 5, acc_results()$accVal)
        quantileNum <- 5

        probs <- seq(0, 1, length.out = quantileNum + 1)
        bins <- quantile(acc_results()$accVal, probs, na.rm = TRUE, names = FALSE)

        while (length(unique(bins)) != length(bins)) {
          quantileNum <- quantileNum - 1
          probs <- seq(0, 1, length.out = quantileNum + 1)
          bins <- quantile(acc_results()$accVal, probs, na.rm = TRUE, names = FALSE)
        }

        pal <- colorBin("YlGn", bins = bins)
        
        leafletProxy("map", data = acc_results()) %>%
          clearControls() %>%
          clearShapes() %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LONG,
                           lat = ~LAT,
                           popup = paste("", acc_results()$blk_no_street, "<br><br>",
                                         "Acc-Val: ", acc_results()$accVal),
                           color = "black",
                           fillColor = ~pal(acc_results()$accVal),
                           fillOpacity = 0.8)
          #addLegend(pal = pal, values = ~accVal, opacity = 0.8, position = "bottomright")
        
      } else if(input$analysisType == 'allocation') {
        new_clinic_location <<- input$map_click

        ## This displays the pin drop circle
        if(!is.null(new_clinic_location)) {
          leafletProxy("map") %>%
            clearControls() %>%
            clearShapes() %>%
            addCircles(new_clinic_location$lng, new_clinic_location$lat, 
                       radius=15, color="black", fillColor = "blue")
        }
        
        allocateElderly(input$subzone, input$cCapacity, new_clinic_location)

        output$total_elderly_in_subzone <- renderText(
         paste("<b>", "TOTAL ELDERLY IN ", input$subzone, ": </b>", nrow(mpsz_HDB_split)))
        
        output$total_elderly_allocated <- renderText(
         paste("<b>", "TOTAL ELDERLY ALLOCATED: ", "</b>", nrow(total_allocated_elderly)))
        
        output$total_alloc_dist <- renderText(
         paste("<b>", "TOTAL ALLOCATION DISTANCE: ", "</b>", sum(total_allocated_elderly$allocdist)))
        
        output$total_elderly_unallocated <- renderText(
         paste("<b>", "TOTAL ELDERLY UNALLOCATED: ", "</b>", nrow(total_unallocated_elderly)))
        
        total_allocated_elderly <<- total_allocated_elderly %>% mutate(freq = 1)
        total_unallocated_elderly <<- total_unallocated_elderly %>% mutate(freq = 1)
        
        if(nrow(total_allocated_elderly) > 0) {
         allocated_aggregated <<- aggregate(total_allocated_elderly$freq, 
                                           by=list(blk_no_street=total_allocated_elderly$blk_no_street), 
                                           FUN=sum) %>% rename(no_of_elderly_allocated = x)
         output$allocated_elderly_output <- renderDataTable(allocated_aggregated)
        }
        
        if(nrow(total_unallocated_elderly) > 0) {
         unallocated_aggregated <<- aggregate(total_unallocated_elderly$freq, 
                                             by=list(blk_no_street=total_unallocated_elderly$blk_no_street), 
                                             FUN=sum) %>% rename(no_of_elderly_unallocated = x)
         output$unallocated_elderly_output <- renderDataTable(unallocated_aggregated) 
        }
        
        colorOnHDB(unallocated_aggregated, allocated_aggregated)
        
        leafletProxy("map", data = final_results) %>%
         clearControls() %>%
         clearMarkers() %>%
         addCircleMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste(final_results$blk_no_street, "<br><br>",
                                        "Elderly Allocated: ", final_results$no_of_elderly_allocated, 
                                        "<br>", "Elderly Unallocated: ", 
                                        final_results$no_of_elderly_unallocated),
                          color = "black",
                          fillColor = final_results$color,
                          fillOpacity = 0.8)
      }
      
    } else if(c("clinics_combined") %in% input$Type) {
      leafletProxy("map", data = clinic_results()) %>%
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", clinic_results()$clinic_name, "<br><br>",
                                        "", clinic_results()$address),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
    } else if(c("hdb") %in% input$Type) {
      leafletProxy("map", data = hdb_results()) %>%
        clearControls() %>%
        clearShapes() %>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", hdb_results()$blk_no_street, "<br><br>",
                                        "Elderly Population: ", hdb_results()$No_of_Elderly_in_block),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      
    } else {
      leafletProxy("map", data = NULL) %>%
        clearControls() %>%
        clearShapes()%>%
        clearMarkers()
    }
  
  })
  
  ## Logic for Data Explorer Tab ##
  output$viewDataTable <- renderDataTable(elderly_per_hdb)
  
  observeEvent(input$selectDT, {
    if(input$selectDT == "elderly_per_hdb") {
      elderly_per_hdb <- mpsz_HDB %>% st_set_geometry(NULL) %>% 
        select(`blk_no_street`, `postal_code`, `SUBZONE_N`, `No_of_Elderly_in_block`)
      
      output$viewDataTable <- renderDataTable(elderly_per_hdb)
      
    } else {
      total_clinics <- mpsz_clinics %>% st_set_geometry(NULL) %>% 
        select(`clinic_name`, `address`, `SUBZONE_N`)
      
      output$viewDataTable <- renderDataTable(total_clinics)
    }
  })
})

# Function to return all clinic points and elderly points in each HDB in a particular Subzone
getClinicAndElderlyPointsInSubzone <- function(subzone) {
  cat("clinicAndElderlyInSubzone: ", subzone, "\n")
  
  mpsz_HDB_filtered <<- mpsz_HDB[mpsz_HDB$SUBZONE_N==subzone, ]
  clinics_combined_filtered <<- mpsz_clinics[mpsz_clinics$SUBZONE_N==subzone, ]
  clinics_combined_filtered <<- na.omit(clinics_combined_filtered)
  
  mpsz_HDB_split <<- matrix(ncol = 4, nrow = 0)
  x <<- c("blk_no_street","LAT","LONG","SUBZONE_N")
  colnames(mpsz_HDB_split) <<- x
  
  if(nrow(mpsz_HDB_filtered) > 0) {
    withProgress(message = "Calculation in progress", detail = "This may take a while...", value = 0, {
      for(i in 1:nrow(mpsz_HDB_filtered)){
        incProgress(1/10)
        
        n <<- mpsz_HDB_filtered$No_of_Elderly_in_block[i]
        lat <<- mpsz_HDB_filtered$LAT[i]
        long <<- mpsz_HDB_filtered$LONG[i]
        radiusLat <<- 0.0001
        radiusLong <<- 0.0001
        angle <<- 2*pi*rand(n,1)
        rLat <<- radiusLat*sqrt(rand(n,1))
        rLong <<- radiusLong*sqrt(rand(n,1))
        latToAdd <<- rLat*cos(angle)+ lat;
        longToAdd <<- rLong*sin(angle)+ long;
        
        for(j in 1:n){
          newRow_df <<- data.frame(blk_no_street=mpsz_HDB_filtered$blk_no_street[i],
                                   LAT = latToAdd[j],
                                   LONG = longToAdd[j],
                                   LAT1 = latToAdd[j],
                                   LONG1 = longToAdd[j],
                                   SUBZONE_N = mpsz_HDB_filtered$SUBZONE_N[i])
          mpsz_HDB_split <<- rbind(mpsz_HDB_split, newRow_df)
        }
      }
    })
    
    mpsz_HDB_split_sf <<- st_as_sf(mpsz_HDB_split, coords = c("LONG1","LAT1"), crs = 4326)
    
    clinics_combined_filtered <<- clinics_combined_filtered %>% select(clinic_name, LAT, LONG, SUBZONE_N) %>%
      st_set_geometry(NULL)
    clinics_combined_filtered$LAT1 <<- clinics_combined_filtered$LAT
    clinics_combined_filtered$LONG1 <<- clinics_combined_filtered$LONG
    clinics_combined_filtered <<- st_as_sf(clinics_combined_filtered, coords = c("LONG1","LAT1"),
                                           crs = 4326)
    
    mpsz_HDB_filtered_sp <<- as_Spatial(mpsz_HDB_split_sf)
    clinics_combined_filtered_sp <<- as_Spatial(clinics_combined_filtered)
  }
}

# Function to run the allocations algorithm of tbart
allocateElderly <- function(subzone, capacity, new_clinic_location) {
  cat("allocateElderly Subzone: ", subzone, " | allocateElderly Capacity: ", capacity)
  
  if(!is.null(new_clinic_location)) {
    new_clinic <- data.frame(clinic_name="New Clinic",
                             LAT=new_clinic_location$lat,
                             LONG = new_clinic_location$lng,
                             SUBZONE_N = subzone)
    
    new_clinic_list <- clinics_combined_filtered %>% select(clinic_name, LAT, LONG, SUBZONE_N) %>%
      st_set_geometry(NULL)
    new_clinic_list <- rbind(new_clinic_list, new_clinic)
    
    new_clinic_list$LAT1 <- new_clinic_list$LAT
    new_clinic_list$LONG1 <- new_clinic_list$LONG
    new_clinic_list <- st_as_sf(new_clinic_list, coords = c("LONG1","LAT1"), crs = 4326)
    
    new_clinic_list_sp <- as_Spatial(new_clinic_list)
    
    alloc_results <<- allocations(mpsz_HDB_filtered_sp, new_clinic_list_sp, p=nrow(new_clinic_list))
    alloc_results <<- st_as_sf(alloc_results, coords = c("LONG", "LAT"), crs = st_crs(mpsz))
    alloc_results <<- alloc_results[order(alloc_results$allocation, alloc_results$allocdist),]
    
    subzone_clinics <<- st_as_sf(new_clinic_list_sp, coords = c("LONG", "LAT"), 
                                 crs = st_crs(mpsz)) %>% mutate(capacity = capacity)
    
  } else {
    alloc_results <<- allocations(mpsz_HDB_filtered_sp, clinics_combined_filtered_sp, p=nrow(clinics_combined_filtered))
    alloc_results <<- st_as_sf(alloc_results, coords = c("LONG", "LAT"), crs = st_crs(mpsz))
    alloc_results <<- alloc_results[order(alloc_results$allocation, alloc_results$allocdist),]
    
    subzone_clinics <<- st_as_sf(clinics_combined_filtered_sp, coords = c("LONG", "LAT"), 
                                 crs = st_crs(mpsz)) %>% mutate(capacity = capacity)
  }
  
  # create empty data frame with header columns
  total_allocated_elderly <<- alloc_results[0,]
  total_unallocated_elderly <<- alloc_results[0,]
  
  ### FIRST RUN OF ALLOCATION ALGORITHM ###
  withProgress(message = "Calculation in progress", detail = "This may take a while...", value = 0, {
    for(i in 1:nrow(subzone_clinics)) {
      incProgress(1/10)
      
      clinic_row <<- subzone_clinics[i,]
      
      # get the total elderlys allocated to this allocation ID
      clinic_n_allocation <<- alloc_results[which(alloc_results$allocation == i), ]
      
      if(nrow(clinic_n_allocation) >= capacity) {
        # capacity for this clinic is maxed out
        subzone_clinics$capacity[i] <<- 0
        
        total_allocated_elderly <<- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:capacity))
        
        # get the remaining unallocated elderlys, append to total unallocated elderlys
        unallocated_elderly <<- clinic_n_allocation %>% slice(capacity+1:nrow(clinic_n_allocation))
        total_unallocated_elderly <<- rbind(total_unallocated_elderly, unallocated_elderly)
        
      } else if(nrow(clinic_n_allocation) < capacity) {
        # calculate the remaining capacity
        subzone_clinics$capacity[i] <<- capacity - nrow(clinic_n_allocation)
        
        total_allocated_elderly <<- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:nrow(clinic_n_allocation)))
        
      }
    }
  })
  
  withProgress(message = "Calculation in progress", detail = "This may take a while...", value = 0, {
  
    ### CONTINUOUS ALLOCATION ALGORITHM ###
    while(nrow(subzone_clinics[which(subzone_clinics$capacity > 0), ]) > 0 & 
          nrow(total_unallocated_elderly) > 0) {
      incProgress(1/10)
      
      no_of_clinics <<- nrow(subzone_clinics[which(subzone_clinics$capacity > 0), ])
      clinics_remaining <<- subzone_clinics[which(subzone_clinics$capacity > 0), ]
      
      total_unallocated_elderly <<- total_unallocated_elderly %>%
        select("blk_no_street","LAT","LONG","SUBZONE_N")
      
      # convert both data.frames to SpatialPointsDataFrame
      clinics_remaining_sp <<- as_Spatial(clinics_remaining)
      total_unallocated_elderly_sp <<- as_Spatial(total_unallocated_elderly)
      
      # run ALLOCATION algorithm again
      alloc_results <<- allocations(total_unallocated_elderly_sp,
                                   clinics_remaining_sp, p=no_of_clinics)
      
      alloc_results <<- st_as_sf(alloc_results, coords = c("LONG", "LAT"), crs = st_crs(mpsz))
      alloc_results <<- alloc_results[order(alloc_results$allocation, alloc_results$allocdist),]
      
      # create empty data frame with header columns
      total_unallocated_elderly <<- alloc_results[0,]
      
      for(i in 1:nrow(clinics_remaining)) {
        clinic_row <<- clinics_remaining[i,]
        
        # get the total elderlys allocated to this allocation ID
        clinic_n_allocation <<- alloc_results[which(alloc_results$allocation == i), ]
        
        clinic_capacity <<- subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name]
        
        if(nrow(clinic_n_allocation) >= clinic_capacity) {
          subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name] <<- 0
          
          # get the unallocated elderlys, append to total unallocated elderlys
          unallocated_elderly <<- clinic_n_allocation %>% slice(clinic_capacity+1:nrow(clinic_n_allocation))
          total_unallocated_elderly <<- rbind(total_unallocated_elderly, unallocated_elderly)
          
          total_allocated_elderly <<- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:clinic_capacity))
          
        } else if(nrow(clinic_n_allocation) < clinic_capacity) {
          # calculate the remaining capacity
          subzone_clinics$capacity[subzone_clinics$clinic_name == clinic_row$clinic_name] <<- clinic_capacity - nrow(clinic_n_allocation)
          
          total_allocated_elderly <<- rbind(total_allocated_elderly, clinic_n_allocation %>% slice(1:nrow(clinic_n_allocation)))
        }
      }
    }
  })
  
  total_unallocated_elderly <<- total_unallocated_elderly %>% select("blk_no_street","LAT","LONG","SUBZONE_N")
}

# Function to combine allocated, unallocated and half-allocated HDB elderly and denote
# each of them with different colors (green, red, orange)
colorOnHDB <- function(allocated, unallocated) {
  final_results <<- full_join(unallocated, allocated) %>% mutate(color = "orange")
  final_results[is.na(final_results)] <<- 0
  
  for(i in 1:nrow(final_results)) {
    row <- final_results[i,]
    if(row$no_of_elderly_allocated == 0) {
      final_results$color[i] <<- "red"
    } else if(row$no_of_elderly_unallocated == 0) {
      final_results$color[i] <<- "green"
    }
  }
  
  final_results <<- full_join(final_results, mpsz_HDB_filtered) %>% 
    select("blk_no_street","no_of_elderly_allocated", "no_of_elderly_unallocated", 
           "LAT","LONG", "color", "geometry")
}
