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
library(rgeos)
library(proj4)

# Define server logic
shinyServer(function(input, output, session) {
  
  ## Logic for Interactive Map Tab ##
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 103.8509, lat = 1.3800, zoom = 12)
  })
  
  observeEvent(c(input$subzone),{
    updateSliderInput(session, "initialP", value = nrow(mpsz_clinics %>% filter(SUBZONE_N == input$subzone)),
                    min = 1, max = nrow(mpsz_clinics %>% filter(SUBZONE_N == input$subzone)),
                    step = 1)
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
        ## edit this
        setView(lng = latlon$lon, lat = latlon$lat, zoom = 16)
    })
  })
  


  observeEvent(c(input$map_click, input$subzone, input$initialP, input$analysisType,input$Type), {
    
    if(all(c("clinics_combined", "hdb") %in% input$Type)) {
      click <- input$map_click
      if(!is.null(click) & input$analysisType == 'pMed'){
        clicked = TRUE
        text<-paste("Latitude ", round(click$lat,2), "Longtitude ", round(click$lng,2))
        proxy <- leafletProxy("map")
        
        
        ## This displays the pin drop circle
        proxy %>%
          #clearPopups() %>%
          #clearMarkers(layerId=input$map_click) %>%
          #addPopups(click$lng, click$lat) %>%
          clearShapes()%>%
          addCircles(click$lng, click$lat, radius=15, color="red")
        
        clinics_of_subzone <- mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
        
        if(nrow(clinics_of_subzone) > 0){
          
          clinics_of_subzone <- clinics_of_subzone %>% select(clinic_name, LAT, LONG, SUBZONE_N) %>%
            st_set_geometry(NULL)
          
          clinics_of_subzone$LONG1 <- clinics_of_subzone$LONG
          clinics_of_subzone$LAT1 <- clinics_of_subzone$LAT
          
          clinics_of_subzone <- st_as_sf(clinics_of_subzone, coords = c("LONG1","LAT1"),
                                         crs = 4326)
          
          #print(clinics_of_subzone)
          newRow <- data.frame(clinic_name="New Clinic",
                               LAT=click$lat,
                               LONG = click$lng,
                               LAT1=click$lat,
                               LONG1 = click$lng,
                               SUBZONE_N = clinics_of_subzone$SUBZONE_N[1])
          
          newRow_sf <- st_as_sf(newRow, coords = c("LONG1", "LAT1"),
                                crs = 4326)
          
          clinics_of_subzone <- rbind(clinics_of_subzone, newRow_sf)
          
          #print(clinics_of_subzone)
          
          pMed_results <- reactive({
            #clinics_of_subzone <- mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
            HDB_of_subzone <- mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
            HDB_of_subzone <- HDB_of_subzone %>% select(blk_no_street, LAT, LONG, SUBZONE_N, No_of_Elderly_in_block) %>%
              st_set_geometry(NULL)
            HDB_of_subzone$LONG1 <- HDB_of_subzone$LONG
            HDB_of_subzone$LAT1 <- HDB_of_subzone$LAT
            HDB_of_subzone <- st_as_sf(HDB_of_subzone, coords = c("LONG1","LAT1"),
                                       crs = 4326)
            if(nrow(HDB_of_subzone) != 0){
              clinics_of_subzone_sp <- as_Spatial(clinics_of_subzone)
              HDB_of_subzone_sp <- as_Spatial(HDB_of_subzone)
              allocation <- allocations(HDB_of_subzone_sp, clinics_of_subzone_sp, p=input$initialP + 1)
            }
          })
          
          
          
          if(!is.null(pMed_results())) {
            
            pal = colorQuantile("Blues", n = 5, pMed_results()$allocdist)
            leafletProxy("map", data = pMed_results()) %>%
              clearMarkers() %>%
              addCircleMarkers(lng = ~LONG,
                               lat = ~LAT,
                               popup = paste("", pMed_results()$blk_no_street, "<br><br>",
                                             "Allocation-Dist: ", pMed_results()$allocdist),
                               color = "black",
                               fillColor = ~pal(pMed_results()$allocdist),
                               fillOpacity = 0.8) %>%
              clearControls() %>%
              addLegend(pal = pal, values = ~allocdist, opacity = 0.8, position = "bottomright")
          }
        }
      }
    } else if(c("clinics_combined") %in% input$Type) {
      clinic_results <- reactive({
        mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
      })
      leafletProxy("map", data = clinic_results()) %>%
        #clearShapes()%>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", clinic_results()$clinic_name, "<br><br>",
                                        "", clinic_results()$address),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
    }
    

  })
  
  observeEvent(c(input$Type, input$subzone, input$accMethod, input$power, 
                 input$initialP, input$analysisType), {
    
      
    clinic_results <- reactive({
      mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
    })
    
    hdb_results <- reactive({
      mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
    })
    
    
    
    acc_results <- reactive({
      clinics_coords <- clinic_results() %>%
        st_coordinates()
      hdb_coords <- hdb_results() %>% 
        st_coordinates()
       
      # check that reactive expr return value is not empty before proceeding
      if(nrow(clinics_coords) != 0 & nrow(hdb_coords) != 0) {
        capacity <- round(sum(hdb_results()$No_of_Elderly_in_block) / nrow(clinic_results()))
        
        clinics <- clinic_results() %>% mutate(`capacity` = capacity)
        
        dm <- distance(hdb_coords, clinics_coords)
        acc_val <- data.frame(ac(hdb_results()$No_of_Elderly_in_block,
                                    clinics$capacity, dm,
                                    power = input$power, family = input$accMethod))
        colnames(acc_val) <- "accVal"
        acc_val <- tbl_df(acc_val)
        acc_val <- lapply(acc_val, function(x) replace(x, !is.finite(x), 0))
        
        HDB_acc <- bind_cols(hdb_results(), acc_val)
      }
    })
# 
#     clinics_of_subzone <- mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
#     HDB_of_subzone <- mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
#     clinics_of_subzone_sp <- as_Spatial(clinics_of_subzone)
#     HDB_of_subzone_sp <- as_Spatial(HDB_of_subzone)

    # output$interaction_slider <- renderUI(
    #   sliderInput(
    #     inputId = "testp",
    #     label = "Power Separation",
    #     min   = 1,
    #     max   = nrow(mpsz_clinics %>% filter(SUBZONE_N == input$subzone)),
    #     value = nrow(mpsz_clinics %>% filter(SUBZONE_N == input$subzone)),
    #     step = 1
    #     )
    # )
    
    
    
    pMed_results <- reactive({
      HDB_of_subzone <- mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
      # HDB_of_subzone <- HDB_of_subzone %>% select(blk_no_street, LAT, LONG, SUBZONE_N, No_of_Elderly_in_block) %>% 
      #   st_set_geometry(NULL)
      # HDB_of_subzone$LONG1 <- HDB_of_subzone$LONG
      # HDB_of_subzone$LAT1 <- HDB_of_subzone$LAT
      # HDB_of_subzone <- st_as_sf(HDB_of_subzone, coords = c("LONG1","LAT1"),
      #                            crs = 4326)
        # print("TRUE")
        # }else{
          clinics_of_subzone <- mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
        # }
      if(nrow(clinics_of_subzone) != 0 & nrow(HDB_of_subzone) != 0){
        clinics_of_subzone_sp <- as_Spatial(clinics_of_subzone)
        HDB_of_subzone_sp <- as_Spatial(HDB_of_subzone)
        allocation <- allocations(HDB_of_subzone_sp, clinics_of_subzone_sp, p=input$initialP)
      }
    })
    
    

    if(all(c("clinics_combined", "hdb") %in% input$Type)) {
      # leafletProxy("map", data = clinic_results()) %>%
      #   clearMarkers() %>%
      #   addAwesomeMarkers(lng = ~LONG,
      #                     lat = ~LAT,
      #                     popup = paste("", clinic_results()$clinic_name, "<br><br>",
      #                                   "", clinic_results()$address),
      #                     icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      # 
      # leafletProxy("map", data = hdb_results()) %>%
      #   addAwesomeMarkers(lng = ~LONG,
      #                     lat = ~LAT,
      #                     popup = paste("", hdb_results()$blk_no_street, "<br><br>",
      #                                   "Elderly Population: ", hdb_results()$No_of_Elderly_in_block),
      #                     icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      #print(clicked)

      if(!is.null(acc_results()) & input$analysisType == 'geoAcc') {
        #print(acc_results()$accVal)
        pal = colorQuantile("Purples", n = 5, acc_results()$accVal)
        leafletProxy("map", data = acc_results()) %>%
          clearShapes()%>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LONG,
                           lat = ~LAT,
                           popup = paste("", acc_results()$blk_no_street, "<br><br>",
                                         "Acc-Val: ", acc_results()$accVal),
                           color = "black",
                           fillColor = ~pal(acc_results()$accVal),
                           fillOpacity = 0.8) %>%
          clearControls() %>%
          addLegend(pal = pal, values = ~accVal, opacity = 0.8, position = "bottomright")
      }
      
      
      else if(!is.null(pMed_results()) & input$analysisType == 'pMed') {
        #print(pMed_results()$LONG)
        pal = colorQuantile("Blues", n = 5, pMed_results()$allocdist)
        leafletProxy("map", data = pMed_results()) %>%
          #clearShapes()%>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LONG,
                           lat = ~LAT,
                           popup = paste("", pMed_results()$blk_no_street, "<br><br>",
                                         "Allocation-Dist: ", pMed_results()$allocdist),
                           color = "black",
                           fillColor = ~pal(pMed_results()$allocdist),
                           fillOpacity = 0.8) %>%
          clearControls() %>%
          addLegend(pal = pal, values = ~allocdist, opacity = 0.8, position = "bottomright")
      }
      
      # else if(!is.null(pMed_results()) & input$analysisType == 'pMed' & clicked == TRUE) {
      #   #print(pMed_results()$LONG)
      #   pal = colorQuantile("Blues", n = 5, pMed_results()$allocdist)
      #   leafletProxy("map", data = pMed_results()) %>%
      #     clearMarkers() %>%
      #     addCircleMarkers(lng = ~LONG,
      #                      lat = ~LAT,
      #                      popup = paste("", pMed_results()$blk_no_street, "<br><br>",
      #                                    "Allocation-Dist: ", pMed_results()$allocdist),
      #                      color = ~pal(pMed_results()$allocdist), 
      #                      fillOpacity = 0.8) %>%
      #     clearControls() %>%
      #     addLegend(pal = pal, values = ~allocdist, opacity = 0.8, position = "bottomright")
      # }
      
    } else if(c("clinics_combined") %in% input$Type) {
      leafletProxy("map", data = clinic_results()) %>%
        #clearShapes()%>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", clinic_results()$clinic_name, "<br><br>",
                                        "", clinic_results()$address),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
    } else if(c("hdb") %in% input$Type) {
      leafletProxy("map", data = hdb_results()) %>%
        clearShapes()%>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", hdb_results()$blk_no_street, "<br><br>",
                                        "Elderly Population: ", hdb_results()$No_of_Elderly_in_block),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      
    } else {
      leafletProxy("map", data = NULL) %>%
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
