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

# Define server logic
shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 103.8509, lat = 1.3800, zoom = 12)
  })
  
  
  observeEvent(c(input$Type, input$subzone, input$distance), {
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
        acc_sam<- data.frame(ac(hdb_results()$No_of_Elderly_in_block,
                                    clinics$capacity, dm, d0 = input$distance,
                                    power = 2, family = "SAM"))
        colnames(acc_sam) <- "accSam"
        acc_sam <- tbl_df(acc_sam)
        acc_sam <- lapply(acc_sam, function(x) replace(x, !is.finite(x), 0))
        
        HDB_acc <- bind_cols(hdb_results(), acc_sam)
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
      
      if(!is.null(acc_results())) {
        print(acc_results()$accSam)
        pal = colorQuantile("Greens", n = 5, acc_results()$accSam)
        leafletProxy("map", data = acc_results()) %>%
          clearMarkers() %>%
          addCircleMarkers(lng = ~LONG,
                           lat = ~LAT,
                           popup = paste("", acc_results()$blk_no_street, "<br><br>",
                                         "Acc-SAM: ", acc_results()$accSam),
                           color = ~pal(acc_results()$accSam), 
                           fillOpacity = 0.8) %>%
          clearControls() %>%
          addLegend(pal = pal, values = ~accSam, opacity = 0.8, position = "bottomright")
      }
      
    } else if(c("clinics_combined") %in% input$Type) {
      leafletProxy("map", data = clinic_results()) %>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", clinic_results()$clinic_name, "<br><br>",
                                        "", clinic_results()$address),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
    } else if(c("hdb") %in% input$Type) {
      leafletProxy("map", data = hdb_results()) %>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", hdb_results()$blk_no_street, "<br><br>",
                                        "Elderly Population: ", hdb_results()$No_of_Elderly_in_block),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      
    } else {
      leafletProxy("map", data = NULL) %>%
        clearMarkers()
    }
  })
})
