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
library(leaflet)
library(shiny)

# Define server logic
shinyServer(function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>% 
      setView(lng = 103.8509, lat = 1.3800, zoom = 12)
  })
  
  
  observeEvent(c(input$Type, input$subzone), {
    #req(input$Type)
    #req(input$subzone)
    
    clinic_results <- reactive({
      mpsz_clinics %>% filter(SUBZONE_N == input$subzone)
    })
    
    hdb_results <- reactive({
      mpsz_HDB %>% filter(SUBZONE_N == input$subzone)
    })
    
    if(all(c("clinics_combined", "hdb") %in% input$Type)) {
      leafletProxy("map", data = clinic_results()) %>%
        clearMarkers() %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", clinic_results()$clinic_name, "<br><br>",
                                        "", clinic_results()$address),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
      leafletProxy("map", data = hdb_results()) %>%
        addAwesomeMarkers(lng = ~LONG,
                          lat = ~LAT,
                          popup = paste("", hdb_results()$blk_no_street, "<br><br>",
                                        "Elderly Population: ", hdb_results()$No_of_Elderly_in_block),
                          icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      
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
