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
  
  
  observeEvent(c(input$clinicType, input$subzone), {
    req(input$clinicType)
    req(input$subzone)
    
    #select subzone
    selectedSubzone <- reactive({
      if(input$clinicType == "gpclinics") {
        clinics_sf %>% filter(SUBZONE_N == input$subzone)
      } else {
        tcm_sf %>% filter(SUBZONE_N == input$subzone)
      }
    })
    
    if(input$clinicType == "gpclinics") {
      leafletProxy("map", data = selectedSubzone()) %>%
          clearMarkers() %>%
          addAwesomeMarkers(lng = ~LONG,
                     lat = ~LAT,
                     popup = paste("", selectedSubzone()$clinic_name, "<br><br>",
                                   "", selectedSubzone()$address), 
                     icon = makeAwesomeIcon(icon = "icon", markerColor = "blue"))
      
    } else {
      leafletProxy("map", data = selectedSubzone()) %>%
          clearMarkers() %>%
          addAwesomeMarkers(lng = ~LONG,
                     lat = ~LAT,
                     popup = paste("", selectedSubzone()$tcm_place_name, "<br><br>",
                                   "", selectedSubzone()$tcm_address), 
                     icon = makeAwesomeIcon(icon = "icon", markerColor = "orange"))
      
    }
  })
})
