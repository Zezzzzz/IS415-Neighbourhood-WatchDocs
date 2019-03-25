#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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
library(dplyr)
library(SpatialAcc)
library(leaflet)
library(shiny)

navbarPage("NeighbourhoodWatchDocs", id="nav",
  tabPanel("Interactive Map", 
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(
          inputId = "Type", 
          label = "Select Type",
          choices = c("Clinics" = "clinics_combined", "HDB" = "hdb"),
          selected = c("clinics_combined", "hdb")
        ),
        
        selectInput(
          inputId = "subzone", 
          label = "Select Subzone",
          choices = mpsz$SUBZONE_N, 
          selected = "NORTHLAND"
        ),
        
        selectInput(
          inputId = "accMethod", 
          label = "Spatial Accessibility Method",
          choices = c("SAM" = "SAM", "Hansen" = "Hansen"), 
          selected = "SAM"
        ),
        
        sliderInput(
          inputId = "power",
          label = "Power Separation",
          min = 0.01,
          max = 2,
          value = 2,
          step = 0.01
        ),
        
        sliderInput(
          inputId = "initialP",
          label = "Initial P-median set",
          min = 0,
          max = 10,
          value = 1,
          step = 1
        )
      ),
      
      mainPanel(
        leafletOutput("map",height = 500)
      )
    )
  ),
  tabPanel("Data Table")
)
