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
        radioButtons(
          inputId = "analysisType", 
          label = "Select Analysis Type",
          choices = c("Geographical Accessibility" = "geoAcc", "Optimal Allocation Analysis" = "pMed")
          ),
        
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
          selected = "ADMIRALTY"
          ),
        
        conditionalPanel(
          condition = "input.analysisType == 'geoAcc'",
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
            )
          
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'pMed'",
          sliderInput(
            inputId = "initialP",
            label = "Initial P-median set",
            min = 1,
            max = 10,
            value = 1,
            step = 1
            )
          ),

        uiOutput("interaction_slider")
      ),
      
      mainPanel(
        leafletOutput("map",height = 500),
        textOutput("selected_var"),
        conditionalPanel(
          condition = "input.analysisType == 'pMed'",
          dataTableOutput("viewDataTable2")
        )
      )
    )
  ),
  
  tabPanel("Data Explorer",
    fluidRow(
      column(12,
        selectInput("selectDT", "Select Data to View: ", 
                    c("Number of Elderly in each HDB Block" = "elderly_per_hdb", 
                      "Number of Clinics (GP & TCM)" = "total_clinics"), 
                    selected = "elderly_per_hdb"
        )
      )
    ),
    hr(),
    dataTableOutput("viewDataTable")
  )
)
