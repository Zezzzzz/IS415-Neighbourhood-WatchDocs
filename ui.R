#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(leaflet)
library(shiny)
library(rsconnect)

navbarPage("NeighbourhoodWatchDocs", id="nav",
  tabPanel("Interactive Map", 
    sidebarLayout(
      sidebarPanel(
        radioButtons(
          inputId = "analysisType", 
          label = "Select Analysis Type",
          choices = c("Geographical Accessibility" = "geoAcc", "Clinic Allocation Analysis" = "allocation")
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
          choices = subzone_list$subzone, 
          selected = "ADMIRALTY"
          ),
        
        conditionalPanel(
          condition = "input.analysisType == 'geoAcc'",
          selectInput(
            inputId = "accMethod", 
            label = "Spatial Accessibility Method",
            choices = c("SAM" = "SAM", "Hansen" = "Hansen"), 
            selected = "SAM"
            )
          # conditionalPanel(
          #   condition = "input.accMethod == 'Hansen'",
          #   sliderInput(
          #     inputId = "power",
          #     label = "Power Separation",
          #     min = 1,
          #     max = 2,
          #     value = 2,
          #     step = 0.1
          #   )
          # )
        ),
        
        conditionalPanel(
          condition = "input.analysisType == 'allocation'",
          numericInput(
            inputId = "cCapacity",
            label = "Clinic Capacity",
            min = 80,
            max = 160,
            value = 80
            )
          ),

        uiOutput("interaction_slider")
      ),
      
      mainPanel(
        leafletOutput("map",height = 500),
        textOutput("selected_var"),
        conditionalPanel(
          condition = "input.analysisType == 'allocation'",
          hr(),
          htmlOutput("total_elderly_in_subzone"),
          hr(),
          htmlOutput("total_elderly_allocated"),
          hr(),
          htmlOutput("total_elderly_unallocated"),
          hr(),
          htmlOutput("total_alloc_dist"),
          hr(),
          h3("Allocation Result (Blocks & Elderly Count)"),
          dataTableOutput("allocation_result_output")
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
