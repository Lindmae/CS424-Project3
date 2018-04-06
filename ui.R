
#libraries to include
#comment in as needed, to find which ones are not needed
library(shiny)
#library(shinyjs)
library(shinydashboard)
#library(data.table)
library(ggplot2)
library(lubridate)
library(DT)
#library(jpeg)
library(grid)
library(leaflet)
#library(reshape2)
#library(scales)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinycssloaders) #needed for loading bars
library(gdata) #needed for xls files 

# load any processed data here
#format: load("rdata/datafile.RData")


# start up the gui
ui <- dashboardPage(
  #ensure that map will be the correct size 
    #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Isabel", icon = icon("plane", lib = "font-awesome"), tabName = "isabel"),

      menuItem("Bart", icon = icon("plane", lib = "font-awesome"), tabName = "bart"),
      
      menuItem("Vijay", icon = icon("plane", lib = "font-awesome"), tabName = "vijay"),
      
      
      #info
      menuItem("Info", tabName = "info", icon = icon("th"))
    )
  ),
  dashboardBody(

    tabItems(
      tabItem(tabName = "isabel",
              fluidRow( 
                tabBox(
                  #title = "First tabBox",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset1", height = "850px",
                  tabPanel("Injuries", plotlyOutput("injuriesChart", height = 800)),
                  tabPanel("Deaths", plotlyOutput("deathsChart", height = 800)),
                  tabPanel("Property Loss", plotlyOutput("lossChart", height = 800)),
                  width = 12
                ),
                box(status = "primary", solidHeader = TRUE, width = 12,
                    div(DT::dataTableOutput("totalDamagesTable", height = 800), style = "font-size: 200%")
                )
                )
      ),
      tabItem(tabName = "bart",
              fluidRow( box(title = "Tornado tracks across Illinois", solidHeader = TRUE, status = "primary", width = 6),
                        box(title = "Map", solidHeader = TRUE, status = "primary", width = 12,
                            leafletOutput("map")
                        ))
      ),
      tabItem(tabName = "vijay",
              fluidRow( box(title = "vijay Title", solidHeader = TRUE, status = "primary", width = 6))
      )
    )

  )
)
