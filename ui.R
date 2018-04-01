
#libraries to include
#comment in as needed, to find which ones are not needed
library(shiny)
#library(shinyjs)
library(shinydashboard)
#library(data.table)
library(ggplot2)
#library(lubridate)
#library(DT)
#library(jpeg)
library(grid)
library(leaflet)
#library(reshape2)
#library(scales)
#library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinycssloaders) #needed for loading bars
library(gdata) #needed for xls files 

# load any processed data here
#format: load("rdata/datafile.RData")


# start up the gui
ui <- dashboardPage(
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 3"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Stuff", icon = icon("plane", lib = "font-awesome"), tabName = "atab"),

      #info
      menuItem("Info", tabName = "info", icon = icon("th"))
    )
  ),
  dashboardBody(

    tabItems(
      tabItem(tabName = "atab",
              fluidRow( box(title = "Default Title", solidHeader = TRUE, status = "primary", width = 6))
        )
    )

  )
)
