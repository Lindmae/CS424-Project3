
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
              fluidRow( box(title = "isabel Title", solidHeader = TRUE, status = "primary", width = 6))
      ),
      tabItem(tabName = "bart",
              fluidRow( box(title = "bart Title", solidHeader = TRUE, status = "primary", width = 6))
      ),
      tabItem(tabName = "vijay",
              fluidRow( box(title = "vijay Title", solidHeader = TRUE, status = "primary", width = 6))
      )
    )

  )
)
