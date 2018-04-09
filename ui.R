
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
      
      #change between 12/24 hours time formats
      materialSwitch(inputId = "time", label = "24 Time Format", status = "primary", right = TRUE, value = TRUE),
      
      #change between imperial and metric formats
      materialSwitch(inputId = "metric", label = "Imperial", status = "primary", right = TRUE, value = TRUE),
      
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
              fluidRow(
                box(title = "MONTHLY Total Tornadoes by Magnitude - IL - 1950 to 2009", solidHeader = TRUE, status = "primary", width = 12,
                      tabBox(
                        id = "tab_monthyTotalsInILGraphs", height = "850px",
                        tabPanel("Total Numbers Monthly", plotlyOutput("magTotalMonthChart", height = 800)),
                        tabPanel("Total Percent Monthly", plotlyOutput("magTotalMonthChartPercent", height = 800)),
                        width = 12
                      )
                  ),
                box(title = "MONTHLY Total Tornadoes by Magnitude - IL - 1950 to 2009", status = "primary", solidHeader = TRUE, width = 12,
                    tabBox(
                      id = "tab_monthlyTotalsInILTables", height = "850px",
                      tabPanel("Total Numbers Monthly", div(DT::dataTableOutput("magTotalMonthTable", height = 300), style = "font-size: 200%")),
                      tabPanel("Total Percent Monthly", div(DT::dataTableOutput("magTotalMonthTablePercent", height = 300), style = "font-size: 200%")),
                      width = 12
                    )
                ),
                box(title = "HOURLY Total Tornadoes by Magnitude - IL - 1950 to 2009", solidHeader = TRUE, status = "primary", width = 12,
                    tabBox(
                      id = "tab_monthyTotalsInILGraphs", height = "850px",
                      tabPanel("Total Numbers Hourly", plotlyOutput("magTotalHourChart", height = 800)),
                      tabPanel("Total Percent Hourly", plotlyOutput("magTotalHourChartPercent", height = 800)),
                      width = 12
                    )
                ),
                box(title = "HOURLY Total Tornadoes by Magnitude - IL - 1950 to 2009", status = "primary", solidHeader = TRUE, width = 12,
                    tabBox(
                      id = "tab_monthlyTotalsInILTables", height = "850px",
                      tabPanel("Total Numbers Hourly", 
                               div(DT::dataTableOutput("magTotalHourTableI", height = 300), style = "font-size: 200%"),
                               div(DT::dataTableOutput("magTotalHourTableII", height = 300), style = "font-size: 200%")),
                      tabPanel("Total Percent Hourly",
                               div(DT::dataTableOutput("magTotalHourTablePercentI", height = 300), style = "font-size: 200%"),
                               div(DT::dataTableOutput("magTotalHourTablePercentII", height = 300), style = "font-size: 200%")),
                      width = 12
                    )
                ),
                box(title = "MONTHLY Deaths, Injuries, and Losses - IL - 1950 to 2009", status = "primary", solidHeader = TRUE, width = 12,
                    tabBox(
                      id = "damagesByMonth", height = "850px",
                      tabPanel("Injuries", plotlyOutput("injuriesChartByMonth", height = 800)),
                      tabPanel("Deaths", plotlyOutput("deathsChartByMonth", height = 800)),
                      tabPanel("Property Loss", plotlyOutput("lossChartByMonth", height = 800)),
                      width = 12
                    ),
                    box(status = "primary", solidHeader = TRUE, width = 12,
                        div(DT::dataTableOutput("totalDamagesByMonthTable", height = 800), style = "font-size: 200%")
                    )   
                )
                )
      )
    )

  )
)
