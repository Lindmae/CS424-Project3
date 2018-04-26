
#libraries to include
#comment in as needed, to find which ones are not needed
library(shiny)
library(shinyjs)
library(shinydashboard)
#library(data.table)
library(ggplot2)
library(lubridate)
library(DT)
#library(jpeg)
library(grid)
library(leaflet)
library(reshape2)
#library(scales)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinycssloaders) #needed for loading bars
library(gdata) #needed for xls files 
library(RColorBrewer)
library(geojsonio)
library(geosphere)

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
    #below is all the styling for the map and it's controls 
    tags$style(type = "text/css", "#map {/* make map taller */  height: calc(100vh - 240px) !important;}
      .irs-grid-text {font-size: 100%;color:black;}
      .irs-grid-pol {display: none;}
       .checkbox { /* checkbox is a div class*/
        line-height: 30px;
        margin-bottom: 40px; /*set the margin, so boxes don't overlap*/
      }
      input[type='checkbox']{ /* style for checkboxes */
        width: 30px; /*Desired width*/
        height: 30px; /*Desired height*/
        line-height: 30px; 
      }
      input[type='radio']{ /* style for radio buttons */
        width: 30px; /*Desired width*/
        height: 30px; /*Desired height*/
        line-height: 30px; 
      }
      .checkbox-inline, .radio-inline span { 
          margin-left: 15px;  /*set the margin, so boxes don't overlap labels... BUT IT MISALIGNS OUR SIDEBAR -- VIJAY*/
          line-height: 30px; 
      }
       .checkbox-inline span { 
          margin-left: 15px;  /*set the margin, so boxes don't overlap labels... BUT IT MISALIGNS OUR SIDEBAR -- VIJAY*/
          line-height: 30px; 
      }         "),
    tabItems(tabItem(tabName = "info",
                     fluidRow(
                       column(2,""
                       ),
                       column(8,
                              p(" Written by:"),
                              h2("Bartosz Kupiec, Isabel Lindmae, Vijay Mahida"),
                              br(),
                              p("Project 3 for CS 424 Spring 2018 UIC"),
                              br(),
                              p("Libraries used: shiny, shinyjs, 
                                shinydashboard, 
                                ggplot2, 
                                lubridate, 
                                DT, 
                                grid, 
                                leaflet, 
                                reshape2, 
                                dplyr, 
                                plotly, 
                                shinyWidgets, 
                                shinycssloaders,  gdata,  RColorBrewer, 
                                geojsonio, 
                                geosphere."),
                              p("Data from: NOAA's National Weather Service ", a("www.spc.noaa.gov/wcm/index.html#data", href="www.spc.noaa.gov/wcm/index.html#data", target="blank"), ")"),
                              br(),
                              p("For project requirements visit here: ", a("www.evl.uic.edu/aej/424/", href="www.evl.uic.edu/aej/424/", target="blank")),
                              br(),
                              p("For more information/analysis visit: ", a("Website goes here", href="", target="blank"))
                       ),
                       column(2,""
                       )
                     )
    ),
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
                ),
                tabBox(
                  #title = "First tabBox",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  id = "tabset9", height = "850px",
                  tabPanel("Without", plotlyOutput("countyGraphWithout", height = 800)),
                  tabPanel("With", plotlyOutput("countyGraph", height = 800)),
                  width = 12
                ),
                box(status = "primary", solidHeader = TRUE, width = 12,
                    div(plotlyOutput("yearlyGraph", height = 800), style = "font-size: 200%"),
                    div(plotlyOutput("yearlyGraphPer", height = 800), style = "font-size: 200%")
                ),
                box(status = "primary", solidHeader = TRUE, width = 3,
                    div(DT::dataTableOutput("yearlyTornadoTable", height = 800), style = "font-size: 200%")
                ),
                box(status = "primary", solidHeader = TRUE, width = 12,
                    div(DT::dataTableOutput("eDistanceTable", height = 800), style = "font-size: 200%"),
                    div(plotlyOutput("distanceCountGraph", height = 800), style = "font-size: 200%")
                    
                     )
                )
              
      ),
      tabItem(tabName = "bart",
              fluidRow(div(
                        column(12, box(title = "Map", solidHeader = TRUE, status = "primary", width = 24,
                            leafletOutput("map")))), style = "font-size: 300%"),
              absolutePanel(top = 150, right = 50,box(title = "Tornado tracks across Illinois", solidHeader = TRUE, status = "primary",width = 12,
                           checkboxGroupInput("magnitudes", "Magnitudes to show:",
                                              choices=c(0,1,2,3,4,5,"unknown" = -9), inline = TRUE,selected = c(1,2)),
                           radioButtons("mapColor","Tornado tracks with color based on :",
                                        choices=c("magnitude" = "mag", "length" = "len", "width" = "wid",
                                                  "loss" = "loss", "injuries" = "inj", "fatalities" = "fat"),
                                        inline = TRUE,selected = "len"),
                           radioButtons("mapWidth","Tornado tracks with width based on :",
                                        choices=c("magnitude" = "mag", "length" = "len", "width" = "wid", 
                                                  "loss" = "loss", "injuries" = "inj", "fatalities" = "fat"), 
                                        inline = TRUE,selected = "loss"),
                           # Sliders for all the different values
                           #val = Length 
                           sliderInput("mapLenSlider", label = "Length Range", min = 0, 
                                       max = 240, value = c(0, 240)),
                           #val = Width 
                           sliderInput("mapWidthSlider", label = "Width Range", min = 0, 
                                       max = 4600, value = c(0, 4600)),
                           #val = Loss
                           sliderInput("mapLossSlider", label = "Loss($ in million/s) Range", min = 0, 
                                       max = 22, value = c(0, 22),step = .01),
                           #val = injury
                           sliderInput("mapInjurySlider", label = "Injury Range", min = 0, 
                                       max = 1750, value = c(0, 1750)),
                           #val = fatalities 
                           sliderInput("mapFatSlider", label = "Fatality Range", min = 0, 
                                       max = 160, value = c(0, 160)),
                           #val = year 
                           checkboxInput("yearChoice", "Specific year", FALSE),
                           sliderInput("mapYearSlider", label = "Year Range", min = 1950, 
                                       max = 2016, value = c(1950, 2016),sep="",step = 1,animate = TRUE),
                           #below here is formatting for the map / colors 
                           checkboxInput("legend", "Show legend", TRUE),
                           #val = theme 
                           selectInput("colors", "Color Scheme",
                                       rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                           ),
                           selectInput("topTornado", label = h3("Choose a tornado.."), 
                                       choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3))
                           
              ))
      ),
      tabItem(tabName = "vijay",
              tabsetPanel(
              tabPanel("Monthly",
                    fluidRow(
                      box(title = "MONTHLY Total Tornadoes by Magnitude - IL - 1950 to 2016", solidHeader = TRUE, status = "primary", width = 12,
                            tabBox(
                              id = "tab_monthyTotalsInILGraphs", height = "850px",
                              tabPanel("Total Numbers Monthly", plotlyOutput("magTotalMonthChart", height = 800)),
                              tabPanel("Total Percent Monthly", plotlyOutput("magTotalMonthChartPercent", height = 800)),
                              width = 12
                            )
                        ),
                      box(title = "MONTHLY Total Tornadoes by Magnitude - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                          tabBox(
                            id = "tab_monthlyTotalsInILTables", height = "850px",
                            tabPanel("Total Numbers Monthly", div(DT::dataTableOutput("magTotalMonthTable", height = 300), style = "font-size: 200%")),
                            tabPanel("Total Percent Monthly", div(DT::dataTableOutput("magTotalMonthTablePercent", height = 300), style = "font-size: 200%")),
                            width = 12
                          )
                      )
                    )
              ),
              tabPanel("Hourly",
                  fluidRow(
                  box(title = "HOURLY Total Tornadoes by Magnitude - IL - 1950 to 2016", solidHeader = TRUE, status = "primary", width = 12,
                      tabBox(
                        id = "tab_monthyTotalsInILGraphs", height = "850px",
                        tabPanel("Total Numbers Hourly", plotlyOutput("magTotalHourChart", height = 800)),
                        tabPanel("Total Percent Hourly", plotlyOutput("magTotalHourChartPercent", height = 800)),
                        width = 12
                      )
                  ),
                  box(title = "HOURLY Total Tornadoes by Magnitude - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
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
                  )
                  )
                ),
                tabPanel("Damages Monthly",
                  fluidRow(
                  box(title = "MONTHLY Deaths, Injuries, and Losses - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
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
                ),
                tabPanel("Damages Hourly",
                  fluidRow(
                  box(title = "HOURLY Deaths, Injuries, and Losses - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                      tabBox(
                        id = "damagesByHour", height = "850px",
                        tabPanel("Injuries", plotlyOutput("injuriesChartByHour", height = 800)),
                        tabPanel("Deaths", plotlyOutput("deathsChartByHour", height = 800)),
                        tabPanel("Property Loss", plotlyOutput("lossChartByHour", height = 800)),
                        width = 12
                      ),
                      box(status = "primary", solidHeader = TRUE, width = 12,
                          div(DT::dataTableOutput("totalDamagesByHourTable", height = 800), style = "font-size: 200%")
                      )   
                  )
                  )
                ),
              tabPanel("Damages by County",
                       fluidRow(
                         box(title = "COUNTY Deaths, Injuries, and Losses - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                             tabBox(
                               id = "damagesByCounty", height = "850px",
                               tabPanel("County Map", leafletOutput("countyMap")),
                               width = 12
                             ),
                             box(status = "primary", solidHeader = TRUE, width = 12,
                                 div(DT::dataTableOutput("countyDataILTable", height = 800), style = "font-size: 200%")
                             )   
                         )
                       )
              )
              )
        )
    )
  )
)
