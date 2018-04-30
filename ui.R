
#libraries to include
#comment in as needed, to find which ones are not needed
library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(grid)
library(leaflet)
library(reshape2)
library(scales)
library(dplyr)
library(plotly)
library(shinyWidgets)
library(shinycssloaders) #needed for loading bars
library(gdata) #needed for xls files 
library(geosphere)
library(tigris) # to grab census data on state, counties, roads, etc.
library(sp)
library(colourpicker)

# load any processed data here
#format: load("rdata/datafile.RData")


# start up the gui
ui <- dashboardPage(
  
  #ensure that map will be the correct size 
  #set title and disable sidebar
  dashboardHeader(title = "CS 424 | Project 3"),
  dashboardSidebar(
    sidebarMenu(

      menuItem("Map", icon = icon("map", lib = "font-awesome"), tabName = "bart"),
      
      menuItem("Illinois Analysis", icon = icon("search", lib = "font-awesome"), tabName = "vijay"),
      
      menuItem("County Analysis", icon = icon("compass", lib = "font-awesome"), tabName = "countyAnalysis"),
      
      menuItem("Multi-State Analysis", icon = icon("table", lib = "font-awesome"), tabName = "stateAnalysis"),
      
      
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
    tags$style(type = "text/css", "#map {/* make map taller */  height: calc(100vh - 240px) !important;position:sticky !important;}
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
      }       
      .slider-animate-button {
          font-size: 5.2em;
          color: green;
          opacity: 1;
      }
      .leaflet-popup  leaflet-zoom-animated
       {
          left: -271px !important;
       }
      .leaflet-popup-content
       {
          width: 500px !important;
          font-size: 24px;
       }
      .leaflet-container a.leaflet-popup-close-button 
       {
          font: 46px/44px Tahoma, Verdana, sans-serif !important;
          width: 48px;
       }

      .leaflet-touch .leaflet-bar a 
      {
        width: 100px;
        height: 100px;
        line-height: 100px;
        font-size: 100px;
      }

      "),
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
      tabItem(tabName = "bart",
              fluidRow(
                        column(12, box(title = "Map", solidHeader = FALSE, status = "primary", width = 24,
                            leafletOutput("map"))), style = "font-size: 300%"),
               fixedPanel(bottom = 80, left = 420, draggable = TRUE,
                            box(status = "info",solidHeader = TRUE, width = 14,
                                h1(textOutput("yearSelected"),style = "font-size: 500%"))
                            ),
              fixedPanel(top = 150, right = 50,draggable = TRUE, box(title = "Tornado tracks across Illinois", solidHeader = TRUE, status = "primary",width = 12,
                           actionBttn("resetMap","----Reset all values----",icon = NULL, color="danger", style = "material-flat",size = "lg"),
                            checkboxGroupInput("magnitudes", h3("Magnitudes to show:"),
                                              choices=c(0,1,2,3,4,5,"unknown" = -9), inline = TRUE,selected = c(0,1,2,3,4,5)),
                           radioButtons("mapColor",h3("Tornado tracks with color based on :"),
                                        choices=c("magnitude" = "mag", "length" = "len", "width" = "wid",
                                                  "loss" = "loss", "injuries" = "inj", "fatalities" = "fat"),
                                        inline = TRUE,selected = "len"),
                           radioButtons("mapWidth",h3("Tornado tracks with width based on :"),
                                        choices=c("magnitude" = "mag", "length" = "len", "width" = "wid", 
                                                  "loss" = "loss", "injuries" = "inj", "fatalities" = "fat"), 
                                        inline = TRUE,selected = "loss"),
                           # Sliders for all the different values
                           #val = Length 
                           sliderInput("mapLenSlider", label = h3("Length Range"), min = 0, 
                                       max = 240, value = c(0, 240)),
                           #val = Width 
                           sliderInput("mapWidthSlider", label = h3("Width Range"), min = 0, 
                                       max = 4600, value = c(0, 4600)),
                           #val = Loss
                           sliderInput("mapLossSlider", label = h3("Loss($ in million/s) Range"), min = 0, 
                                       max = 1000, value = c(0, 1000),step = 1),
                           #val = injury
                           sliderInput("mapInjurySlider", label = h3("Injury Range"), min = 0, 
                                       max = 1750, value = c(0, 1750)),
                           #val = fatalities 
                           sliderInput("mapFatSlider", label = h3("Fatality Range"), min = 0, 
                                       max = 160, value = c(0, 160)),
                           #val = year 
                           sliderInput("mapYearSlider", label = h3("Year Select"), min = 1950, 
                                       max = 2016, value = 1950,sep="",step = 1,animate = TRUE),
                           #below here is formatting for the map / colors 
                           #checkboxInput("legend", "Show legend", TRUE),
                           #val = theme
                           colourInput("colorPathStart", label = h3("Tornado's tracks color (lowest value)"), "steelblue1",returnName = TRUE,palette = "limited"),
                           colourInput("colorPathEnd", label = h3("Tornado's tracks color (highest value)"), "royalblue4",returnName = TRUE,palette = "limited"),
                           
                           colourInput("colorStart", label = h3("Tornado's starting position color"), "green",returnName = TRUE,palette = "limited",
                                       allowedCols = c(
                                         "white", "black", "red","green","blue","orange","purple","brown","steelblue1")),
                           colourInput("colorEnd", label = h3("Tornado's ending position color"), "red",returnName = TRUE,palette = "limited"),
                           selectInput("topTornado", label = h3("Choose a tornado.."), 
                                       choices = list("None/reset" = -1,"Choice 1" = 367, "Choice 2" = 355, "Choice 3" = 1037,"Choice 4" = 116, "Choice 5" = 834, "Choice 6" = 2265,"Choice 7" = 233, "Choice 8" = 2206, "Choice 9" = 362, "Choice 10" = 325)),
                           selectInput("mapChoice", label = h3("Choose a base map.."), 
                                       choices = list("Base" = 1, "Dark" = 2, "Terrain" = 3, "Satellite" = 4, "Light Pollution" = 5, "Something" = 6))
                           
              ))
      ),
      tabItem(tabName = "vijay",
              tabsetPanel(
              tabPanel("Monthly",
                    fluidRow(
                      box(title = "MONTHLY Total Tornadoes by Magnitude - IL - 1950 to 2016", solidHeader = TRUE, status = "primary", width = 12,
                            tabBox(
                              id = "tab_monthyTotalsInILGraphs", height = "1000px",
                              tabPanel("Total Numbers Monthly", plotlyOutput("magTotalMonthChart", height = 950)),
                              tabPanel("Total Percent Monthly", plotlyOutput("magTotalMonthChartPercent", height = 950)),
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
                        id = "tab_monthyTotalsInILGraphs", height = "1000px",
                        tabPanel("Total Numbers Hourly", plotlyOutput("magTotalHourChart", height = 950)),
                        tabPanel("Total Percent Hourly", plotlyOutput("magTotalHourChartPercent", height = 950)),
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
              tabPanel("Yearly",
                       fluidRow(
                         box(title = "YEARLY Total Tornadoes by Magnitude - IL - 1950 to 2016", solidHeader = TRUE, status = "primary", width = 12,
                             tabBox(
                               id = "tab_yearlyTotalsInILGraphs", height = "1000px",
                               tabPanel("Total Numbers Hourly", plotlyOutput("yearlyGraph", height = 950)),
                               tabPanel("Total Percent Hourly", plotlyOutput("yearlyGraphPer", height = 950)),
                               width = 12
                             )
                         ),
                         box(title = "YEARLY Total Tornadoes by Magnitude - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                             div(DT::dataTableOutput("yearlyTornadoTable", height = 800), style = "font-size: 200%")
                         )
                       )
              ),
                tabPanel("Damages Monthly",
                  fluidRow(
                  box(title = "MONTHLY Deaths, Injuries, and Losses - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                      tabBox(
                        id = "damagesByMonth", height = "1000px",
                        tabPanel("Injuries", plotlyOutput("injuriesChartByMonth", height = 950)),
                        tabPanel("Deaths", plotlyOutput("deathsChartByMonth", height = 950)),
                        tabPanel("Property Loss", plotlyOutput("lossChartByMonth", height = 950)),
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
                        id = "damagesByHour", height = "1000px",
                        tabPanel("Injuries", plotlyOutput("injuriesChartByHour", height = 950)),
                        tabPanel("Deaths", plotlyOutput("deathsChartByHour", height = 950)),
                        tabPanel("Property Loss", plotlyOutput("lossChartByHour", height = 950)),
                        width = 12
                      ),
                      box(status = "primary", solidHeader = TRUE, width = 12,
                          div(DT::dataTableOutput("totalDamagesByHourTable", height = 800), style = "font-size: 200%")
                      )   
                  ))
                ),
              
              tabPanel("Damages Yearly",
                       fluidRow(
                         box(title = "Yearly Deaths, Injuries, and Losses - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                             tabBox(
                               id = "damagesByYear", height = "1000px",
                               tabPanel("Injuries", plotlyOutput("injuriesChart", height = 950)),
                               tabPanel("Deaths", plotlyOutput("deathsChart", height = 950)),
                               tabPanel("Property Loss", plotlyOutput("lossChart", height = 950)),
                               width = 12
                             ),
                             box(status = "primary", solidHeader = TRUE, width = 12,
                                 div(DT::dataTableOutput("totalDamagesTable", height = 800), style = "font-size: 200%")
                             )   
                         ))
              ),
              
              
              tabPanel("Distance",
                       fluidRow(
                         box(title = "Total tornadoes by distance - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                             div(plotlyOutput("distanceCountGraph", height = 1200), style = "font-size: 200%")
                             ),
                         box(title = "Total tornadoes by distance - IL - 1950 to 2016", status = "primary", solidHeader = TRUE, width = 12,
                             div(DT::dataTableOutput("eDistanceTable", height = 600), style = "font-size: 200%")
                         ) 
                         )
              )
              
              )
        ),
    tabItem(tabName = "countyAnalysis",
            fluidRow(
              box(title = textOutput('countyStateTypeText', inline = TRUE), status = "primary", solidHeader = TRUE, width = 12,
                  selectInput("cState", label = "Select a State:", state.abb, selected = "IL")
              ),
              box(title = textOutput('magTypeText', inline = TRUE), status = "primary", solidHeader = TRUE, width = 3, height = "1080px",
                  leafletOutput("mapTotalTornadoes", height = "928px") %>% withSpinner(color="#0dc5c1"),
                  radioButtons("mapChosenMag", "Tornado Magnitude: ", choices=c("all" = 6, 0, 1, 2, 3, 4, 5, "unknown" = 7), inline = TRUE, selected = 6)
              ),
              box(title = "Deaths", status = "primary", solidHeader = TRUE, width = 3, height = "1080px",
                  leafletOutput("mapDeathsByTornadoes", height = "928px") %>% withSpinner(color="#0dc5c1")
              ),
              box(title = "Injuries", status = "primary", solidHeader = TRUE, width = 3, height = "1080px",
                  leafletOutput("mapInjuriesByTornadoes", height = "928px") %>% withSpinner(color="#0dc5c1")
              ),
              box(title = "Losses", status = "primary", solidHeader = TRUE, width = 3, height = "1080px",
                  leafletOutput("mapLossByTornadoes", height = "928px") %>% withSpinner(color="#0dc5c1")
              ),
              box(title = "Data Table", status = "primary", solidHeader = TRUE, width = 6,
                  div(DT::dataTableOutput("countyDataTable", height = 600) %>% withSpinner(color="#0dc5c1"))
              ),
              tabBox(
                #title = "First tabBox",
                # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabset9", height = "880px",
                tabPanel("Without", plotlyOutput("countyGraphWithout", height = 800) %>% withSpinner(color="#0dc5c1")),
                tabPanel("With", plotlyOutput("countyGraph", height = 800) %>% withSpinner(color="#0dc5c1")), 
                width = 6
              )
              
            )
    ),
    tabItem(tabName = "stateAnalysis",
            fluidRow(
              box(status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("aState1", label = "Select a State:", state.abb, selected = "IL"),
                  radioButtons("tabDataFormat1", label = "Select a Format:", choices = c("Yearly", "Monthly", "Hourly"), selected = "Yearly", inline = TRUE),
                  radioButtons("tabDataType1", label = "Select a Type:", choices = c("Damages", "Magnitude"), selected = "Damages", inline = TRUE)
              ),
              box(status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("aState2", label = "Select a State:", state.abb, selected = "NY"),
                  radioButtons("tabDataFormat2", label = "Select a Format:", choices = c("Yearly", "Monthly", "Hourly"), selected = "Yearly", inline = TRUE),
                  radioButtons("tabDataType2", label = "Select a Type:", choices = c("Damages", "Magnitude"), selected = "Damages", inline = TRUE)
              )),
            fluidRow(
              box(title= textOutput("mState1Text", inline = TRUE), status = "primary", solidHeader = TRUE, width = 6, height = "1850px",
                  div(DT::dataTableOutput("state1DataTable", height = 1800) %>% withSpinner(color="#0dc5c1"))
              ),
              box(title= textOutput("mState2Text", inline = TRUE), status = "primary", solidHeader = TRUE, width = 6, height = "1850px",
                  div(DT::dataTableOutput("state2DataTable", height = 1800) %>% withSpinner(color="#0dc5c1"))
              ))
    )
    
    
    
    )
  )
)
