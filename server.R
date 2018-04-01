server <- function(input, output) {
  #load any data here
  #format:  load("rdata/datatoload.RData") for RData,
  tornadoes <- read.csv(file="data/tornadoes.csv", header=TRUE, sep=",")
  fipsCodes <- read.csv("data/US_FIPS_Codes.csv",header = TRUE, sep =  ",")
#--------REACTIVE-----------------------------------------------------------------------
totalTornadoes <- ({
  data <- tornadoes %>% filter(st == "IL")
  data
})



#--------TABLES-----------------------------------------------------------------------
output$totalTornadoes <- renderDataTable(totalTornadoes, extensions = 'Scroller',
  rownames = FALSE, options = list(
  deferRender = TRUE,
  scrollY = 500,
  scroller = TRUE,
  bFilter=0
  )
)


#--------CHARTS/GRAPHS-----------------------------------------------------------------------

output$hourlyGraph <- renderPlotly({

   plot_ly(totalTornadoes, x = ~timeFrame$time, y = ~selectedData$"Departures ORD", type = 'scatter', mode = 'lines+markers', name = 'ORD Departures',
           hoverinfo = 'text', text = ~paste('</br>', selectedData$"Departures ORD", ' ORD Departures </br>'), line = list(color = 'rgb(31,120,180)')) %>%


     layout(font = list(size=30), title="Total number of tornadoes (IL)",
            yaxis = list(title = "# of Flights", titlefont=list(size=30), tickfont=list(size=20)),
            margin = list(l = 100, t = 100, b = 100),
            barmode = 'group')
 })


#--------MAP-----------------------------------------------------------------------
  
  # add a leaflet map and put markers where the deaths occured
  
  output$map <- renderLeaflet({
    #red is for the deaths, while blue is for the wells 
    pal <- colorFactor(c("red","blue"), domain = c("well","death"))
    
    #rename for testing
    colnames(tornadoes)[16] <- "latitude"
    colnames(tornadoes)[17] <- "longitude"
    
    m <-leaflet(tornadoes) %>% addTiles() %>% addCircleMarkers(
      radius = 5,
      color = "blue",
      stroke = FALSE,
      fillOpacity = 1
      #label = ~ifelse(type == "death",paste("Deaths:", joined$deaths) , "Well")
    ) 
    # use the black/white map so it doesn't colide with the data we are displaying 
    m = addProviderTiles(map = m, provider = "CartoDB.Positron")
    #set starting position to one of the locations from the data file 
    #m <- setView(m, lng = -0.136668,lat = 51.513341 , zoom = 16)
    m
  })
  
  

}
