server <- function(input, output) {
  
#--------DATA---------------------------------------------------------------------------
  #load any data here
  #format:  load("rdata/datatoload.RData") for RData,
  tornadoes <- read.csv(file="data/tornadoes.csv", header=TRUE, sep=",")
  fipsCodes <- read.csv("data/US_FIPS_Codes.csv",header = TRUE, sep =  ",")
  
  # filtered to IL data
  totalTornadoes <- tornadoes %>% filter(st == "IL")
  
  #1
  yearlyTornadoes <- totalTornadoes %>% group_by(yr, mag) %>% summarise(n())
  names(yearlyTornadoes) <- c("Year", "Magnitude", "Count")
  
  #4
  #TO DO
  
  #5
  deaths <- totalTornadoes %>% group_by(yr, fat) %>% summarise(n())
  names(deaths) <- c("Year", "X", "Count")
  deathCount <- aggregate(deaths$Count * deaths$X, by=list(Category=deaths$Year), FUN=sum)
  names(deathCount) <- c("Year", "Deaths")
  
  injuries <- totalTornadoes %>% group_by(yr, inj) %>% summarise(n())
  names(injuries) <- c("Year", "X", "Count")
  injuriesCount <- aggregate(injuries$Count * injuries$X, by=list(Category=injuries$Year), FUN=sum)
  names(injuriesCount) <- c("Year", "Injuries")
  
  loss <- totalTornadoes %>% group_by(yr, loss) %>% summarise(n())
  names(loss) <- c("Year", "X", "Count")
  lossCount <- aggregate(loss$Count * loss$X, by=list(Category=loss$Year), FUN=sum)
  names(lossCount) <- c("Year", "Loss")
  
  totalDamages <- merge(deathCount,injuriesCount,by="Year")
  totalDamages <- merge(totalDamages, lossCount, by="Year")
  
  #8
  county1 <- totalTornadoes %>% group_by(f1) %>% summarise(n())
  names(county1) <- c("County", "Count1")
  county2 <- totalTornadoes %>% group_by(f2) %>% summarise(n())
  names(county2) <- c("County", "Count2")
  county3 <- totalTornadoes %>% group_by(f3) %>% summarise(n())
  names(county3) <- c("County", "Count3")
  county4 <- totalTornadoes %>% group_by(f4) %>% summarise(n())
  names(county4) <- c("County", "Count4")
  
  countyCounts <- merge(county1, county2, by="County", all.x = TRUE, all.y = TRUE)
  countyCounts <- merge(countyCounts, county3, by="County", all.x = TRUE, all.y = TRUE)
  countyCounts <- merge(countyCounts, county4, by="County", all.x = TRUE, all.y = TRUE)
  countyCounts[is.na(countyCounts)] <- 0
  countyCounts$Final <- rowSums( countyCounts[,2:5] )
  
  countyData <- subset(countyCounts, select = c(County,Final))
  names(countyData) <- c("County", "Total Tornadoes")
  
  #order by total tornadoes
  countyData <- countyData[order(-countyData$`Total Tornadoes`),] 
  
#--------REACTIVE-----------------------------------------------------------------------
# example reactive element below
# totalTornadoes <- ({
#  data <- tornadoes %>% filter(st == "IL")
#  data
#})



#--------TABLES-----------------------------------------------------------------------
output$totalTornadoes <- renderDataTable(totalTornadoes, #extensions = 'Scroller', rownames = FALSE, 
  options = list(
  deferRender = TRUE,
  scrollY = 500,
  scroller = TRUE,
  bFilter=0
  )
)
  
  output$totalDamagesTable <- renderDataTable(totalDamages, extensions = 'Scroller', 
                                                   rownames = FALSE, options = list(
                                                     deferRender = TRUE,
                                                     scrollY = 800,
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

  
  output$injuriesChart <- renderPlotly({
    
    plot_ly(totalDamages, x = ~Year, y = ~Injuries, name = 'trace 0', type = 'scatter', mode = 'lines+markers') %>%
      
      
      layout(font = list(size=30), title="Injuries per Year",
             yaxis = list(title = "# of Injuries", titlefont=list(size=30), tickfont=list(size=20)),
             margin = list(l = 100, t = 100, b = 100),
             barmode = 'group')
  })
  
  output$deathsChart <- renderPlotly({
    
    plot_ly(totalDamages, x = ~Year, y = ~Deaths, name = 'trace 0', type = 'scatter', mode = 'lines+markers') %>%
      
      
      layout(font = list(size=30), title="Deaths per Year",
             yaxis = list(title = "# of Deaths", titlefont=list(size=30), tickfont=list(size=20)),
             margin = list(l = 100, t = 100, b = 100),
             barmode = 'group')
  })
  
  output$lossChart <- renderPlotly({
    
    plot_ly(totalDamages, x = ~Year, y = ~Loss, name = 'trace 0', type = 'scatter', mode = 'lines+markers') %>%
      
      
      layout(font = list(size=30), title="Property Loss per Year",
             yaxis = list(title = "Property Loss Value", titlefont=list(size=30), tickfont=list(size=20)),
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
