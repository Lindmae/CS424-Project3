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
  
  #2
  monthsMag <- totalTornadoes %>% group_by(mo, mag) %>% summarise(n())
  names(monthsMag) <- c("mo", "mag", "total")
  
  magTotals <- data.frame(
    aggregate(total ~ mag, monthsMag, FUN = sum),
    jan = 0, feb = 0, mar = 0, apr = 0, may = 0, jun = 0, jul = 0, aug = 0, sep = 0, oct = 0, nov = 0, dec = 0,
    janPercent = 0, febPercent = 0, marPercent = 0, aprPercent = 0, mayPercent = 0, junPercent = 0, julPercent = 0, augPercent = 0,
    sepPercent = 0, octPercent = 0, novPercent = 0, decPercent = 0
  )
  test <- aggregate(mag ~ mo, monthsMag, FUN = sum)
  
  for (i in 1:length(test$mo)){
    for (j in 1:length(magTotals$mag)){
      # assumption here that magTotals$mag contains all magnitude ranges (this way we account for cases were a particular
      # month might not have ANY of a particular magnitude tornado)
      temp <- monthsMag %>% filter(mo == i) %>% filter(mag == magTotals$mag[j])
      if (length(temp$mag) == 0){
        magTotals[[i+2]][j] <- 0
        magTotals[[i+14]][j] <- 0
        tGraph <- data.frame(
          mo = i,
          mag = magTotals$mag[j],
          total = 0,
          magPercent = 0
        )
        if (i*j == 1){
          graphFriendlyMagTotals <- tGraph
        } else {
          graphFriendlyMagTotals <- rbind(graphFriendlyMagTotals, tGraph)
        }
      } else {
        magTotals[[i+2]][j] <- temp$total
        magTotals[[i+14]][j] <- round(temp$total / magTotals[[2]][j], 3)
        tGraph <- data.frame(
          mo = i,
          mag = magTotals$mag[j],
          total = temp$total,
          magPercent = magTotals[[i+14]][j]
        )
        if (i*j == 1){
          graphFriendlyMagTotals <- tGraph
        } else {
          graphFriendlyMagTotals <- rbind(graphFriendlyMagTotals, tGraph)
        }
      }
    }
  }
  
  #3
  # similar approach to breaking up hours as here: https://stats.stackexchange.com/questions/147063/r-how-to-separate-date-time-data-types
  hr <- format(as.POSIXct(strptime(totalTornadoes$time,"%H:%M:%S",tz="")) ,format = "%H")
  min <- format(as.POSIXct(strptime(totalTornadoes$time,"%H:%M:%S",tz="")) ,format = "%M")
  expandedTotalTornadoes <- totalTornadoes
  expandedTotalTornadoes$hr <- hr
  expandedTotalTornadoes$min <- min
  
  hoursMag <- expandedTotalTornadoes %>% group_by(hr, mag) %>% summarise(n())
  names(hoursMag) <- c("hr", "mag", "total")
  
  magTotalsByHour <- data.frame(
    aggregate(total ~ mag, hoursMag, FUN = sum),
    hour0 = 0, hour1 = 0, hour2 = 0, hour3 = 0, hour4 = 0, hour5 = 0, hour6 = 0, hour7 = 0, hour8 = 0,
    hour9 = 0, hour10 = 0, hour11 = 0, hour12 = 0, hour13 = 0, hour14 = 0, hour15 = 0, hour16 = 0, hour17 = 0,
    hour18 = 0, hour19 = 0, hour20 = 0, hour21 = 0, hour22 = 0, hour23 = 0,
    prcntHr0 = 0, prcntHr1 = 0, prcntHr2 = 0, prcntHr3 = 0, prcntHr4 = 0, prcntHr5 = 0, prcntHr6 = 0, prcntHr7 = 0, prcntHr8 = 0,
    prcntHr9 = 0, prcntHr10 = 0, prcntHr11 = 0, prcntHr12 = 0, prcntHr13 = 0, prcntHr14 = 0, prcntHr15 = 0, prcntHr16 = 0, prcntHr17 = 0,
    prcntHr18 = 0, prcntHr19 = 0, prcntHr20 = 0, prcntHr21 = 0, prcntHr22 = 0, prcntHr23 = 0
  )
  testHour <- aggregate(mag ~ hr, hoursMag, FUN = sum)
  
  for (i in 1:length(testHour$hr)){
    for (j in 1:length(magTotalsByHour$mag)){
      # assumption here that magTotalsByHour$mag contains all magnitude ranges (this way we account for cases were a particular
      # hour might not have ANY of a particular magnitude tornado)
      temp <- hoursMag %>% filter(hr == testHour$hr[i]) %>% filter(mag == magTotalsByHour$mag[j])
      if (length(temp$mag) == 0){
        magTotalsByHour[[i+2]][j] <- 0
        magTotalsByHour[[i+26]][j] <- 0
        tGraph <- data.frame(
          hr = i-1,
          mag = magTotalsByHour$mag[j],
          total = 0,
          magPercent = 0
        )
        if (i*j == 1){
          graphFriendlymagTotalsByHour <- tGraph
        } else {
          graphFriendlymagTotalsByHour <- rbind(graphFriendlymagTotalsByHour, tGraph)
        }
      } else {
        magTotalsByHour[[i+2]][j] <- temp$total
        magTotalsByHour[[i+26]][j] <- round(temp$total / magTotalsByHour[[2]][j], 3)
        tGraph <- data.frame(
          hr = i-1,
          mag = magTotalsByHour$mag[j],
          total = temp$total,
          magPercent = magTotalsByHour[[i+26]][j]
        )
        if (i*j == 1){
          graphFriendlymagTotalsByHour <- tGraph
        } else {
          graphFriendlymagTotalsByHour <- rbind(graphFriendlymagTotalsByHour, tGraph)
        }
      }
    }
  }
  
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
  
#--------REACTIVE-----------------------------------------------------------------------
  getTimeFrame <- reactive({
    totalALL <- NA
    totalALL$hr <- graphFriendlymagTotalsByHour$hr
    
    if(!input$time){
      totalALL$hr <- format(strptime(totalALL$hr, format='%H'), '%r')
    }
    
    chosen <- NA
    chosen$hr <- unique(totalALL$hr)
    
    chosen
  })
  
  getMagTotalsByHourAMPM <- reactive({
    chosen <- magTotalsByHour
    if(!input$time){
      chosen <- rename(chosen,
        hour12am = hour0, hour1am = hour1, hour2am = hour2, hour3am = hour3, hour4am = hour4, hour5am = hour5, hour6am = hour6, hour7am = hour7, hour8am = hour8,
        hour9am = hour9, hour10am = hour10, hour11am = hour11, hour12pm = hour12, hour1pm = hour13, hour2pm = hour14, hour3pm = hour15, hour4pm = hour16, hour5pm = hour17,
        hour6pm = hour18, hour7pm = hour19, hour8pm = hour20, hour9pm = hour21, hour10pm = hour22, hour11pm = hour23,
        prcHr12am = prcntHr0, prcHr1am = prcntHr1, prcHr2am = prcntHr2, prcHr3am = prcntHr3, prcHr4am = prcntHr4, prcHr5am = prcntHr5, prcHr6am = prcntHr6, prcHr7am = prcntHr7, prcHr8am = prcntHr8,
        prcHr9am = prcntHr9, prcHr10am = prcntHr10, prcHr11am = prcntHr11, prcHr12pm = prcntHr12, prcHr1pm = prcntHr13, prcHr2pm = prcntHr14, prcHr3pm = prcntHr15, prcHr4pm = prcntHr16, prcHr5pm = prcntHr17,
        prcHr6pm = prcntHr18, prcHr7pm = prcntHr19, prcHr8pm = prcntHr20, prcHr9pm = prcntHr21, prcHr10pm = prcntHr22, prcHr11pm = prcntHr23
      )
    }
    chosen
  })



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
  
  output$magTotalMonthTable <- renderDataTable(magTotals[, 1:14], extensions = 'Scroller', 
                                               rownames = FALSE, options = list(
                                                 deferRender = TRUE,
                                                 scrollY = 800,
                                                 scroller = TRUE,
                                                 bFilter=0
                                                 )
  )
  
  output$magTotalMonthTablePercent <- renderDataTable(magTotals[, c(1, 15:26)], extensions = 'Scroller', 
                                               rownames = FALSE, options = list(
                                                 deferRender = TRUE,
                                                 scrollY = 800,
                                                 scroller = TRUE,
                                                 bFilter=0
                                               )
  )
  
  output$magTotalHourTableI <- renderDataTable(getMagTotalsByHourAMPM()[, 1:14], extensions = 'Scroller', 
                                                      rownames = FALSE, options = list(
                                                        deferRender = TRUE,
                                                        scrollY = 800,
                                                        scroller = TRUE,
                                                        bFilter=0
                                                      )
  )
  output$magTotalHourTableII <- renderDataTable(getMagTotalsByHourAMPM()[, c(1, 2, 15:26)], extensions = 'Scroller', 
                                               rownames = FALSE, options = list(
                                                 deferRender = TRUE,
                                                 scrollY = 800,
                                                 scroller = TRUE,
                                                 bFilter=0
                                               )
  )
  output$magTotalHourTablePercentI <- renderDataTable(getMagTotalsByHourAMPM()[, c(1, 27:38)], extensions = 'Scroller', 
                                               rownames = FALSE, options = list(
                                                 deferRender = TRUE,
                                                 scrollY = 800,
                                                 scroller = TRUE,
                                                 bFilter=0
                                               )
  )
  output$magTotalHourTablePercentII <- renderDataTable(getMagTotalsByHourAMPM()[, c(1, 39:50)], extensions = 'Scroller', 
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
  
  # get TOTAL TORNADOES (by MAG) per MONTH summed over 1950 - 2009
  output$magTotalMonthChart <- renderPlotly({
    data <- graphFriendlyMagTotals
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    # takes mag totals across 12 months (each has a length of 12)
    mag0 <- data %>% filter(mag == 0)
    mag1 <- data %>% filter(mag == 1)
    mag2 <- data %>% filter(mag == 2)
    mag3 <- data %>% filter(mag == 3)
    mag4 <- data %>% filter(mag == 4)
    mag5 <- data %>% filter(mag == 5)
    
    plot_ly(test, x = ~textOfMonth, y = ~mag0$total, type = 'bar', name = 'Mag 0', hoverinfo = 'text', 
            text = ~paste('</br>Mag:', mag0$mag, '</br>Tornadoes:', mag0$total, '<br>Month:', mag0$mo, '</br>')) %>%
      add_trace(y = ~mag1$total, name = 'Mag 1', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag1$mag, '</br>Tornadoes:', mag1$total, '<br> Month:', mag1$mo, '</br>')) %>% 
      add_trace(y = ~mag2$total, name = 'Mag 2', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag2$mag, '</br>Tornadoes:', mag2$total, '<br> Month:', mag2$mo, '</br>')) %>% 
      add_trace(y = ~mag3$total, name = 'Mag 3', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag3$mag, '</br>Tornadoes:', mag3$total, '<br> Month:', mag3$mo, '</br>')) %>%
      add_trace(y = ~mag4$total, name = 'Mag 4', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag4$mag, '</br>Tornadoes:', mag4$total, '<br> Month:', mag4$mo, '</br>')) %>%
      add_trace(y = ~mag5$total, name = 'Mag 5', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag5$mag, '</br>Tornadoes:', mag5$total, '<br> Month:', mag5$mo, '</br>')) %>% 

      layout(title = "Total Tornadoes by Magnitude in Illinois 1950 - 2009", xaxis = list(title = "Month", autotick = F, dtick = 1, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
  # get PERCENT TORNADOES (by MAG) per MONTH summed over 1950 - 2009
  output$magTotalMonthChartPercent <- renderPlotly({
    data <- graphFriendlyMagTotals
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    # takes mag totals across 12 months (each has a length of 12)
    mag0 <- data %>% filter(mag == 0)
    mag1 <- data %>% filter(mag == 1)
    mag2 <- data %>% filter(mag == 2)
    mag3 <- data %>% filter(mag == 3)
    mag4 <- data %>% filter(mag == 4)
    mag5 <- data %>% filter(mag == 5)
    
    plot_ly(mag0, x = ~textOfMonth, y = ~mag0$magPercent, type = 'bar', name = 'Mag 0', hoverinfo = 'text', 
            text = ~paste('</br>Mag:', mag0$mag, '</br>% Tornadoes:', mag0$magPercent, '<br>Month:', mag0$mo, '</br>')) %>%
      add_trace(y = ~mag1$magPercent, name = 'Mag 1', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag1$mag, '</br>% Tornadoes:', mag1$magPercent, '<br> Month:', mag1$mo, '</br>')) %>% 
      add_trace(y = ~mag2$magPercent, name = 'Mag 2', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag2$mag, '</br>% Tornadoes:', mag2$magPercent, '<br> Month:', mag2$mo, '</br>')) %>% 
      add_trace(y = ~mag3$magPercent, name = 'Mag 3', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag3$mag, '</br>% Tornadoes:', mag3$magPercent, '<br> Month:', mag3$mo, '</br>')) %>%
      add_trace(y = ~mag4$magPercent, name = 'Mag 4', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag4$mag, '</br>% Tornadoes:', mag4$magPercent, '<br> Month:', mag4$mo, '</br>')) %>%
      add_trace(y = ~mag5$magPercent, name = 'Mag 5', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag5$mag, '</br>% Tornadoes:', mag5$magPercent, '<br> Month:', mag5$mo, '</br>')) %>% 
      
      layout(title = "Percent of Tornadoes by Magnitude in Illinois 1950 - 2009", xaxis = list(title = "Month", autotick = F, dtick = 1, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
  # get TOTAL TORNADOES (by MAG) per HOUR summed over 1950 - 2009
  output$magTotalHourChart <- renderPlotly({
    data <- graphFriendlymagTotalsByHour
    timeFrame <- getTimeFrame()

    # takes mag totals across 24 hours (each has a length of 24)
    mag0 <- data %>% filter(mag == 0)
    mag1 <- data %>% filter(mag == 1)
    mag2 <- data %>% filter(mag == 2)
    mag3 <- data %>% filter(mag == 3)
    mag4 <- data %>% filter(mag == 4)
    mag5 <- data %>% filter(mag == 5)
    
    plot_ly(data, x = ~timeFrame$hr, y = ~mag0$total, type = 'bar', name = 'Mag 0', hoverinfo = 'text', 
            text = ~paste('</br>Mag:', mag0$mag, '</br>Tornadoes:', mag0$total, '<br>Hour:', timeFrame$hr, '</br>')) %>%
      add_trace(y = ~mag1$total, name = 'Mag 1', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag1$mag, '</br>Tornadoes:', mag1$total, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      add_trace(y = ~mag2$total, name = 'Mag 2', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag2$mag, '</br>Tornadoes:', mag2$total, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      add_trace(y = ~mag3$total, name = 'Mag 3', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag3$mag, '</br>Tornadoes:', mag3$total, '<br> Hour:', timeFrame$hr, '</br>')) %>%
      add_trace(y = ~mag4$total, name = 'Mag 4', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag4$mag, '</br>Tornadoes:', mag4$total, '<br> Hour:', timeFrame$hr, '</br>')) %>%
      add_trace(y = ~mag5$total, name = 'Mag 5', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag5$mag, '</br>Tornadoes:', mag5$total, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      
      layout(title = "Total Tornadoes by Magnitude in Illinois 1950 - 2009", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })

  # get PERCENT TORNADOES (by MAG) per HOUR summed over 1950 - 2009
  output$magTotalHourChartPercent <- renderPlotly({
    data <- graphFriendlymagTotalsByHour
    timeFrame <- getTimeFrame()
    
    # takes mag totals across 24 hours (each has a length of 24)
    mag0 <- data %>% filter(mag == 0)
    mag1 <- data %>% filter(mag == 1)
    mag2 <- data %>% filter(mag == 2)
    mag3 <- data %>% filter(mag == 3)
    mag4 <- data %>% filter(mag == 4)
    mag5 <- data %>% filter(mag == 5)
    
    plot_ly(data, x = ~timeFrame$hr, y = ~mag0$magPercent, type = 'bar', name = 'Mag 0', hoverinfo = 'text', 
            text = ~paste('</br>Mag:', mag0$mag, '</br>% Tornadoes:', mag0$magPercent, '<br>Hour:', mag0$mo, '</br>')) %>%
      add_trace(y = ~mag1$magPercent, name = 'Mag 1', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag1$mag, '</br>% Tornadoes:', mag1$magPercent, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      add_trace(y = ~mag2$magPercent, name = 'Mag 2', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag2$mag, '</br>% Tornadoes:', mag2$magPercent, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      add_trace(y = ~mag3$magPercent, name = 'Mag 3', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag3$mag, '</br>% Tornadoes:', mag3$magPercent, '<br> Hour:', timeFrame$hr, '</br>')) %>%
      add_trace(y = ~mag4$magPercent, name = 'Mag 4', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag4$mag, '</br>% Tornadoes:', mag4$magPercent, '<br> Hour:', timeFrame$hr, '</br>')) %>%
      add_trace(y = ~mag5$magPercent, name = 'Mag 5', hoverinfo = 'text', 
                text = ~paste('</br>Mag:', mag5$mag, '</br>% Tornadoes:', mag5$magPercent, '<br> Hour:', timeFrame$hr, '</br>')) %>% 
      
      layout(title = "Percent of Tornadoes by Magnitude in Illinois 1950 - 2009", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr,titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
#--------MAP-----------------------------------------------------------------------
  
  # add a leaflet map and put markers where the deaths occured
  
  output$map <- renderLeaflet({
    #red is for the deaths, while blue is for the wells 
    pal <- colorFactor(c("red","blue"), domain = c("well","death"))
    
    #rename for testing
    tornadoesMap <- tornadoes %>% filter(st == "IL")
    colnames(tornadoesMap)[16] <- "latitude"
    colnames(tornadoesMap)[17] <- "longitude"
 

    m <-leaflet(tornadoesMap) %>% 
    # enable to set overview of US #setView(-96, 37.8, 4) %>%
      setView(-87, 41, 8) %>% #set view to chicago 
      #addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,position = "bottomright") %>%
      addTiles() %>% 
      addPolylines(data = tornadoesMap, lng = ~longitude, lat = ~latitude)
      #addCircleMarkers(radius = 1,color = "blue", stroke = FALSE,fillOpacity = 1)
      #label = ~ifelse(type == "death",paste("Deaths:", joined$deaths) , "Well")
     
    # use the black/white map so it doesn't colide with the data we are displaying 
    m = addProviderTiles(map = m, provider = "CartoDB.Positron")
    #set starting position to one of the locations from the data file 
    #m <- setView(m, lng = -0.136668,lat = 51.513341 , zoom = 16)
    m
  })
  
  

}
