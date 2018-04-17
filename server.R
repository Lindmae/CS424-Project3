server <- function(input, output) {
  
#--------DATA---------------------------------------------------------------------------
  #load any data here
  #format:  load("rdata/datatoload.RData") for RData,
  tornadoes <- read.csv(file="data/tornadoes.csv", header=TRUE, sep=",")
  fipsCodes <- read.csv("data/US_FIPS_Codes.csv",header = TRUE, sep =  ",")
  
  # filtered to IL data
  totalTornadoes <- tornadoes %>% filter(st == "IL")
  
  #1
  yearlyTornadoes <- totalTornadoes %>% group_by(Year = totalTornadoes$yr, Magnitude = totalTornadoes$mag) %>% summarise(Count = n())
  #names(yearlyTornadoes) <- c("Year", "Magnitude", "Count")
  yearlyTornadoes$Magnitude <- factor(yearlyTornadoes$Magnitude)
  
  #2
  load("rdata/magTotals.RData")
  load("rdata/graphFriendlyMagTotals.RData")
  
  #3
  load("rdata/expandedTotalTornadoes.RData") # this object includes hr and min as seperate columns
  load("rdata/magTotalsByHour.RData")
  load("rdata/graphFriendlymagTotalsByHour.RData")
  
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
  
  #6 =================================================================================================
  # NEED TO ACCOUNT FOR CHANGES IN HOW WE LOOK AT LOSSES. SEE TASKS AND DEADLINES DOC...
  # HERE I AM TRYING TO PUT ACTUAL DOLLAR AMOUNT OF LOSSES IN THE COLUMN
  totalTornadoesPre1996 <- totalTornadoes %>% filter(yr <= 1995)
  valueOfLoss <- list(50, 500, 5000, 50000, 500000, 5000000, 50000000, 500000000, 5000000000)
  for(i in 1:length(totalTornadoesPre1996$loss)){
    if(totalTornadoesPre1996$loss[i] != 0){
      totalTornadoesPre1996$loss[i] <- valueOfLoss[[totalTornadoesPre1996$loss[i]]]
    }
  }
  
  totalTornadoesPre2015 <- totalTornadoes %>% filter(yr >= 1996 & yr <= 2015)
  for(i in 1:length(totalTornadoesPre2015$loss)){
    if(totalTornadoesPre2015$loss[i] != 0){
      totalTornadoesPre2015$loss[i] <- totalTornadoesPre2015$loss[i] * 1000000
    }
  }
  
  totalTornadoesModern <- totalTornadoes %>% filter(yr == 2016)
  
  adjTotalTornadoesIL <- rbind(totalTornadoesPre1996, totalTornadoesPre2015)
  adjTotalTornadoesIL <- rbind(adjTotalTornadoesIL, totalTornadoesModern)
  
  load("rdata/totalDamagesByMonth.RData")
  
  #7
  load("rdata/totalDamagesByHour.RData")
  
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
  # adjust graphical interfaces to am/pm
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
  
  getTimeFrameDamages <- reactive({
    totalALL <- NA
    totalALL$hr <- totalDamagesByHour$hr
    
    if(!input$time){
      totalALL$hr <- format(strptime(totalALL$hr, format='%H'), '%r')
    }
    
    chosen <- NA
    chosen$hr <- unique(totalALL$hr)
    
    chosen
  })
  
  getTotalDamagesByHourAMPM<- reactive({
    totalALL <- NA
    totalALL$hr <- totalDamagesByHour$hr
    
    if(!input$time){
      totalALL$hr <- format(strptime(totalALL$hr, format='%H'), '%r')
    }
    
    chosen <- totalDamagesByHour
    chosen$hr <- unique(totalALL$hr)
    
    chosen
  })
  
  # adjust table interfaces to am/pm
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


  reactiveMap <- reactive({
    
    #filter to be only illinois, and make sure they are valid points 
    tornadoesMap <- tornadoes %>% filter(st == input$selectState & slon != 0.00)
    #for any position that ends at 0.00 lat/lon, we will set it to be the same as the start 
    tornadoesMap$elat[tornadoesMap$elat == 0.00] <- tornadoesMap$slat 
    tornadoesMap$elon[tornadoesMap$elon == 0.00] <- tornadoesMap$slon
    #only render magnitudes that have been selected 
    tornadoesMap <- tornadoesMap %>% filter(mag == input$magnitudes)
    #check for all the range sliders 
    tornadoesMap <- tornadoesMap %>% filter(len >= input$mapLenSlider[1] & len <= input$mapLenSlider[2] &
                                            wid >= input$mapWidthSlider[1] & wid <= input$mapWidthSlider[2] &
                                            loss >= input$mapLossSlider[1] & loss <= input$mapLossSlider[2] &
                                            inj >= input$mapInjurySlider[1] & inj <= input$mapInjurySlider[2] &
                                            fat >= input$mapFatSlider[1] & fat <= input$mapFatSlider[2] 
                                            )

    #return the data frame 
    tornadoesMap
    
  })
  
  reactiveMapColor <- reactive({
    selected <- input$mapColor
  })
  
  reactiveMapWidth <- reactive({
    selected <- input$mapWidth
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
  
  output$totalDamagesByMonthTable <- renderDataTable(totalDamagesByMonth, extensions = 'Scroller', 
                                              rownames = FALSE, options = list(
                                                deferRender = TRUE,
                                                scrollY = 800,
                                                scroller = TRUE,
                                                bFilter=0
                                              )
  )
  
  output$totalDamagesByHourTable <- renderDataTable(getTotalDamagesByHourAMPM(), extensions = 'Scroller', 
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
  
  output$yearlyGraph <- renderPlotly({
    
    ggplot(yearlyTornadoes, aes(x = Year, y = Count ,fill = Magnitude)) + 
      ggtitle("Yearly Tornado Count by Magnitude") + geom_bar(stat = "identity", position = "stack")
    
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
  
  # vis TOTAL TORNADOES (by MAG) per MONTH summed over 1950 - 2016
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
    
    plot_ly(mag0, x = ~textOfMonth, y = ~mag0$total, type = 'bar', name = 'Mag 0', hoverinfo = 'text', 
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

      layout(title = "Total Tornadoes by Magnitude in Illinois 1950 - 2016", xaxis = list(title = "Month", autotick = F, dtick = 1, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
  # vis PERCENT TORNADOES (by MAG) per MONTH summed over 1950 - 2016
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
      
      layout(title = "Percent of Tornadoes by Magnitude in Illinois 1950 - 2016", xaxis = list(title = "Month", autotick = F, dtick = 1, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
  # vis TOTAL TORNADOES (by MAG) per HOUR summed over 1950 - 2016
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
      
      layout(title = "Total Tornadoes by Magnitude in Illinois 1950 - 2016", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })

  # vis PERCENT TORNADOES (by MAG) per HOUR summed over 1950 - 2016
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
      
      layout(title = "Percent of Tornadoes by Magnitude in Illinois 1950 - 2016", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr,titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Tornadoes', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
    
  })
  
  # vis (FAT, INJ, and LOSS) per MONTH summed over 1950 - 2016
  output$injuriesChartByMonth <- renderPlotly({
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    
    plot_ly(totalDamagesByMonth, x = ~textOfMonth, y = ~Injuries, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Injuries:', Injuries, '<br>Month:', mo, '</br>')) %>%
      
      
      layout(title = "Injuries by Month in Illinois 1950 - 2016", xaxis = list(title = "Month", autotick = F, dtick = 1, 
            titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Injuries', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  output$deathsChartByMonth <- renderPlotly({
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    
    plot_ly(totalDamagesByMonth, x = ~textOfMonth, y = ~Deaths, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Deaths:', Deaths, '<br>Month:', mo, '</br>')) %>%
      
      
      layout(title = "Deaths by Month in Illinois 1950 - 2016", xaxis = list(title = "Month", autotick = F, dtick = 1, 
                                                                               titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Deaths', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  output$lossChartByMonth <- renderPlotly({
    textOfMonth <- list("January", "February", "March", "April ", "May", "June", "July", "August", "September", "October", "November", "December")
    
    plot_ly(totalDamagesByMonth, x = ~textOfMonth, y = ~Loss, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Loss:', Loss, '<br>Month:', mo, '</br>')) %>%
      
      
      layout(title = "Loss by Month in Illinois 1950 - 2016", xaxis = list(title = "Month", autotick = F, dtick = 1, 
                titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Loss', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  # vis (FAT, INJ, and LOSS) per HOUR summed over 1950 - 2016
  output$injuriesChartByHour <- renderPlotly({
    timeFrame <- getTimeFrameDamages()

    plot_ly(totalDamagesByHour, x = ~timeFrame$hr, y = ~Injuries, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Injuries:', Injuries, '<br>Hour:', timeFrame$hr, '</br>')) %>%
      
      layout(title = "Injuries by Hour in Illinois 1950 - 2016", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Injuries', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  output$deathsChartByHour <- renderPlotly({
    timeFrame <- getTimeFrameDamages()
    
    plot_ly(totalDamagesByHour, x = ~timeFrame$hr, y = ~Deaths, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Deaths:', Deaths, '<br>Hour:', timeFrame$hr, '</br>')) %>%
      
      
      layout(title = "Deaths by Hour in Illinois 1950 - 2016", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
            categoryorder = "array", categoryarray = timeFrame$hr, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Deaths', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  output$lossChartByHour <- renderPlotly({
    timeFrame <- getTimeFrameDamages()
    
    plot_ly(totalDamagesByHour, x = ~timeFrame$hr, y = ~Loss, name = 'trace 0', type = 'scatter', mode = 'lines+markers',
            hoverinfo = 'text', text = ~paste('</br>Loss:', Loss, '<br>Hour:', timeFrame$hr, '</br>')) %>%
      
      
      layout(title = "Loss by Hour in Illinois 1950 - 2016", xaxis = list(title = "Hour", autotick = F, dtick = 1, 
                  categoryorder = "array", categoryarray = timeFrame$hr, titlefont=list(size=30), tickfont=list(size=20))) %>%
      layout(yaxis = list(title = 'Total Loss', titlefont=list(size=30), tickfont=list(size=20)), barmode = 'stack',
             margin=list(l=100, t=100, b=100))
  })
  
  
#--------MAP-----------------------------------------------------------------------
  
  # add a leaflet map and put markers where the deaths occured
  
  output$map <- renderLeaflet({
    
    tornadoesMap <- reactiveMap()
    selectedWidth <- reactiveMapWidth()
    selectedColor <- reactiveMapColor()
    
    m <-leaflet(tornadoesMap) %>%  addTiles() 
    
    for(i in 1:nrow(tornadoesMap)){
    m <-  addPolylines(m,data = tornadoesMap, weight = as.numeric(tornadoesMap[i,selectedWidth]), color = "blue",
         lat = as.numeric(tornadoesMap[i, c('slat','elat' )]), lng = as.numeric(tornadoesMap[i, c('slon', 'elon')])) 
    }
  
  #adding circles to denote start and end of all the tornadoes 
  m <- addCircleMarkers(m, lng = tornadoesMap$slon , lat = tornadoesMap$slat , radius = 2, color = "green", fillColor = "red")
  m <- addCircleMarkers(m, lng = tornadoesMap$elon , lat = tornadoesMap$elat , radius = 2, color = "red", fillColor = "red")
    
    
    # use the black/white map so it doesn't colide with the data we are displaying 
    m = addProviderTiles(map = m, provider = "CartoDB.Positron")

    m
  })
  
 
  
  
}
