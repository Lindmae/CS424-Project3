server <- function(input, output) {

#--------FUNCTIONS----------------------------------------------------------------------
  # columnType = any column from totalTornadoes; damageType = fat, inj, loss
  # Note: group_by_ has been used, more on it here: https://stackoverflow.com/questions/47721747/error-in-grouped-df-impldata-unnamevars-drop
  # TODO: perhaps make it general enough for any state AND county (part A requirements 1 & 2)
  getDamageData <- function(columnType, damageType, sData){
    damage <- sData %>% group_by_(columnType, damageType) %>% summarise(n())
    names(damage) <- c(columnType, "X", "Count")
    damageCount <- aggregate(damage$Count * damage$X, by=list(Category=damage[[1]]), FUN=sum)
    names(damageCount) <- c(columnType, damageType)
    
    return(damageCount)
  }
  
  getFullDamageData <- function(columnType, sData){
    deathCount <- getDamageData(columnType, "fat", sData)
    injuriesCount <- getDamageData(columnType, "inj", sData)
    lossCount <- getDamageData(columnType, "loss", sData)
    
    names(deathCount) <- c(columnType, "Deaths")
    names(injuriesCount) <- c(columnType, "Injuries")
    names(lossCount) <- c(columnType, "Loss")
    
    totalDamagesByType <- merge(deathCount,injuriesCount,by=columnType)
    totalDamagesByType <- merge(totalDamagesByType, lossCount, by=columnType)
  }
  
  getFullDamageDataByCounty <- function(sData, sOrdCountyData){
    # get injuries, deaths, and losses on a per county basis
    county1 <- getFullDamageData("f1", sData)
    county2 <- getFullDamageData("f2", sData)
    county3 <- getFullDamageData("f3", sData)
    county4 <- getFullDamageData("f4", sData)
    
    names(county1) <- c("County", "Deaths1", "Injuries1", "Loss1")
    names(county2) <- c("County", "Deaths2", "Injuries2", "Loss2")
    names(county3) <- c("County", "Deaths3", "Injuries3", "Loss3")
    names(county4) <- c("County", "Deaths4", "Injuries4", "Loss4")
    
    countyCounts <- merge(county1, county2, by="County", all.x = TRUE, all.y = TRUE)
    countyCounts <- merge(countyCounts, county3, by="County", all.x = TRUE, all.y = TRUE)
    countyCounts <- merge(countyCounts, county4, by="County", all.x = TRUE, all.y = TRUE)
    countyCounts[is.na(countyCounts)] <- 0
    
    countyCounts$TotalDeathsByTornado <- rowSums(countyCounts[, c(2, 5, 8, 11)])
    countyCounts$TotalInjuriesByTornado <- rowSums(countyCounts[, c(3, 6, 9, 12)])
    countyCounts$TotalLossByTornado <- rowSums(countyCounts[, c(4, 7, 10, 13)])
    #TODO: FIX SUCH THAT ORDCOUNTY IS LOCAL TO THIS FUNCTION....
    countyCounts$TotalTornadoes <- sOrdCountyData[[2]]
    
    overallCountyData <- subset(countyCounts, select = c(County,TotalTornadoes, TotalDeathsByTornado,TotalInjuriesByTornado,TotalLossByTornado))
    
    return(overallCountyData)
  }
  
  getMagInfo <- function(columnType, sData, mag = "mag"){
    magInfo <- sData %>% group_by_(columnType, mag) %>% summarise(Count = n())
    names(magInfo) <- c(columnType, "Magnitude", "Count")
    
    magInfo$Magnitude <- factor(magInfo$Magnitude)
    magInfoTotals <- sData %>% group_by_(columnType) %>% summarise(n())
    names(magInfoTotals) <- c(columnType, "Total")
    
    magInfoTotals <- merge(magInfo,magInfoTotals,by=columnType)
    # take note this data frame has the counts and totals of a specific tornado... not actual percantage
    
    return(magInfoTotals)
  }
  
  getMagInfoByCounty <- function(sData){
    county1 <- getMagInfo("f1", sData)
    county2 <- getMagInfo("f2", sData)
    county3 <- getMagInfo("f3", sData)
    county4 <- getMagInfo("f4", sData)
    
    names(county1) <- c("County", "Magnitude", "Count1", "Total1")
    names(county2) <- c("County", "Magnitude", "Count2", "Total2")
    names(county3) <- c("County", "Magnitude", "Count3", "Total3")
    names(county4) <- c("County", "Magnitude", "Count4", "Total4")
    
    countyCounts <- merge(county1, county2, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    countyCounts <- merge(countyCounts, county3, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    countyCounts <- merge(countyCounts, county4, by=c("County", "Magnitude"), all.x = TRUE, all.y = TRUE)
    countyCounts[is.na(countyCounts)] <- 0
    
    countyCounts$TornadoCount <- rowSums(countyCounts[, c(3, 5, 7, 9)])
    countyCounts$TotalTornado <- rowSums(countyCounts[, c(4, 6, 8, 10)])
    
    overallCountyData <- subset(countyCounts, select = c(County, Magnitude, TornadoCount, TotalTornado))
    
    return(overallCountyData)
  }
  
  getTotalTornadoes <- function(sData){
    county1 <- sData %>% group_by(f1) %>% summarise(n())
    county2 <- sData %>% group_by(f2) %>% summarise(n())
    county3 <- sData %>% group_by(f3) %>% summarise(n())
    county4 <- sData %>% group_by(f4) %>% summarise(n())
    
    names(county1) <- c("County", "Count1")
    names(county2) <- c("County", "Count2")
    names(county3) <- c("County", "Count3")
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
    
    #order ascending
    ordCountyData <- countyData[order(countyData$County),]
    
    return(ordCountyData)
  }
  
#--------DATA---------------------------------------------------------------------------
  #load any data here
  #format:  load("rdata/datatoload.RData") for RData,
  tornadoes <- read.csv(file="data/tornadoes.csv", header=TRUE, sep=",")
  fipsCodes <- read.csv("data/US_FIPS_Codes.csv",header = TRUE, sep =  ",")
  # below tornado data is corrected such that losses are actual values and hour and minutes are seperate columns
  load("rdata/cTornadoes.RData")
  
  # filtered to IL data
  totalTornadoes <- tornadoes %>% filter(st == "IL")
  
  # filter to IL data AND column LOSS represents total dollar amount lost (from 1950 - 1995 I consider top of range,
  # 1996 - 2015 each number is in millions, and post 2016 is actual dollar amount)
  # CORRECTS LOSS SECTION OF totalTornadoes
  load("rdata/adjTotalTornadoesIL.RData")
  totalTornadoes <- adjTotalTornadoesIL
  
  #1
  yearlyTornadoes <- totalTornadoes %>% group_by(Year = totalTornadoes$yr, Magnitude = totalTornadoes$mag) %>% summarise(Count = n())
  #names(yearlyTornadoes) <- c("Year", "Magnitude", "Count")
  yearlyTornadoes$Magnitude <- factor(yearlyTornadoes$Magnitude)
  yearlyTornadoesPercent <- totalTornadoes %>% group_by(yr) %>% summarise(n())
  names(yearlyTornadoesPercent) <- c("Year", "Total")
  yearlyTornadoesPercent <- merge(yearlyTornadoes,yearlyTornadoesPercent,by="Year")
  
  
  #2
  load("rdata/magTotals.RData")
  load("rdata/graphFriendlyMagTotals.RData")
  
  #3
  load("rdata/expandedTotalTornadoes.RData") # this object includes hr and min as seperate columns
  load("rdata/magTotalsByHour.RData")
  load("rdata/graphFriendlymagTotalsByHour.RData")
  totalTornadoes$hr <- expandedTotalTornadoes$hr
  totalTornadoes$min <- expandedTotalTornadoes$min
  
  #4
  #chicago lat: 41.8781 N
  #chicago lng: -87.6298 W
  # need lat, lng, and magnitude. Add magnitude percentage. Add distance.
  #ranges? X or less? X or more? X + 100 ? 
  #start or end? or both?
  
  distTornadoes <- subset(totalTornadoes, select = c("elat", "elon","slat", "slon", "mag"))
  
  #get distance from chicago in miles
  distTornadoes <- distTornadoes %>% rowwise() %>% 
    mutate(eDistance = distm(c(elon, elat), c(-87.6298, 41.8781), fun = distHaversine)[,1] / 1609)
  
  distTornadoes <- distTornadoes %>% rowwise() %>% 
    mutate(sDistance = distm(c(slon, slat), c(-87.6298, 41.8781), fun = distHaversine)[,1] / 1609)
  
  a1<-melt(table(cut(distTornadoes$eDistance,breaks=c(0,10,20,30,60,120,240,480,960)))) 
  
  distTornadoes$eDistance <- round(distTornadoes$eDistance,digits=0)
  distTornadoes$sDistance <- round(distTornadoes$sDistance,digits=0)
  
  distTornadoesCountE <- as.data.frame(table(distTornadoes$eDistance))
  distTornadoesCountS <- as.data.frame(table(distTornadoes$sDistance))
  #omit unknowns
  n<-dim(distTornadoesCountE)[1]
  distTornadoesCountE<-distTornadoesCountE[1:(n-1),]
  
  nn<-dim(distTornadoesCountS)[1]
  distTornadoesCountS<-distTornadoesCountS[1:(n-1),]
  
  
  
  eDistance<-melt(table(cut(distTornadoes$eDistance,breaks=c(0,10,20,30,60,120,240,480,960)))) 
  eDistanceData<-data.frame(sapply(a1,function(x) gsub("\\(|\\]","",gsub("\\,","-",x)))) 
  colnames(eDistanceData)<-c("Miles Away","End Count")
  
  sDistance<-melt(table(cut(distTornadoes$eDistance,breaks=c(0,10,20,30,60,120,240,480,960)))) 
  sDistanceData<-data.frame(sapply(a1,function(x) gsub("\\(|\\]","",gsub("\\,","-",x)))) 
  colnames(sDistanceData)<-c("Miles Away","Start Count")
  
  
  eDistanceData <- merge(eDistanceData,sDistanceData,by="Miles Away")
  
  #5
  totalDamages <- getFullDamageData("yr", totalTornadoes)
  # we followed slightly different naming conventions in our graphs so I'll keep it the same for your data -- Vijay
  names(totalDamages) <- c("Year", "Deaths", "Injuries", "Loss")
  
  #6 
  totalDamagesByMonth <- getFullDamageData("mo", totalTornadoes)

  #7
  totalDamagesByHour <- getFullDamageData("hr", totalTornadoes)

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
  
  #order ascending
  ordCountyData <- countyData[order(countyData$County),]

  #Make data set without 0 counties
  ordCountyDataWithout <- ordCountyData[-1,]
  
  #Top destructive
  topTornadoes <- subset(totalTornadoes, select = c("date", "time", "inj", "fat"))
  topTornadoes$Score <- topTornadoes$fat + topTornadoes$inj
  topTornadoes$WeightScore <- (topTornadoes$fat)*10 + topTornadoes$inj
  topTornadoes <- topTornadoes[order(-topTornadoes$Score),] 
  
  
  
  
  
  
  
  # A -- requirement 1 (DO NOT MOVE... .............K ): 
  # total deaths, injuries, and loss caused by a tornado that started at the county OR passed by the county
  # also includes tornado by magnitude that started at the county OR passed by the county
  # put in function below because I want to expand it for ANY STATE and ANY COUNTY (you know go above and beyond reqs)
  load("rdata/countyDataIL.RData")
  countyDataIL <- countyDataIL[-1,]
  allMags <- c(0, 1, 2, 3, 4, 5, -9)
  #TODO: correct IL county data where there are NA (change the 0)... it makes sense don't worry about it
  
  
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
    tornadoesMap <- totalTornadoes %>% filter(st == "IL" & slon != 0.00)
    #for any position that ends at 0.00 lat/lon, we will set it to be the same as the start 
    tornadoesMap$elat[tornadoesMap$elat == 0.00] <- tornadoesMap$slat 
    tornadoesMap$elon[tornadoesMap$elon == 0.00] <- tornadoesMap$slon
    #only render magnitudes that have been selected 
    tornadoesMap <- tornadoesMap %>% filter(mag == input$magnitudes)
    #check for all the range sliders 
    tornadoesMap <- tornadoesMap %>% filter(len >= input$mapLenSlider[1] & len <= input$mapLenSlider[2] &
                                            wid >= input$mapWidthSlider[1] & wid <= input$mapWidthSlider[2] &
                                            loss >= ( input$mapLossSlider[1] * 1000000) & loss <= (input$mapLossSlider[2] * 1000000)  &
                                            inj >= input$mapInjurySlider[1] & inj <= input$mapInjurySlider[2] &
                                            yr == input$mapYearSlider&
                                            fat >= input$mapFatSlider[1] & fat <= input$mapFatSlider[2] 
                                            )
    #based off the width and color selection, make sure that the values are normalized 
    selectedWidth <- reactiveMapWidth()
    tornadoesMap[,selectedWidth] <- rescale(tornadoesMap[,selectedWidth], to=c(1,6))
    #selectedColor <- reactiveMapColor()
    #tornadoesMap[,selectedColor] <- rescale(tornadoesMap[,selectedColor], to=c(1,6))
    
    #return the data frame 
    tornadoesMap
    
  })
  
  reactiveMapColor <- reactive({
    selected <- input$mapColor
  })
  
  reactiveMapWidth <- reactive({
    selected <- input$mapWidth
  })
  
  colorpal <- reactive({
    selectedColor <- reactiveMapColor()
    tornadoesMap <- reactiveMap()
    colorNumeric(input$colors, as.numeric(tornadoesMap[,selectedColor]))
  })
  
  # TODO: Isabel, put your input here 
  reactiveMapProvider <- reactive({
    #get input
    choice <- input$mapChoice
    choice <- as.integer(choice)
    print(choice)
    #choose map
    if(choice == 1){
      selected <- "CartoDB.Positron"
    }
    else if(choice == 2) {
      selected <- "CartoDB.DarkMatterNoLabels"
    }
    else if(choice == 3) {
      selected <- "Stamen.Terrain"
    }
    else{
      selected <- "Esri.WorldImagery"
    }
      
  })
  
  getCountyDataByState <- reactive({
    chosenState <- as.character(input$cState)
    
    if(chosenState == "IL"){
      countyData <- countyDataIL
    } else {
      chosenStateData <- cTornadoes %>% filter(st == chosenState)
      chTotalTornadoes <- getTotalTornadoes(chosenStateData)
      countyData <- data.frame(
        getFullDamageDataByCounty(chosenStateData, chTotalTornadoes),
        mag0 = 0, mag1 = 0, mag2 = 0, mag3 = 0, mag4 = 0, mag5 = 0, magUnknown = 0
      )
      countyMagInfo <- getMagInfoByCounty(chosenStateData)
      
      for(i in 1:length(countyData$County)){
        for(j in 1:length(allMags)){
            temp <- countyMagInfo %>% filter(County == countyData$County[i]) %>% filter(Magnitude == allMags[[j]])
            if(length(temp$Magnitude) == 0){
              countyData[[j + 5]][i] <- 0
            } else {
              countyData[[j + 5]][i] <- temp$TornadoCount
            }
        }
        currentCounty <- countyData$County[i]
        if (as.numeric(currentCounty) < 10){
          countyData$County[i] <- paste("00", currentCounty, sep = "")
        } else if (as.numeric(currentCounty) < 100){
          countyData$County[i] <- paste("0", currentCounty, sep = "")
        }
      }
      
      countyData <- countyData[-1,]
    }
    
    countyData
  })
  
  getMergedCountyDataByState <- reactive({
    chosenState <- as.character(input$cState)
    selectedStateCounties <- counties(state = chosenState, cb = TRUE, resolution = '20m')
    
    totalTornadoData <- getCountyDataByState()
    colnames(totalTornadoData) <- c("COUNTYFP", "TotalTornadoes", "TotalDeathsByTornado", "TotalInjuriesByTornado", "TotalLossByTornado", "mag0", "mag1", "mag2", "mag3", "mag4", "mag5", "magUnknown")
    
    mCountyData <- geo_join(selectedStateCounties, totalTornadoData, "COUNTYFP", "COUNTYFP")
    mCountyData
  })
  
  getCountyDataByStateTable <- reactive({
    mCountyData <- getMergedCountyDataByState()
    
    chosenData <- data.frame(
      County = mCountyData$NAME,
      FIPS = mCountyData$COUNTYFP,
      TotalTornadoes = mCountyData$TotalTornadoes,
      TotalDeathsByTornado = mCountyData$TotalDeathsByTornado,
      TotalInjuriesByTornado = mCountyData$TotalInjuriesByTornado,
      TotalLossByTornado = mCountyData$TotalLossByTornado,
      mag0 = mCountyData$mag0, mag1 = mCountyData$mag1, mag2 = mCountyData$mag2, mag3 = mCountyData$mag3, mag4 = mCountyData$mag4, mag5 = mCountyData$mag5, magUnknown = mCountyData$magUnknown
    )
    chosenData <- chosenData[order(chosenData$FIPS),]
    
    chosenData
  })
  
  getMagTypeText <- reactive({
    if (input$mapChosenMag > 5){
      thisText <- paste('All Tornadoes')
    } else if (input$mapChosenMag < 0) {
      thisText <- thisText <- paste('MagUnknown', ' Tornadoes')
    } else {
      thisText <- thisText <- paste('Mag', as.character(input$mapChosenMag), ' Tornadoes', sep = '')
    }
  })
  
  getCountyStateTypeText <- reactive({
    chosenText <- paste(as.character(input$cState), '- 1950 to 2016')
    chosenText
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
  
  output$yearlyTornadoTable <- renderDataTable(yearlyTornadoesPercent, extensions = 'Scroller', 
                                              rownames = FALSE, options = list(
                                                deferRender = TRUE,
                                                scrollY = 800,
                                                scroller = TRUE,
                                                bFilter=0
                                              )
  )
  
  output$eDistanceTable<- renderDataTable(eDistanceData, extensions = 'Scroller', 
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
  
  output$countyDataTable <- renderDataTable(getCountyDataByStateTable(), extensions = 'Scroller', 
                                                     rownames = FALSE, options = list(
                                                       deferRender = TRUE,
                                                       scrollY = 800,
                                                       scroller = TRUE,
                                                       bFilter=0
                                                     )
  )
  
  output$yearSelected <- renderText({ input$mapYearSlider})


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
  
  output$yearlyGraphPer <- renderPlotly({
    
    ggplot(yearlyTornadoesPercent, aes(x = Year, y = ((Count/Total) *100) ,fill = Magnitude)) + 
      ggtitle("Yearly Tornado Count by Magnitude") + geom_bar(stat = "identity", position = "stack")
    
  })
  
  output$countyGraphWithout <- renderPlotly({
    
    plot_ly(
      x = ordCountyDataWithout$County,
      y = ordCountyDataWithout$`Total Tornadoes`,
      type = "bar"
    ) %>%
      layout(font = list(size=30), xaxis = list(title = "County Code", autotick = T, tickangle = 0, dtick = 1, titlefont=list(size=25), tickfont=list(size=20)),
             yaxis = list(title = "# of Tornadoes", titlefont=list(size=30), tickfont=list(size=20)),
             margin = list(l = 100, b = 100),
             barmode = 'group')
    
    
  })
  
  output$countyGraph <- renderPlotly({
    
    plot_ly(
      x = ordCountyData$County,
      y = ordCountyData$`Total Tornadoes`,
      type = "bar"
    ) %>%
      layout(font = list(size=30), xaxis = list(title = "County Code", autotick = T, tickangle = 0, dtick = 1, titlefont=list(size=25), tickfont=list(size=20)),
             yaxis = list(title = "# of Tornadoes", titlefont=list(size=30), tickfont=list(size=20)),
             margin = list(l = 100, b = 100),
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
  
  output$distanceCountGraph <- renderPlotly({
    
    s <- seq(1, 322, by = 1)
    plot_ly(distTornadoesCountE, x = ~s, y = ~Freq, type = 'scatter', name = 'End', mode = 'lines') %>%
      add_trace(y = ~distTornadoesCountS$Freq, name = 'Start', mode = 'lines') %>%
      layout(font = list(size=30), xaxis = list(title = "Distance (miles)", autotick = T, tickangle = 0, dtick = 1, titlefont=list(size=25), tickfont=list(size=20)),
             yaxis = list(title = "# of Tornadoes", titlefont=list(size=30), tickfont=list(size=20)),
             margin = list(l = 100, b = 100),
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
  
  output$magTypeText <- renderText({ getMagTypeText() })
  
  output$countyStateTypeText <- renderText({ getCountyStateTypeText() })
  
  
#--------MAP-----------------------------------------------------------------------
  
  # add a leaflet map and put markers where the deaths occured
  
  output$map <- renderLeaflet({
    
    tornadoesMap <- reactiveMap()
    selectedWidth <- reactiveMapWidth()
    selectedColor <- reactiveMapColor()
    pal <- colorpal()
    
    m <-leaflet(tornadoesMap) %>%  addTiles() 
    
    #adding all the different lines 
    for(i in 1:nrow(tornadoesMap)){
    m <-  addPolylines(m,data = tornadoesMap, weight = as.numeric(tornadoesMap[i,selectedWidth]), color = pal(tornadoesMap[1,selectedColor]), #popup = ~paste(tornadoesMap$yr),
         lat = as.numeric(tornadoesMap[i, c('slat','elat' )]), lng = as.numeric(tornadoesMap[i, c('slon', 'elon')])) 
    }
  
  #adding circles to denote start and end of all the tornadoes 
  m <- addCircleMarkers(m, lng = tornadoesMap$slon , lat = tornadoesMap$slat , radius = 2, color = "green", fillColor = "green")
  m <- addCircleMarkers(m, lng = tornadoesMap$elon , lat = tornadoesMap$elat , radius = 2, color = "red", fillColor = "red")
    
    
    # change the theme to what ever is selected
    m = addProviderTiles(map = m, provider = reactiveMapProvider())
    m
  })
  
  
  # similar approach as found here: http://rstudio-pubs-static.s3.amazonaws.com/90665_de25062951e540e7b732f21de53001f0.html
  output$mapTotalTornadoes <- renderLeaflet({
    mCountyData <- getMergedCountyDataByState()
    
    if (input$mapChosenMag > 5){
      chosenData <- mCountyData$TotalTornadoes
    } else if (input$mapChosenMag < 0) {
      chosenData <- mCountyData$magUnkown
    } else {
      chosenData <- mCountyData[[as.numeric(input$mapChosenMag) + 15]]
    }
    
    pal <- colorNumeric("viridis", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = mCountyData,
                  fillColor = ~pal(as.numeric(chosenData)),
                  fillOpacity = 0.7,
                  color = "#444444",
                  opacity = 1.0,
                  weight = 1.0, 
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "red", weight = 10, bringToFront = TRUE),
                  label = ~paste(mCountyData$NAME, '(FIPS=', mCountyData$COUNTYFP, '): ', formatC(chosenData, big.mark = ","))) %>%
      addLegend(pal = pal, 
                values = chosenData, 
                position = "bottomright", 
                title = "# of Tornado")
  })

  output$mapDeathsByTornadoes<- renderLeaflet({
    mCountyData <- getMergedCountyDataByState()
    chosenData <- mCountyData$TotalDeathsByTornado
    
    pal <- colorNumeric("viridis", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = mCountyData,
                  fillColor = ~pal(as.numeric(chosenData)),
                  fillOpacity = 0.7,
                  color = "#444444",
                  opacity = 1.0,
                  weight = 1.0, 
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "red", weight = 10, bringToFront = TRUE),
                  label = ~paste(mCountyData$NAME, '(FIPS=', mCountyData$COUNTYFP, '): ', formatC(chosenData, big.mark = ","))) %>%
      addLegend(pal = pal, 
                values = chosenData, 
                position = "bottomright", 
                title = "# of Deaths")
  })
  
  output$mapInjuriesByTornadoes <- renderLeaflet({
    mCountyData <- getMergedCountyDataByState()
    chosenData <- mCountyData$TotalInjuriesByTornado
    
    pal <- colorNumeric("viridis", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = mCountyData,
                  fillColor = ~pal(as.numeric(chosenData)),
                  fillOpacity = 0.7,
                  color = "#444444",
                  opacity = 1.0,
                  weight = 1.0, 
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "red", weight = 10, bringToFront = TRUE),
                  label = ~paste(mCountyData$NAME, '(FIPS=', mCountyData$COUNTYFP, '): ', formatC(chosenData, big.mark = ","))) %>%
      addLegend(pal = pal, 
                values = chosenData, 
                position = "bottomright", 
                title = "# of Deaths")
  })
  
  output$mapLossByTornadoes <- renderLeaflet({
    mCountyData <- getMergedCountyDataByState()
    chosenData <- mCountyData$TotalLossByTornado
    
    pal <- colorNumeric("viridis", NULL)
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = mCountyData,
                  fillColor = ~pal(as.numeric(chosenData)),
                  fillOpacity = 0.7,
                  color = "#444444",
                  opacity = 1.0,
                  weight = 1.0, 
                  smoothFactor = 0.2,
                  highlightOptions = highlightOptions(color = "red", weight = 10, bringToFront = TRUE),
                  label = ~paste(mCountyData$NAME, '(FIPS=', mCountyData$COUNTYFP, '): ', formatC(chosenData, big.mark = ","))) %>%
      addLegend(pal = pal, 
                values = chosenData, 
                position = "bottomright", 
                title = "# of Deaths")
  })
  
}
