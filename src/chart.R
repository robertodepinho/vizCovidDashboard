covidBlue <- function(selVar, tsCAgg,listP, anchorCases,days, cases.y, logscale.ctrl, countryList) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  nPaises = length(unique(countryList))
  
  
  tsCShiftList = tsCShift[ tsCShift$Country.Region %in% countryList,]
  
  tsCShiftList = tsCShiftList[ order(tsCShiftList$Country.Region, tsCShiftList$diffDate),]
  
  serie_2 = data.frame( x = rep(tsCShiftList$diffDate,nPaises) , y = rep(tsCShiftList$selVarValue,nPaises), 
                        cnt = rep(tsCShiftList$Country.Region,nPaises), Country.Region = rep(unique(countryList),nrow(tsCShiftList)))
  
  
  serie_label = data.frame(x = rep(labelSubset$diffDate, nPaises),
                           y = rep(labelSubset$selVarValue, nPaises), 
                           label =rep(labelSubset$cnt.Code,nPaises),
                           Country.Region = rep(unique(countryList),each = nrow(labelSubset)))
  
  covidBluePlot = ggplot(data=tsCShiftList,aes(x=diffDate, y=selVarValue,label = Country.Region)) + 
    geom_text_repel(data = serie_label, aes(x=x, y=y,label = label), colour = "gray50", size = 3) +
    geom_line(size = 1, data = serie_2, aes(x=x, y=y, group = cnt), colour = "gray") + 
    geom_point(size = 1) +
    geom_line(size = 1, colour = "darkblue") +
    facet_wrap(~Country.Region) + 
    coord_cartesian(xlim=c(xlimBot, xlimSup),  ylim = c(1,ylimSup)) + 
    scale_color_discrete() +
    scale_x_continuous(name=paste("Days from first day with", anchorCases," or more ", selVar, sep=" ")) +
    #                     breaks = xlimBot:xlimSup) +
    scale_y +
    #scale_y_continuous( trans = "log10", limits = c(-1,125), breaks = c(-1, 5))  + 
    theme(legend.position = "none", legend.title =  element_blank()) 
  
  covidBluePlot =  covidBluePlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                            x = Inf, y = 1, vjust=0, hjust=1.1)
  
  covidBluePlot
  
  
  
}

covidBlueDate <- function(selVar, tsCAgg,listP, anchorCases,days, cases.y, logscale.ctrl, countryList) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  nPaises = length(unique(countryList))
  
  
  tsCShiftList = tsCShift[ tsCShift$Country.Region %in% countryList,]
  
  tsCShiftList = tsCShiftList[ order(tsCShiftList$Country.Region, tsCShiftList$Date),]
  
  serie_2 = data.frame( x = rep(tsCShiftList$Date,nPaises) , y = rep(tsCShiftList$selVarValue,nPaises), 
                        cnt = rep(tsCShiftList$Country.Region,nPaises), Country.Region = rep(unique(countryList),nrow(tsCShiftList)))
  
  
  serie_label = data.frame(x = rep(labelSubset$Date, nPaises),
                           y = rep(labelSubset$selVarValue, nPaises), 
                           label =rep(labelSubset$cnt.Code,nPaises),
                           Country.Region = rep(unique(countryList),each = nrow(labelSubset)))
  
  covidBluePlot = ggplot(data=tsCShiftList,aes(x=Date, y=selVarValue,label = Country.Region)) + facet_wrap(~Country.Region) 
  covidBluePlot = covidBluePlot  + 
    geom_text_repel(data = serie_label, aes(x=x, y=y,label = label), colour = "gray50", size = 3) 
  covidBluePlot = covidBluePlot  + 
    geom_line(size = 1, data = serie_2, aes(x=x, y=y, group = cnt), colour = "gray") 
  
  
   covidBluePlot = covidBluePlot  + geom_point(size = 1) 
   covidBluePlot = covidBluePlot  + geom_line(size = 1, colour = "darkblue") 
  # covidBluePlot = covidBluePlot  
   covidBluePlot = covidBluePlot  + coord_cartesian(ylim = c(1,ylimSup)) 
   covidBluePlot = covidBluePlot  + scale_color_discrete() 
   covidBluePlot = covidBluePlot  + scale_y + theme(legend.position = "none", legend.title =  element_blank()) 
   
   covidBluePlot =  covidBluePlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                             x = max(tsCShiftList$Date, na.rm=T), y = 1, vjust=0, hjust=1.1)
 
   covidBluePlot = covidBluePlot  + scale_x_date(date_breaks = "1 month", date_labels = "%b")
  
   return(covidBluePlot)
   
}


covidColor <- function(selVar,tsCAgg,listP, anchorCases,days, cases.y, logscale.ctrl, countryList, mark.ctrl, high.ctrl, doublingTime, est.ctrl) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  
  covidColorPlot = ggplot(data=tsCShift[tsCShift$Country.Region %in% countryList,]) 
  
  if("Color" %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(x=diffDate, y=selVarValue, colour = Country.Region, shape = Country.Region, 
                                           label = Country.Region) 
  } else {
    covidColorPlot =  covidColorPlot + aes(x=diffDate, y=selVarValue, shape = Country.Region, 
                                           label = Country.Region) 
  }
  
  if("Line Style" %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(linetype = Country.Region) 
  } 
  
  
  covidColorPlot =  covidColorPlot +coord_cartesian(xlim=c(xlimBot, xlimSup),  ylim = c(1,ylimSup))  
  
  covidColorPlot =  covidColorPlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                              x = Inf, y = 1, vjust=0, hjust=1.1)
  
  if("Background Lines" %in% mark.ctrl) {
    x = tsCShift[tsCShift$Group %in% "JHU.C" & !(tsCShift$Country.Region %in%  countryList),]
    covidColorPlot = covidColorPlot  + geom_line(size = 1, data = x , 
                                                 aes(x=diffDate, y=selVarValue), colour = "grey90",
                                                 linetype = "solid",
                                                 show.legend = FALSE , shape = 1) 
  } 
  
  if(est.ctrl !="None") {
    b = b = 2^ (1/doublingTime)
    if(logScale){
      fun.l <- function(x) {log(anchorCases * b^(x), 10)}
    } else {  
      fun.l <- function(x) {anchorCases * b^(x)}
    }
    covidColorPlot = covidColorPlot  + stat_function(fun = fun.l, colour = "green")
  }
  
  if(high.ctrl !="None") {
    covidColorPlot = covidColorPlot  +   
      geom_line(size = 2, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], 
                aes(x=diffDate, y=selVarValue, colour = Country.Region),
                show.legend = FALSE) 
    
  }
  
  covidColorPlot = covidColorPlot  +
    geom_text_repel(data = labelSubset, aes(label = Country.Region)) +
    scale_color_discrete() +
    scale_x_continuous(name=paste("Days from first day with", anchorCases," or more", selVar, sep=" ") ) +
    #                     breaks = xlimBot:xlimSup) +
    scale_y 
  #scale_y_continuous( trans = "log10", limits = c(-1,125), breaks = c(-1, 5))  + 
  
  covidColorPlot = covidColorPlot  +  theme_bw()
  covidColorPlot = covidColorPlot  + theme(legend.position =  "top", legend.title =  element_blank()) 
  
  
  
  if("Marker" %in% mark.ctrl) {
    covidColorPlot = covidColorPlot + geom_point(size = 5, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], shape = 1,
                                                 aes(x=diffDate, y=selVarValue, colour = Country.Region),
                                                 show.legend = FALSE) +
      geom_point(size = 3, show.legend = !("Background Lines" %in% mark.ctrl)) 
  }
  
  if("Line" %in% mark.ctrl) {
    covidColorPlot = covidColorPlot  + geom_line(size = 1, show.legend = !("Background Lines" %in% mark.ctrl)) 
  }
  
  
  covidColorPlot
  
}

covidColorDate <- function(selVar,tsCAgg,listP, anchorCases,days, cases.y, logscale.ctrl, countryList, mark.ctrl, high.ctrl, doublingTime, est.ctrl) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  
  covidColorPlot = ggplot(data=tsCShift[tsCShift$Country.Region %in% countryList,]) 
  
  if("Color" %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(x=Date, y=selVarValue, colour = Country.Region, shape = Country.Region, 
                                           label = Country.Region) 
  } else {
    covidColorPlot =  covidColorPlot + aes(x=Date, y=selVarValue, shape = Country.Region, 
                                           label = Country.Region) 
  }
  
  if("Line Style" %in% mark.ctrl) {  
    covidColorPlot =  covidColorPlot + aes(linetype = Country.Region) 
  } 
  if("Line" %in% mark.ctrl) {
    covidColorPlot = covidColorPlot  + geom_line(size = 1) 
  }
  
  
  covidColorPlot =  covidColorPlot +coord_cartesian(xlim=c(min(tsCAgg$Date, na.rm=T), max(tsCAgg$Date, na.rm=T)),  ylim = c(1,ylimSup))  
  covidColorPlot =  covidColorPlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                              x = max(tsCAgg$Date, na.rm=T), y = 1, vjust=0, hjust=1.1)
  if("Background Lines" %in% mark.ctrl) {
    x = tsCShift[tsCShift$Group %in% "JHU.C" & !(tsCShift$Country.Region %in%  countryList),]
    covidColorPlot = covidColorPlot  + geom_line(size = 1, data = x , 
                                                 aes(x=Date, y=selVarValue), colour = "grey90", 
                                                 linetype = "solid",
                                                 show.legend = FALSE ) 
  } 
  
  if(est.ctrl !="None") {
    b = b = 2^ (1/doublingTime)
    minX = as.numeric(min(tsCAgg$Date, na.rm=T))
    if(logScale){
      fun.l <- function(x) {log( b^(as.numeric(x)-minX), 10)}
    } else {
      fun.l <- function(x) { b^(as.numeric(x)-minX)}
    }
    covidColorPlot = covidColorPlot  + stat_function(fun = fun.l, colour = "green")
  }
  
  if(high.ctrl !="None") {
    covidColorPlot = covidColorPlot  +   
      geom_line(size = 2, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], 
                aes(x=Date, y=selVarValue, colour = Country.Region),
                show.legend = FALSE) 
  }
  
  #!#
  covidColorPlot = covidColorPlot  +
    geom_text_repel(data = labelSubset, aes(label = Country.Region)) 
  covidColorPlot = covidColorPlot  + scale_color_discrete() 
  covidColorPlot = covidColorPlot  + scale_y
  covidColorPlot = covidColorPlot  +  theme_bw()
  covidColorPlot = covidColorPlot  + theme(legend.position =  "top", legend.title =  element_blank()) 
  
  if("Marker" %in% mark.ctrl) {
    covidColorPlot = covidColorPlot + geom_point(size = 5, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], shape = 1,
                                                 aes(x=Date, y=selVarValue, colour = Country.Region),
                                                 show.legend = FALSE) +
      geom_point(size = 3, show.legend = !("Background Lines" %in% mark.ctrl)) 
  }
  
  
  
  covidColorPlot = covidColorPlot  + scale_x_date(date_breaks = "1 week", date_labels = "%d %b")
  
  return(covidColorPlot)
  
}

