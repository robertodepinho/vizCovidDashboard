
source("src/theme_black.R")

winNearAction <- function(tsCAgg, CountryRegion = NA) {
  tsCAgg = tsCAgg[order(tsCAgg$Date),]
  
  if(is.na(CountryRegion)) {
    dfr = tsCAgg  } else { 
    dfr = subset(tsCAgg, Country.Region %in% CountryRegion)
  }
  
  dfr = subset(dfr, !is.na(NewCasesAvg))
  
  lastc = aggregate(NewCasesAvg ~Country.Region, dfr, tail, 1)
  maxc = aggregate(NewCasesAvg ~Country.Region, dfr, max, na.rm=T)
  dfa = merge(lastc, maxc, by = "Country.Region", all = F)
  colnames(dfa)[colnames(dfa)=="NewCasesAvg.x"] = "last_c"
  colnames(dfa)[colnames(dfa)=="NewCasesAvg.y"] = "max_c"
  
  dfa$per = dfa$last_c / dfa$max_c * 100
  
  trend <- function(dfr) {
    m = lm(NewCasesAvg ~ Date, tail(dfr,15))
    m$coefficients[2]
  }
  
  library(dplyr)
  dft = dfr %>% 
    group_by(Country.Region) %>%
    do(data.frame(coef=trend(.)))
  
  dfa = merge(dfa, dft, by = "Country.Region", all = F)
  dfa$fall = dfa$coef < 0 
  dfa$days = ifelse(dfa$fall, - dfa$last_c / dfa$coef, +Inf)
  
  
  ################
  winners = as.character(subset(dfa, last_c < 22)$Country.Region)
  dfa$status[dfa$Country.Region %in% winners] = "win" 
  almost = as.character(subset(dfa, !(status %in% "win") & per < 21)$Country.Region)
  dfa$status[dfa$Country.Region %in% almost] = "near"
  dfa$status[ is.na(dfa$status)] = "action"
  # winners = as.character(subset(dfa, last_c <= 18 & per < 50)$Country.Region)
  # dfa$status[dfa$Country.Region %in% winners] = "win" 
  # almost = as.character(subset(dfa, !(status %in% "win") & (fall | per < 25) & days < 30 & per < 50)$Country.Region)
  # dfa$status[dfa$Country.Region %in% almost] = "near"
  # dfa$status[ is.na(dfa$status)] = "action"
  
  return(dfa)
  
}


winNearAction_deprecated <- function(tsCAgg, CountryRegion = NA) {
  tsCAgg = tsCAgg[order(tsCAgg$Date),]
  
  if(is.na(CountryRegion)) {
    dfr = tsCAgg  }
  else { 
    dfr = subset(tsCAgg, Country.Region %in% CountryRegion)
  }
  
  dfr = subset(dfr, !is.na(NewCasesAvg))
  
  lastc = aggregate(NewCasesAvg ~Country.Region, dfr, tail, 1)
  maxc = aggregate(NewCasesAvg ~Country.Region, dfr, max, na.rm=T)
  dfa = merge(lastc, maxc, by = "Country.Region", all = F)
  
  dfa$per = dfa$NewCasesAvg.x / dfa$NewCasesAvg.y * 100
  
  winners = as.character(subset(dfa, NewCasesAvg.x <= 18 & per < 50)$Country.Region)
  dfa$status[dfa$Country.Region %in% winners] = "win" 
  
  trend <- function(dfr) {
    m = lm(NewCasesAvg ~ Date, tail(dfr,15))
    m$coefficients[2]
  }
  
  library(dplyr)
  dft = dfr %>% 
    group_by(Country.Region) %>%
    do(data.frame(val=trend(.)))
  
  dfa = merge(dfa, dft, by = "Country.Region", all = F)
  dfa$days = - dfa$NewCasesAvg.x / dfa$val
  almost = as.character(subset(dfa, !(status %in% "win") & (val < 0 | per < 25) & days < 21 & per < 50)$Country.Region)
  dfa$status[dfa$Country.Region %in% almost] = "near"
  
  dfa$status[ is.na(dfa$status)] = "action"
  
  return(dfa)
  
}

chartDataPrepare <- function(selVar,tsCAgg,anchorCases, days, cases.y, 
                             logscale.ctrl, countryList, 
                             backgroundList, lg = "pt", translation) {
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl    
  
  df = subset(tsCAgg, Country.Region %in% countryList |Country.Region %in% backgroundList  )
  
  
  
  selVarCol = which(colnames(df) %in% gsub(" ", "", selVar))
  
  
  
  df$selVarValue = df[,selVarCol]
  
  
  qtdRows = sum( (df$selVarValue >= anchorCases), na.rm = T )
  if(qtdRows < 1) {return(list(FALSE))}
  
  #for each country, 1st day with at least anchorNumber
  anchorDate = aggregate(Date ~ Country.Region, df[df$selVarValue >= anchorCases,], min)
  
  qtdRows = sum( (df$Country.Region %in% countryList) & (df$selVarValue >= anchorCases) & (df$Country.Region %in% anchorDate$Country.Region), na.rm=T)
  if(qtdRows < 1) {return(list(FALSE))}
  
  colnames(anchorDate)[colnames(anchorDate) %in% "Date" ] = "anchorDate"
  #remove low cases countries
  df = df[ df$Country.Region %in% anchorDate$Country.Region,]
  
  tsCShift = merge(df, anchorDate, by = "Country.Region", all.x = T)
  
  
  tsCShift$diffDate = tsCShift$Date - tsCShift$anchorDate
  maxDays = max(as.numeric(tsCShift$diffDate), na.rm=T) + 1
  
  
  if(logScale){
    scale_y =   scale_y_log10( labels =  label_dollar(prefix=""), name = paste(translation[[selVar]][[lg]], " (log)", sep=" "))
    listaRows = which(!is.na(tsCShift$selVarValue) &  tsCShift$selVarValue == 0)
    if(length(listaRows)>0) {
      tsCShift$selVarValue[listaRows] = tsCShift$selVarValue[listaRows] + 1
    }
  } else {
    scale_y = scale_y_continuous( labels =  label_dollar(prefix=""), name = paste(translation[[selVar]][[lg]], " ", sep=" "))
  }
  
  if(anchorCases>0) {
    labelSubset = tsCShift[ tsCShift$selVarValue <  ylimSup & 
                              as.numeric(tsCShift$diffDate) <= xlimSup &
                              tsCShift$Country.Region %in% countryList
                            ,]
    maxDiff = aggregate(diffDate ~Country.Region, labelSubset, max)
    labelSubset = merge(labelSubset, maxDiff, by = "Country.Region")
    labelSubset = labelSubset[ as.numeric(labelSubset$diffDate.x) == labelSubset$diffDate.y, ]
    labelSubset$diffDate = labelSubset$diffDate.x
    labelSubset$cnt.Code = countrycode(sourcevar = labelSubset$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
    
  } else {
    labelSubset = tsCShift[ tsCShift$selVarValue <  ylimSup & 
                              
                              tsCShift$Country.Region %in% countryList
                            ,]
    maxDate = aggregate(Date ~Country.Region, labelSubset, max)
    labelSubset = merge(labelSubset, maxDate, by = "Country.Region")
    labelSubset = labelSubset[ as.numeric(labelSubset$Date.x) == labelSubset$Date.y, ]
    labelSubset$Date = labelSubset$Date.x
    labelSubset$cnt.Code = countrycode(sourcevar = labelSubset$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
    
  }
  
  
  return(list(TRUE,tsCShift, logScale,labelSubset, scale_y, maxDays))
  
}





covidBlue <- function(selVar, tsCAgg,listP, anchorCases,days, 
                      cases.y, logscale.ctrl, countryList, 
                      lg = "pt", translation) {
  
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
    scale_x_continuous(name=paste(translation[['days_from']][[lg]], anchorCases,translation[['or_more']][[lg]], translation[[selVar]][[lg]], sep=" ")) +
    #                     breaks = xlimBot:xlimSup) +
    scale_y 
  #scale_y_continuous( trans = "log10", limits = c(-1,125), breaks = c(-1, 5))  + 
  
  covidBluePlot =  covidBluePlot +  theme_jf()
  covidBluePlot =  covidBluePlot + theme(legend.position = "none", legend.title =  element_blank()) 
  
  # covidBluePlot =  covidBluePlot + annotate("text", label = "@robertodepinho", color= "grey50",
  #                                           x = Inf, y = 1, vjust=0, hjust=1.1)
  # 
  
  covidBluePlot =  covidBluePlot + labs(tag = "@robertodepinho") + 
    theme(plot.tag.position = c(.8, .05), plot.tag = element_text(color="gray50", size=8))
  
  
  
  covidBluePlot
  
  
  
}

covidBlueDate <- function(selVar, tsCAgg,listP, anchorCases,days, cases.y, 
                          logscale.ctrl, countryList, date_range, 
                          showLabels = TRUE, showWinNearAction = FALSE,
                          showBack = TRUE, scaleFree = FALSE, 
                          lg = "pt", translation) {
  
  
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
  if(showWinNearAction){
    x = winNearAction(tsCShift, CountryRegion = countryList)
    tsCShiftList = merge(tsCShiftList, x[, c("Country.Region","status")], by="Country.Region", all.x = TRUE)
    
  } else tsCShiftList$status = "darkblue"
  
  covidBluePlot = ggplot(data=tsCShiftList,aes(x=Date, y=selVarValue,
                                               label = Country.Region,
                                               colour = status)) 
  if(scaleFree) {
    covidBluePlot = covidBluePlot  + facet_wrap(~Country.Region, scales = "free") 
  } else {
    covidBluePlot = covidBluePlot  + facet_wrap(~Country.Region) 
  }
  
  if(showLabels) { 
    covidBluePlot = covidBluePlot  + 
      geom_text_repel(data = serie_label, aes(x=x, y=y,label = label), colour = "gray50", size = 3) 
  }
  
  if(showBack) {
    covidBluePlot = covidBluePlot  + 
      geom_line(size = 1, data = serie_2, aes(x=x, y=y, group = cnt), colour = "gray") 
  }
  
  #covidBluePlot = covidBluePlot  + geom_point(size = 1) 
  covidBluePlot = covidBluePlot  + geom_line(size = 1) 
  # covidBluePlot = covidBluePlot  
  if(!scaleFree) {
    covidBluePlot = covidBluePlot  + coord_cartesian(ylim = c(1,ylimSup)) 
  }
  covidBluePlot = covidBluePlot  + scale_color_manual(breaks = c("win", "near", "action", "darkblue"),
                                                      values =  c("darkgreen", "orange", "red", "darkblue")) 
  
  covidBluePlot =  covidBluePlot +  theme_jf()
  covidBluePlot = covidBluePlot  + scale_y + theme(legend.position = "none", legend.title =  element_blank()) 
  
  # covidBluePlot =  covidBluePlot + annotate("text", label = "@robertodepinho", color= "grey50",
  #                                           x = max(tsCShiftList$Date, na.rm=T), y = 1, vjust=0, hjust=1.1)
  # 
  covidBluePlot =  covidBluePlot + labs(tag = "@robertodepinho") + 
    theme(plot.tag.position = c(.8, .05), plot.tag = element_text(color="gray50", size=8))
  
  covidBluePlot = covidBluePlot  + scale_x_date(name="",
                                                date_breaks = "1 month", date_labels = "%b", limits = date_range)
  
  
  
  return(covidBluePlot)
  
}


covidColor <- function(selVar,tsCAgg,listP, anchorCases,days, cases.y, 
                       logscale.ctrl, countryList, mark.ctrl, 
                       high.ctrl, doublingTime, est.ctrl,  
                       backgroundList, lg = "pt", translation) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  
  covidColorPlot = ggplot(data=tsCShift[tsCShift$Country.Region %in% countryList,]) 
  
  if(translation[['color']][[lg]] %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(x=diffDate, y=selVarValue, colour = Country.Region, shape = Country.Region, 
                                           label = Country.Region) 
  } else {
    covidColorPlot =  covidColorPlot + aes(x=diffDate, y=selVarValue, shape = Country.Region, 
                                           label = Country.Region) 
  }
  
  if(translation[['line_style']][[lg]] %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(linetype = Country.Region) 
  } 
  
  
  covidColorPlot =  covidColorPlot +coord_cartesian(xlim=c(xlimBot, xlimSup),  ylim = c(1,ylimSup))  
  
  covidColorPlot =  covidColorPlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                              x = Inf, y = 1, vjust=0, hjust=1.1)
  
  if(translation[['background_lines']][[lg]] %in% mark.ctrl) {
    #x = tsCShift[tsCShift$Group %in% "JHU.C" & !(tsCShift$Country.Region %in%  countryList),]
    x = tsCShift[tsCShift$Country.Region %in%  backgroundList & !(tsCShift$Country.Region %in%  countryList),]
    covidColorPlot = covidColorPlot  + geom_line(size = 1, data = x , 
                                                 aes(x=diffDate, y=selVarValue), colour = "grey90",
                                                 linetype = "solid",
                                                 show.legend = FALSE , shape = 1) 
  } 
  
  if(est.ctrl !=translation[['none']][[lg]]) {
    b = b = 2^ (1/doublingTime)
    if(logScale){
      fun.l <- function(x) {log(anchorCases * b^(x), 10)}
    } else {  
      fun.l <- function(x) {anchorCases * b^(x)}
    }
    covidColorPlot = covidColorPlot  + stat_function(fun = fun.l, colour = "green")
  }
  
  if(high.ctrl !=translation[['none']][[lg]]) {
    covidColorPlot = covidColorPlot  +   
      geom_line(size = 2, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], 
                aes(x=diffDate, y=selVarValue, colour = Country.Region),
                show.legend = FALSE) 
    
  }
  
  covidColorPlot = covidColorPlot  +
    geom_text_repel(data = labelSubset, aes(label = Country.Region)) +
    scale_color_discrete() +
    scale_x_continuous(name=paste(translation[['days_from']][[lg]], anchorCases,translation[['or_more']][[lg]], translation[[selVar]][[lg]], sep=" ") ) +
    #                     breaks = xlimBot:xlimSup) +
    scale_y 
  #scale_y_continuous( trans = "log10", limits = c(-1,125), breaks = c(-1, 5))  + 
  
  covidColorPlot = covidColorPlot  +  theme_jf()
  covidColorPlot = covidColorPlot  + theme(legend.position =  "top", legend.title =  element_blank()) 
  
  
  
  if(translation[['marker']][[lg]] %in% mark.ctrl) {
    covidColorPlot = covidColorPlot + geom_point(size = 5, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], shape = 1,
                                                 aes(x=diffDate, y=selVarValue, colour = Country.Region),
                                                 show.legend = FALSE) +
      geom_point(size = 3, show.legend = !(translation[['background_lines']][[lg]] %in% mark.ctrl)) 
  }
  
  if(translation[['line']][[lg]] %in% mark.ctrl) {
    covidColorPlot = covidColorPlot  + geom_line(size = 1, show.legend = !(translation[['background_lines']][[lg]] %in% mark.ctrl)) 
  }
  
  
  covidColorPlot
  
}

covidColorDate <- function(selVar,tsCAgg,listP, anchorCases,days, 
                           cases.y, logscale.ctrl, countryList, 
                           mark.ctrl, high.ctrl, doublingTime, 
                           est.ctrl, date_range, 
                           backgroundList, lg = "pt", translation) {
  
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl
  
  tsCShift = listP[[2]] 
  logScale =  listP[[3]]
  labelSubset = listP[[4]]
  scale_y = listP[[5]]
  
  
  
  
  covidColorPlot = ggplot(data=tsCShift[tsCShift$Country.Region %in% countryList,]) 
  
  if(translation[['color']][[lg]] %in% mark.ctrl) {  
    
    covidColorPlot =  covidColorPlot + aes(x=Date, y=selVarValue, colour = Country.Region, shape = Country.Region, 
                                           label = Country.Region) 
  } else {
    covidColorPlot =  covidColorPlot + aes(x=Date, y=selVarValue, shape = Country.Region, 
                                           label = Country.Region) 
  }
  
  if(translation[['line_style']][[lg]] %in% mark.ctrl) {  
    covidColorPlot =  covidColorPlot + aes(linetype = Country.Region) 
  } 
  if(translation[['line']][[lg]] %in% mark.ctrl) {
    covidColorPlot = covidColorPlot  + geom_line(size = 1) 
  }
  
  
  covidColorPlot =  covidColorPlot +coord_cartesian(xlim=date_range,  ylim = c(1,ylimSup))  
  covidColorPlot =  covidColorPlot + annotate("text", label = "@robertodepinho", color= "grey50",
                                              x = max(tsCAgg$Date, na.rm=T), y = 1, vjust=0, hjust=1.1)
  
  
  
  if(translation[['background_lines']][[lg]] %in% mark.ctrl) {
    x = tsCShift[tsCShift$Country.Region %in%  backgroundList & !(tsCShift$Country.Region %in%  countryList),]
    covidColorPlot = covidColorPlot  + geom_line(size = 1, data = x , 
                                                 aes(x=Date, y=selVarValue), colour = "grey90", 
                                                 linetype = "solid",
                                                 show.legend = FALSE ) 
  } 
  
  if(est.ctrl !=translation[['none']][[lg]]) {
    b = b = 2^ (1/doublingTime)
    minX = as.numeric(min(tsCAgg$Date, na.rm=T))
    if(logScale){
      fun.l <- function(x) {log( b^(as.numeric(x)-minX), 10)}
    } else {
      fun.l <- function(x) { b^(as.numeric(x)-minX)}
    }
    covidColorPlot = covidColorPlot  + stat_function(fun = fun.l, colour = "green")
  }
  
  if(high.ctrl !=translation[['none']][[lg]]) {
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
  covidColorPlot = covidColorPlot  + xlab("")
  covidColorPlot = covidColorPlot  +  theme_jf() 
  
  covidColorPlot = covidColorPlot  + theme(legend.position =  "top", legend.title =  element_blank()) 
  
  if(translation[['marker']][[lg]] %in% mark.ctrl) {
    covidColorPlot = covidColorPlot + geom_point(size = 5, data = tsCShift[tsCShift$Country.Region %in%  high.ctrl,], shape = 1,
                                                 aes(x=Date, y=selVarValue, colour = Country.Region),
                                                 show.legend = FALSE) +
      geom_point(size = 3, show.legend = !(translation[['background_lines']][[lg]] %in% mark.ctrl)) 
  }
  
  
  
  covidColorPlot = covidColorPlot  + scale_x_date(name="",
                                                  date_breaks = "1 week", date_labels = "%d %b", limits = date_range)
  
  return(covidColorPlot)
  
}

