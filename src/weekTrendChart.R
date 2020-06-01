deathTrendCheck <- function(tsCAgg, newData, countryList = c("Brazil", "Spain", "Italy", "France", "United Kingdom"),
                       varSel = "Deaths", 
                       varSelAvg = varSel, days = 15, ahead = 15) {
  
  
  
  tsCAgg = tsCAgg[order(tsCAgg$Date, tsCAgg$Country.Region),]
  dfwl = tail(tsCAgg[tsCAgg$Country.Region %in% countryList, 
                     c("Date", "Country.Region", "NewCases","NewCasesAvg", "NewDeaths","NewDeathsAvg", "Deaths", "Confirmed")], 
              days * length(countryList))
  
  newData = subset(newData, Country.Region %in% countryList)  
  newData = subset(newData, Date >= min(dfwl$Date, na.rm=T))  
  newData$Value = newData[, varSel]
  
  dfcr = data.frame()
  for(cr in countryList){
    dfw = subset(dfwl,Country.Region %in% cr)
    dfw = dfw[order(dfw$Date),]
    dfw$Value = dfw[, varSel]
    dfw$ValueAvg = dfw[, varSelAvg]
    
    dfw$d = as.numeric(dfw$Date - dfw$Date[1] + 1)
    m = lm(ValueAvg ~d, dfw)
    for(i in 1:ahead){
      proj = dfw[(i+days-1),]
      proj$Date = proj$Date+1; proj$d = proj$d +1
      proj[,3:10] = NA
      dfw = rbind(dfw, proj)
    }
    #dfw$linear = dfw$ValueAvg[days] + (dfw$d-median(1:days))*m$coefficients[2]
    dfw$linear = predict(m, dfw)
    
    m =lm(log(ValueAvg) ~ d, dfw[1:days,])
    exps = sum(exp( 1:(days-1) *  m$coefficients[2])) 
    x1 = log( days * dfw$ValueAvg[days] /  (exps + 1) ) - m$coefficients[2] 
    
    #dfw$expo = exp(x1) * exp(dfw$d*m$coefficients[2])  #adjust do last day moving average
    pred = exp(predict(m, dfw, interval="confidence",level = 0.99))
    dfw$expo = pred[,"fit"]
    dfw$lwr = pred[,"lwr"]
    dfw$upr = pred[,"upr"]
    
    #print(dfw[, c("Date", "Country.Region", "d", "Value", "ValueAvg", "linear", "expo")], row.names = F, )
    dfcr = rbind(dfcr,dfw)
    
  }
  
  
  
  library(ggplot2)
  library(scales)
  covidPlot = ggplot(data=dfcr) 
  
  covidPlot = covidPlot + ggtitle(label = "", subtitle = 
                                    paste("Projeções com dados até:", format(max(dfcr$Date[!is.na(dfcr$Value)], na.rm=T), "%d-%b"),
                                          " (linhas), \nDados atualizados até :", format(max(newData$Date, na.rm=T), "%d-%b"), 
                                          " (pontos)",
                                          sep = ""))
                                    
  covidPlot = covidPlot + aes(x=Date, y=Value, color = Country.Region) 
  covidPlot = covidPlot  + geom_point(data = newData, size = 3, aes(x=Date, y=Value, color = Country.Region)) 
  
  covidPlot = covidPlot  + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Country.Region, color = Country.Region), 
                                      alpha=0.25, linetype = "dashed")
  
  
  # covidPlot = covidPlot  + geom_line(aes(y = lwr, group = Country.Region), 
  #                                    colour = "gray50", linetype = "dashed")
  # 
  # covidPlot = covidPlot  + geom_line(aes(y = upr, group = Country.Region), 
  #                                    colour = "gray50", linetype = "dashed")
  # 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=expo, linetype = "expo"), 
                                     
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=linear, linetype = "linear"), 
                                     show.legend = TRUE ) 
  #covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
  #                                   aes(x=Date, y=ValueAvg, linetype = "movel"), 
  #                                   show.legend = TRUE ) 
  covidPlot = covidPlot  +  theme_bw()
  covidPlot = covidPlot  + scale_linetype_manual(values = c("solid", "dashed", "dotted"),
                                                 breaks = c("movel", "expo", "linear")  )
  covidPlot = covidPlot  + scale_y_continuous(name=varSel, labels =  label_comma()) + xlab("Date")
  covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
  covidPlot = covidPlot  + scale_x_date(date_breaks = "1 week" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
    #covidPlot
  return(covidPlot)
  
}



deathTrend <- function(tsCAgg, countryList = c("Brazil", "Spain", "Italy", "France", "United Kingdom"),
                       varSel = "Deaths", 
                       varSelAvg = "Deaths", days = 15, ahead = 15) {
  
  
  tsCAgg = tsCAgg[order(tsCAgg$Date, tsCAgg$Country.Region),]
  dfwl = tail(tsCAgg[tsCAgg$Country.Region %in% countryList, 
                     c("Date", "Country.Region", "NewCases","NewCasesAvg", "NewDeaths","NewDeathsAvg", "Deaths")], 
              days * length(countryList))
  
  
  dfcr = data.frame()
  for(cr in countryList){
    dfw = subset(dfwl,Country.Region %in% cr)
    dfw = dfw[order(dfw$Date),]
    dfw$d = as.numeric(dfw$Date - dfw$Date[1] + 1)
    dfw$Value = dfw[, varSel]
    dfw$ValueAvg = dfw[, varSelAvg]
    m = lm(ValueAvg ~d, dfw)
    for(i in 1:ahead){
      proj = dfw[(i+days-1),]
      proj$Date = proj$Date+1; proj$d = proj$d +1
      proj[,3:7] = NA; proj[,9:10] = NA
      dfw = rbind(dfw, proj)
    }
    #dfw$linear = dfw$ValueAvg[days] + (dfw$d-median(1:days))*m$coefficients[2]
    dfw$linear = predict(m, dfw)
    
    m =lm(log(ValueAvg) ~ d, dfw[1:days,])
    exps = sum(exp( 1:(days-1) *  m$coefficients[2])) 
    x1 = log( days * dfw$ValueAvg[days] /  (exps + 1) ) - m$coefficients[2] 
    
    #dfw$expo = exp(x1) * exp(dfw$d*m$coefficients[2])  #adjust do last day moving average
    dfw$expo = exp(predict(m, dfw))

    #print(dfw[, c("Date", "Country.Region", "d", "Value", "ValueAvg", "linear", "expo")], row.names = F, )
    dfcr = rbind(dfcr,dfw)
    
  }
  
  
  library(ggplot2)
  library(scales)
  covidPlot = ggplot(data=dfcr) 
  covidPlot = covidPlot + aes(x=Date, y=Value, color = Country.Region) 
  covidPlot = covidPlot  + geom_point(size = 1) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=expo, linetype = "expo"), 
                                     
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=linear, linetype = "linear"), 
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=ValueAvg, linetype = "movel"), 
                                     
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  +  theme_bw()
  covidPlot = covidPlot  + scale_y_continuous(name=varSel, labels =  label_comma()) + xlab("Date")
  covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
  covidPlot = covidPlot  + scale_x_date(date_breaks = "1 week" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
  #covidPlot
  return(covidPlot)
  
}



weekTrendChart <- function(tsCAgg, CountryRegion = "BRA:Brasil",
                           varSel = "NewCases", 
                           varSelAvg = "NewCasesAvg", days = 7) {
  
  
  tsCAgg = tsCAgg[order(tsCAgg$Date),]
  dfw = tail(tsCAgg[tsCAgg$Country.Region %in% CountryRegion, c("Date", "Country.Region", "NewCases","NewCasesAvg", "NewDeaths","NewDeathsAvg")], days)
  
  
  
  #varSel = "NewDeaths"
  #varSelAvg = "NewDeathsAvg"
  
  
  dfw = dfw[order(dfw$Date),]
  dfw$d = as.numeric(dfw$Date - dfw$Date[1] + 1)
  dfw$Value = dfw[, varSel]
  dfw$ValueAvg = dfw[, varSelAvg]
  
  
  m = lm(ValueAvg ~d, dfw)
  print(m)
  #dfw$linear = dfw$ValueAvg[days] + (dfw$d-median(dfw$d))*m$coefficients[2]
  dfw$linear = dfw$ValueAvg[days] + (dfw$d-days+3)*m$coefficients[2]
  
  mean(dfw$linear[(days-6):days])  
  mean(dfw$linear)  
  
  m =lm(log(ValueAvg) ~ d, dfw)
  print(m)
  
  # x1 = log( 7 * dfw$ValueAvg[7] /  
  #             (
  #                 exp( 1 *  m$coefficients[2])+ 
  #                 exp( 2 *  m$coefficients[2]) + 
  #                 exp( 3 *  m$coefficients[2]) + 
  #                 exp( 4 *  m$coefficients[2]) + 
  #                 exp( 5 *  m$coefficients[2]) + 
  #                 exp( 6  *  m$coefficients[2]) + 
  #                 1
  #             ) ) - m$coefficients[2] 
  # 
  #Ajuste para que a curva de tendência tenha média dos seus últimos 7 dias equivalente à média móvel (7 dias) do último dia
  #exps = sum(exp( 1:(days-1) *  m$coefficients[2])) 
  exps = sum(exp( 1:(7-1) *  m$coefficients[2]))
  
  #x1 = log( days * dfw$ValueAvg[days] /  (exps + 1) ) - m$coefficients[2] 
  x1 = log( 7 * dfw$ValueAvg[days] /  (exps + 1) ) - m$coefficients[2] 
  
  
  exp(x1)
  b =  exp(m$coefficients[2]) 
  t = log(2, base = 2) /  log(b, base = 2)
  print(b)
  
  #dfw$expo = exp(x1) * exp((dfw$d)*m$coefficients[2])
  dfw$expo = exp(x1) * exp((dfw$d-(days-7))*m$coefficients[2])
  
  mean(dfw$expo)
  mean(dfw$expo[(days-6):days])  
  
  
  print(varSel)
  print(dfw[, c("Date", "Country.Region", "d", "Value", "ValueAvg", "linear", "expo")], row.names = F, )
  
  library(ggplot2)
  library(scales)
  covidPlot = ggplot(data=dfw) 
  covidPlot = covidPlot + aes(x=Date, y=Value) 
  covidPlot = covidPlot  + geom_point(size = 1) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfw , 
                                     aes(x=Date, y=expo, colour = "expo"), 
                                     linetype = "solid",
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfw , 
                                     aes(x=Date, y=linear, colour = "linear"), 
                                     linetype = "solid",
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfw , 
                                     aes(x=Date, y=ValueAvg, colour = "movel"), 
                                     linetype = "solid",
                                     show.legend = TRUE ) 
  covidPlot = covidPlot  +  theme_bw()
  covidPlot = covidPlot  + scale_y_continuous(name=varSel, labels =  label_comma()) + xlab("Data")
  covidPlot = covidPlot  + scale_color_manual(name = "Tendência", values = c("red", "green", "black"),
                                              labels = c("Exponencial", "Linear", "Média móvel"))
  covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
  covidPlot = covidPlot  + ggtitle(CountryRegion)
  covidPlot = covidPlot  + scale_x_date(date_breaks = "2 days" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
  #covidPlot
  return(covidPlot)
  
}

#plot(dfw$Date,dfw$Value)
#lines(dfw$Date,dfw$expo, col = "red")
#lines(dfw$Date,dfw$linear, col = "green")
#lines(dfw$Date,dfw$ValueAvg)

#####################################################
# log(dfw$NewCasesAvg[7]) = log( ( exp(x1 + 1 *  m$coefficients[2]) +
#                                exp(x1 + 2 *  m$coefficients[2]) +
#                                exp(x1 + 3 *  m$coefficients[2]) +
#                                exp(x1 + 4 *  m$coefficients[2]) +
#                                exp(x1 + 5 *  m$coefficients[2]) +
#                                exp(x1 + 6 *  m$coefficients[2]) +
#                                exp(x1 + 7 *  m$coefficients[2]))/7 )
#
# exp(y + b) + exp(y + 2 b) + exp(y + 3 b) + exp(y + 4 b) + exp(y + 5 b) + exp(y + 6 b) + exp(y + 7 b)
# (e^b + e^(2 b) + e^(3 b) + e^(4 b) + e^(5 b) + e^(6 b) + 1) e^(b + y)≈8.63592 e^y

