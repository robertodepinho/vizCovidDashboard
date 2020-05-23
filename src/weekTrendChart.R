



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
  m
  dfw$linear = dfw$ValueAvg[days] + (dfw$d-median(dfw$d))*m$coefficients[2]
  mean(dfw$linear)  
  
  m =lm(log(ValueAvg) ~ d, dfw)
  m
  
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
  
  exps = sum(exp( 1:(days-1) *  m$coefficients[2])) 
    
  x1 = log( days * dfw$ValueAvg[days] /  (exps + 1) ) - m$coefficients[2] 
  
  exp(x1)
  b =  exp(m$coefficients[2]) 
  t = log(2, base = 2) /  log(b, base = 2)
  
  dfw$expo = exp(x1) * exp(dfw$d*m$coefficients[2])
  mean(dfw$expo)
  
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

