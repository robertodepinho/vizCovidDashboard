source("src/theme_black.R")


covid_center <- function(){
  source("src/theme_black.R")
  uf = "SP"
  
  source("src/updateFunc.R")
  #downloadBrasil.io()
  zz=gzfile("upd/csv_how/Brasil.ioCaso_last.csv.gz",'rt')  
  df = read.csv(zz)
  close(zz)
  
  uf_ibge = subset(df, state %in% uf & place_type =="state")$city_ibge_code[1]
  df = subset(df, state %in% uf & place_type =="city" & !(is.na(city_ibge_code)))
  df$value = df$new_confirmed
  
  df = df[,c("city", "city_ibge_code", "date","value")]
  
  df = df[ order( df$city_ibge_code, df$date),]
  df$value_avg = NA
  city_bef = -1
  for(i in 1:nrow(df)){
    if(city_bef != df$city_ibge_code[i]){
      city_bef = df$city_ibge_code[i]
      count_city = 0
    }
    df$value_avg[i] = mean(df$value[(i-count_city):i],na.rm=T)
    count_city = count_city + 1
    count_city = ifelse(count_city>6,6, count_city)
  }
  
  
  url <- paste('https://servicodados.ibge.gov.br/api/v2/malhas/',
               uf_ibge,
               '?resolucao=5&formato=application/vnd.geo+json&qualidade=4',
               sep="")
  geojson <- rjson::fromJSON(file=url)
  centroides = data.frame()
  for(i in 1:length(geojson[["features"]])){
    
    centroides = rbind(centroides,with(geojson[["features"]][[i]][["properties"]],
                                       data.frame(city_ibge_code = codarea, lon = centroide[1], lat = centroide[2])))
    
  }
  
  
  df = merge(df[,c("city_ibge_code", "date","value", "value_avg" )], centroides, by = "city_ibge_code")
  df$date = as.Date(as.character(df$date), origin="1970-01-01")
  df = df[order(df$date),]
  
  df$per = with(df, ave(value_avg, date, FUN = function(x) x/sum(x)))
  df$lon_pond = df$lon * df$per
  df$lat_pond = df$lat * df$per
  
  df_date = aggregate( cbind(lon_pond, lat_pond, value_avg) ~date, df, sum, na.rm=T )
  df_date = df_date[order(df_date$date),]
  
  #UF map
  
  
  
  
  url <- paste('https://servicodados.ibge.gov.br/api/v2/malhas/',
               uf_ibge,
               '?resolucao=2&formato=application/vnd.geo+json&qualidade=4',
               sep="")
  
  #geojson_uf <- rjson::fromJSON(file=url)
  
  
  
  library(ggplot2)
  library(rgdal)
  library(plyr)
  library(rgeos)
  library(mapproj)
  library(gganimate)
  
  theme_rgp = theme_jf() %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position =  "bottom", legend.title =  element_blank() )
  
  #shapefiles  
  #ftp://geoftp.ibge.gov.br/organizacao_do_territorio/malhas_territoriais/malhas_municipais/municipio_2015/UFs/
  shapefile <- readOGR(paste("gis/", uf,"/", sep=""), paste(uf_ibge,"UFE250GC_SIR", sep=""))
  shapefile_df <- fortify(shapefile)
  
  df_date$long = df_date$lon_pond
  df_date$lat = df_date$lat_pond
  margin = 1.3
  
  
  
  map <- ggplot() +
    #map border 1
    geom_path(data = shapefile_df, aes(x = long, y = lat, group = group), color = 'red', size = .2) +
    
    #city points 2
    geom_point(data = df, aes(x = lon, y = lat, size = value_avg), colour = "red", alpha = 0.1) +
    scale_size(range = c(.1, 24), limits = c(0,20000)) +
    
    #centroid 3
    geom_point(data = df_date, aes(x = long, y = lat, color = date, size =value_avg), alpha  = 0.75) + 
    
    
    annotate("text", label = "@robertodepinho", color= "grey50",
             x = min(shapefile_df$long), y = min(shapefile_df$lat), vjust=0, hjust=0) +
    
    transition_time(date) +
    #transition_states(states = date) +
    labs(title = "{frame_time}") +  
    #shadow_wake(wake_length = 0.1, alpha = TRUE) +
    shadow_mark(alpha = .50, size = 1, exclude_layer = c(1,2)) + 
    scale_color_date() +coord_map() + theme_rgp 
  
  print(map, nframe = 10, fps = 1)
  anim_save(paste("upd/gif_how/",uf,".gif",sep=""),map,nframes = 300, fps = 6) 
  
  map <- map + scale_x_continuous(limits = c(min(df_date$long)-margin, max(df_date$long)+margin))
  map <- map + scale_y_continuous(limits = c(min(df_date$lat)-margin, max(df_date$lat)+margin))
  print(map, nframe = 10, fps = 1)
  
  anim_save(paste("upd/gif_how/z",uf,".gif",sep=""),map,nframes = 300, fps = 6) 
  
  
  ################################################3
  # library(plotly)
  # library(rjson)
  # 
  # g <- list(
  #   fitbounds = "locations",
  #   visible = FALSE
  # )
  # 
  # fig <- plot_ly()
  # 
  # 
  # 
  # fig <- fig %>% add_trace(
  #   type="choropleth",
  #   geojson=geojson_uf,
  #   #locations=df$codarea,
  #   #hoverinfo = "text",
  #   #text = paste(df$UF,":", df$Tamanho.da.linha.da.costa..km., " km", sep = ""),
  #   z=1,
  #   colorscale="Inferno",
  #   featureidkey="properties.codarea"
  # )
  # fig <- fig %>% layout(
  #   geo = g
  # )
  # #fig <- fig %>% colorbar(title = "Linha da costa (km)")
  # fig <- fig %>% layout(
  #   title = "Brasil"
  # )
  # fig
  # 
  
}


days_to <- function(tsCAgg, cr = "BRA:Brasil", mult = 100*10^3){ 
  
  df = subset(tsCAgg, Country.Region %in% cr)
  max_val = max(df$Confirmed, na.rm=T)
  df = df[order(df$Date),]
  res = data.frame()
  c_mult = mult
  while(c_mult < max_val){
    df_mul = subset(df,Confirmed > c_mult)
    res = rbind(res, data.frame(Date = min(df_mul$Date, na.rm=T), c_mult))
    c_mult = c_mult + mult
  }
  res$delta = c(0,diff(as.numeric(res$Date)))
  print(res, row.names = F)
  
  
}





rel_grid_plot <- function(tsCAgg, countryList = c("Brazil", "Spain", "Italy", "France", "United Kingdom"),
                          varSel = "NewCases",  
                          varSelAvg = "NewCasesAvg", days = 14) {
  
  
  
  tsCAgg = tsCAgg[order(tsCAgg$Date, tsCAgg$Country.Region),]
  dfwl = tail(tsCAgg[tsCAgg$Country.Region %in% countryList, 
                     c("Date", "Country.Region", "NewCases","NewCasesAvg", "NewDeaths","NewDeathsAvg", "Deaths", "Confirmed")], 
              days * length(countryList))
  
  dfcr = data.frame()
  for(cr in countryList){
    dfw = subset(dfwl,Country.Region %in% cr)
    dfw = dfw[order(dfw$Date),]
    dfw$Value = dfw[, varSel]
    dfw$ValueAvg = dfw[, varSelAvg]
    
    dfw$d = as.numeric(dfw$Date - dfw$Date[1] + 1)
    m = lm(ValueAvg ~d, dfw)
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
    
    #Value, lwr, upr, expo, liner, ValueAvg  , lwr, upr, expo, liner, ValueAvg
    
    dfw = within(dfw,
                 { 
                   ref_value = ValueAvg[1]
                   
                   Value =  Value / ref_value * 100  
                   ValueAvg =  ValueAvg / ref_value * 100  
                   lwr =  lwr/ ref_value * 100  
                   upr =  upr/ ref_value * 100  
                   expo =  expo/ ref_value * 100  
                   linear =  linear/ ref_value * 100
                 }
                 
    )
    dfw$end_value = NA
    dfw$end_value[nrow(dfw)] =round(dfw$ValueAvg[nrow(dfw)],0)
    
    #print(dfw[, c("Date", "Country.Region", "d", "Value", "ValueAvg", "linear", "expo")], row.names = F, )
    dfcr = rbind(dfcr,dfw)
    
  }
  
  
  
  library(ggplot2)
  library(scales)
  library(ggrepel)
  
  
  theme_rgp = theme_jf() %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          legend.position =  "bottom", legend.title =  element_blank() )
  
  
  
  # paste("Modelos com base em dados até:", format(max(dfcr$Date[!is.na(dfcr$Value)], na.rm=T), "%d-%b"),
  #       ", Projetados até:", format(max(dfcr$Date[!is.na(dfcr$Value)], na.rm=T)+ahead, "%d-%b"),
  #       " (linhas), \nDados atualizados até :", format(max(newData$Date, na.rm=T), "%d-%b"), 
  #       " (pontos)",
  covidPlot = ggplot(data=dfcr) 
  covidPlot = covidPlot + ggtitle(label = "", subtitle = paste("índice: valor da média móvel em ", 
                                                               format(min(dfcr$Date[!is.na(dfcr$Value)], na.rm=T), "%d-%b"),
                                                               " = 100",
                                                               sep=""))
  covidPlot = covidPlot + aes(x=Date, y=Value) + facet_wrap(~Country.Region) 
  covidPlot = covidPlot  + geom_point(size = 0.5, aes(x=Date, y=Value), alpha = 0.1) 
  
  covidPlot = covidPlot  + geom_ribbon(aes(ymin = lwr, ymax = upr), 
                                       alpha=0.25, linetype = "dashed", colour = "gray90")
  
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=ValueAvg, linetype = "média movel"), 
                                     show.legend = TRUE, colour = "gray50" ) 
  covidPlot = covidPlot  + geom_line(size = 1, data = dfcr , 
                                     aes(x=Date, y=expo, linetype = "expo"), 
                                     show.legend = TRUE , color = "darkblue") 
  
  covidPlot = covidPlot  + geom_text_repel(aes(x=Date, y=ValueAvg, label = end_value))
  
  covidPlot = covidPlot  + scale_linetype_manual(values = c("solid", "dotted", "dashed"),
                                                 breaks = c("média movel", "expo", "linear")  )
  
  
  covidPlot = covidPlot  + scale_y_continuous(name=paste(varSel, " (", format(min(dfcr$Date[!is.na(dfcr$Value)], 
                                                                                  na.rm=T), "%d-%b"), 
                                                         " = 100)" ,sep=""),
                                              labels =  label_comma(), 
                                              limits = c(min(dfcr$lwr, na.rm = T), max(dfcr$upr, na.rm = T))) 
  
  covidPlot = covidPlot  + xlab("Data") 
  covidPlot = covidPlot  +  theme_rgp
  covidPlot = covidPlot  + scale_x_date(date_breaks = "1 week" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
  covidPlot
  return(covidPlot)
  
}





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
  covidPlot = covidPlot  +  theme_jf()
  covidPlot = covidPlot  + scale_linetype_manual(values = c("solid", "dashed", "dotted"),
                                                 breaks = c("movel", "expo", "linear")  )
  covidPlot = covidPlot  + scale_y_continuous(name=varSel, labels =  label_comma()) + xlab("Date")
  covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
  covidPlot = covidPlot  + scale_x_date(date_breaks = "1 day" , date_labels = "%d-%b", 
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
  covidPlot = covidPlot  + scale_x_date(date_breaks = "3 days" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
  #covidPlot
  return(covidPlot)
  
}

##################################################################################
week_by_week_trend <- function(tsCAgg, CountryRegion = "BRA:Brasil",
                               varSel = "NewCases", 
                               varSelAvg = "NewCasesAvg", 
                               days = 7, y_label = "Novos Casos (média móvel)") {
  
  library(lubridate)
  tsCAgg = tsCAgg[order(tsCAgg$Date),]
  
  dfw = tsCAgg[tsCAgg$Country.Region %in% CountryRegion, c("Date", "Country.Region", "NewCases","NewCasesAvg", "NewDeaths","NewDeathsAvg")]
  dfw = subset(dfw, !is.na(NewCasesAvg) & Date > "2020-04-01")
  
  dfw$day_num = as.numeric(dfw$Date) - as.numeric(dfw$Date[nrow(dfw)]) -1
  dfw$period_num = abs(dfw$day_num %/% days)
  dfw$period_day = abs(dfw$day_num %% days)
  
  dfw = subset(dfw, period_num < max(period_num))
  
  
  dfw = dfw[order(dfw$Date),]
  dfw$d = as.numeric(dfw$Date - dfw$Date[1] + 1)
  dfw$Value = dfw[, varSel]
  dfw$ValueAvg = dfw[, varSelAvg]
  
  dfw_over = dfw
  x = subset(dfw,period_day ==0)
  x$period_num = x$period_num +1
  dfw_over = rbind(dfw_over,x)
  x = subset(dfw,period_day ==(days-1))
  x$period_num = x$period_num -1
  dfw_over = rbind(dfw_over,x)
  dfw_over = dfw_over[order(dfw_over$Date),]
  
  
  calc <- function(x) {
    model = lm( log(ValueAvg)~Date, x)
    coef = (exp(model$coefficients[2])-1)*100
    expo = exp(predict(model))
    data.frame(Date = x$Date, expo = expo, coef = coef)
  }
  
  
  
  library(dplyr)
  df_expo = dfw_over %>% 
    group_by(period_num) %>%
    do(data.frame(calc(.)))
  df_expo = df_expo[order(df_expo$Date),]
  df_expo$label = ifelse(wday(df_expo$Date)==1,paste(round(df_expo$coef,1),"%"),"")
  
  #############################33
  
  covidPlot <- function() {
    library(ggplot2)
    library(scales)
    library(ggrepel)
    require(lubridate)
    source("src/theme_black.R")
    
    covidPlot = ggplot(data=dfw) 
    covidPlot = covidPlot + aes(x=Date, y=Value) 
    covidPlot = covidPlot  + geom_point(size = 1, colour = "grey40") 
    covidPlot = covidPlot  + geom_line(size = 1, data = dfw , 
                                       aes(x=Date, y=ValueAvg), colour = "black",
                                       linetype = "solid",
                                       show.legend = TRUE ) 
    covidPlot = covidPlot  + geom_line(size = 1, data = df_expo , 
                                       aes(x=Date, y=expo, colour = coef, group = period_num), 
                                       linetype = "solid",
                                       show.legend = TRUE ) 
    covidPlot = covidPlot  +  theme_jf()
    
    covidPlot = covidPlot  + scale_y_continuous(name=y_label, labels =  label_comma()) + xlab("Data")
    
    
    
    covidPlot = covidPlot  + scale_colour_gradient2(low = "green", 
                                                    mid = "orange",
                                                    high = "red",
                                                    midpoint = 0) 
    
    covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
    covidPlot = covidPlot  + ggtitle(CountryRegion)
    covidPlot = covidPlot  + scale_x_date(date_breaks = "2 weeks" , date_labels = "%d-%b", 
                                          date_minor_breaks = "1 day") 
    covidPlot = covidPlot  + annotate("text", label = "@robertodepinho", color= "grey50",
               x = max(dfw$Date), y = 0, vjust=0.1, hjust=1.1)
    
    covidPlot = covidPlot  + geom_label_repel(data = df_expo , 
                                             aes(x=Date, y=expo, 
                                                 label = label), nudge_y = max(dfw$Value)/7) 
    
    covidPlot
  }
  covidPlot()
  
    ##########################
  
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
  l =  m$coefficients[2]
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
  covidPlot = covidPlot  +  theme_jf()
  covidPlot = covidPlot  + scale_y_continuous(name=varSel, labels =  label_comma()) + xlab("Data")
  covidPlot = covidPlot  + scale_color_manual(name = "Tendência", values = c("red", "green", "black"),
                                              labels = c("Exponencial", "Linear", "Média móvel"))
  covidPlot = covidPlot  + theme(legend.position =  "bottom", legend.title =  element_blank()) 
  covidPlot = covidPlot  + ggtitle(CountryRegion)
  covidPlot = covidPlot  + scale_x_date(date_breaks = "2 days" , date_labels = "%d-%b", 
                                        date_minor_breaks = "1 day") 
  #covidPlot
  return(list(covidPlot, b, t,l))
  
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

