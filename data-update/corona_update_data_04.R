# Roberto de Pinho 2020 CC-BY-SA  

library(reshape2)
library(curl)
library(dplyr)
library(countrycode)



downloadJHU <- function() {
  
  #source
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  #Live file with download
  destFile = paste("./csv_how/time_series_19-covid-Confirmed_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "time_series_19-covid-Confirmed_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  #Live file with download
  destFile = paste("./csv_how/time_series_19-covid-Deaths_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "time_series_19-covid-Deaths_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  #Live file with download
  destFile = paste("./csv_how/time_series_19-covid-Recovered_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "time_series_19-covid-Recovered_last.csv", overwrite = TRUE)
  
  
}

downloadJHU.US <- function() {
  
  #source
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  #Live file with download
  destFile = paste("./csv_how/time_series_19-covid-Confirmed_US_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "time_series_19-covid-Confirmed_US_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  #Live file with download
  destFile = paste("./csv_how/time_series_19-covid-Deaths_US_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "time_series_19-covid-Deaths_US_last.csv", overwrite = TRUE)
  
  # sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_US.csv"
  # #Live file with download
  # destFile = paste("./csv_how/time_series_19-covid-Recovered_US_", timeStamp, ".csv", sep = "")
  # download.file(sourceURL, destfile = destFile, method = "curl")
  # file.copy(destFile, "time_series_19-covid-Recovered_US_last.csv", overwrite = TRUE)
  # 
  
}



downloadMSCSV <- function(fileName){
  

  UFData = read.delim(fileName, header = T, sep = ";", fileEncoding="latin1")
  UFData$Date =as.Date(as.character(UFData$data), format = "%Y-%m-%d") #as.Date(as.character(UFData$data), format = "%d/%m/%Y") #as.Date(UFData$data, origin = as.Date("2020-01-30")-43860) #as.Date(as.character(UFData$data), format = "%d/%m/%Y")
  UFData$Country.Region = UFData$estado
  UFData$Confirmed = UFData$casosAcumulados
  UFData$Deaths = UFData$obitosAcumulados
  UFData$Recovered = NA
  UFData$Active = NA
  UFData$Group =  "BRA.UF"
  
  tail(UFData[UFData$Country.Region %in% "BA" ,])
  ufCAgg = UFData[, c("Date", "Country.Region", "Confirmed", "Deaths", "Recovered", 
                      "Active", "Group")]
  ufCAgg$Country.Region = paste("BRA:", ufCAgg$Country.Region, sep = "")
  tsCAgg = rbind(tsCAgg, ufCAgg)
  
  
  x = aggregate(cbind(Confirmed, Deaths) ~ Date , tsCAgg[ (substr(tsCAgg$Country.Region, 1, 4)) == "BRA:" ,],sum, na.rm=T)
  x$Country.Region = "BRA:Brasil"
  x$Recovered = NA
  x$Active = NA
  x$Group = "BRA.UF"
  x = x[, colnames(tsCAgg)]
  tsCAgg = rbind(tsCAgg, x)
  
  
  return(tsCAgg)
  
}


downloadBrasil.io <- function(){
  
  sourceURL = "https://brasil.io/dataset/covid19/caso?format=csv"
  
  df = read.csv(sourceURL)
  UFData = df[ df$place_type %in% "state",]
  UFData$Date = as.Date(as.character(UFData$date)) 
  UFData$Country.Region = UFData$state
  UFData$Confirmed = UFData$confirmed
  UFData$Deaths = UFData$deaths
  UFData$Recovered = NA
  UFData$Active = NA
  UFData$Group =  "IO.UF"
  
  UFData = UFData[ order(UFData$state, UFData$date),]
  
  #UFData[UFData$Country.Region %in% "BA" ,]
  ufCAgg = UFData[, c("Date", "Country.Region", "Confirmed", "Deaths", "Recovered", 
                      "Active", "Group")]
  ufCAgg$Country.Region = paste("IO.BRA:", ufCAgg$Country.Region, sep = "")
  tsCAgg = rbind(tsCAgg, ufCAgg)
  
  
  UFData = df[ df$place_type %in% "city",]
  UFData$Date = as.Date(as.character(UFData$date)) 
  UFData$Country.Region = paste(UFData$state, UFData$city, sep=":")
  UFData$Confirmed = UFData$confirmed
  UFData$Deaths = UFData$deaths
  UFData$Recovered = NA
  UFData$Active = NA
  UFData$Group =  "IO.CT"
  
  UFData = UFData[ order(UFData$state, UFData$date),]
  
  #UFData[UFData$Country.Region %in% "BA" ,]
  ufCAgg = UFData[, c("Date", "Country.Region", "Confirmed", "Deaths", "Recovered", 
                      "Active", "Group")]
  ufCAgg$Country.Region = paste("CT-", ufCAgg$Country.Region, sep = "")
  tsCAgg = rbind(tsCAgg, ufCAgg)
  
  
  return(tsCAgg)
  
}


prepareData <- function() {
  
  tsConfirmed = read.csv("time_series_19-covid-Confirmed_last.csv")
  tsC = reshape2::melt(tsConfirmed, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Confirmed"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAgg = aggregate( Confirmed ~ Date + Country.Region, tsC, sum, na.rm = T)
  
  tsDeaths = read.csv("time_series_19-covid-Deaths_last.csv")
  tsC = reshape2::melt(tsDeaths, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Deaths"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAggD = aggregate( Deaths ~ Date + Country.Region, tsC, sum, na.rm = T)
  
  tsC = read.csv("time_series_19-covid-Recovered_last.csv")
  tsC = reshape2::melt(tsC, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Recovered"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAggR = aggregate( Recovered ~ Date + Country.Region, tsC, sum, na.rm = T)
  
  tsCAgg = merge(x = tsCAgg, y = tsCAggD, by = c("Date", "Country.Region"), all = TRUE)
  tsCAgg = merge(x = tsCAgg, y = tsCAggR, by = c("Date", "Country.Region"), all = TRUE)
  
  tsCAgg$Active = tsCAgg$Confirmed - tsCAgg$Deaths - tsCAgg$Recovered
  tsCAgg$Group =  "JHU.C"
  
  
  
  
  return(tsCAgg)
  
}


prepareData.US <- function(tsCAggPar) {
  
  tsConfirmed = read.csv("time_series_19-covid-Confirmed_US_last.csv")
  tsC = reshape2::melt(tsConfirmed, id.vars=
                         c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
                           "Country_Region", "Lat", "Long_", "Combined_Key"))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Confirmed"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAgg = aggregate( Confirmed ~ Date + Province_State, tsC, sum, na.rm = T)
  
  tsDeaths = read.csv("time_series_19-covid-Deaths_US_last.csv")
  tsC = reshape2::melt(tsDeaths, id.vars= 
                         c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
                                            "Country_Region", "Lat", "Long_", "Combined_Key"))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Deaths"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAggD = aggregate( Deaths ~ Date + Province_State, tsC, sum, na.rm = T)
  
  # tsC = read.csv("time_series_19-covid-Recovered_US_last.csv")
  # tsC = reshape2::melt(tsC, id.vars=
  #                        c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
  #                          "Country_Region", "Lat", "Long_", "Combined_Key"))
  # colnames(tsC)[colnames(tsC) %in% "value" ] = "Recovered"
  # tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  # tsCAggR = aggregate( Recovered ~ Date + Province_State, tsC, sum, na.rm = T)
  # 
  
  tsCAgg = merge(x = tsCAgg, y = tsCAggD, by = c("Date", "Province_State"), all = TRUE)
  #tsCAgg = merge(x = tsCAgg, y = tsCAggR, by = c("Date", "Province_State"), all = TRUE)
  
  tsCAgg$Recovered = NA
  tsCAgg$Active = NA #tsCAgg$Confirmed - tsCAgg$Deaths - tsCAgg$Recovered
  tsCAgg$Group =  "JHU.US"
  
  colnames(tsCAgg)[colnames(tsCAgg) == "Province_State" ] = "Country.Region"
  
  
  
  tsCAgg = rbind(tsCAggPar, tsCAgg)
  
  return(tsCAgg)
  
}


prepareDataJHU.Regions <- function(tsCAgg) {
  
  tsConfirmed = read.csv("time_series_19-covid-Confirmed_last.csv")
  ts = reshape2::melt(tsConfirmed, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(ts)[colnames(ts) %in% "value" ] = "Confirmed"
  ts$Date = as.Date(as.character(ts$variable), format = "X%m.%d.%y")
  ts$cnt = countrycode(sourcevar = ts$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  ts$Country.Region = paste(ts$cnt, ts$Province.State, sep=":")
  ts = ts[ ts$Province.State != "",]
  tsC = ts
  
  
  tsDeaths = read.csv("time_series_19-covid-Deaths_last.csv")
  ts = reshape2::melt(tsDeaths, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(ts)[colnames(ts) %in% "value" ] = "Deaths"
  ts$Date = as.Date(as.character(ts$variable), format = "X%m.%d.%y")
  ts$cnt = countrycode(sourcevar = ts$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  ts$Country.Region = paste(ts$cnt, ts$Province.State, sep=":")
  ts = ts[ ts$Province.State != "",]
  tsD = ts
  
  
  tsRecovered = read.csv("time_series_19-covid-Recovered_last.csv")
  ts = reshape2::melt(tsRecovered, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(ts)[colnames(ts) %in% "value" ] = "Recovered"
  ts$Date = as.Date(as.character(ts$variable), format = "X%m.%d.%y")
  ts$cnt = countrycode(sourcevar = ts$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  ts$Country.Region = paste(ts$cnt, ts$Province.State, sep=":")
  ts = ts[ ts$Province.State != "",]
  tsR = ts
  
  tsM = merge(x = tsC, y = tsD, by = c("Date", "Country.Region"), all = TRUE)
  tsM = merge(x = tsM, y = tsR, by = c("Date", "Country.Region"), all = TRUE)
  
  tsM = tsM[ tsM$Province.State != "",]
  
  tsM$Active = tsM$Confirmed - tsM$Deaths - tsM$Recovered
  tsM$Group =  "JHU.R"
  
  tsMAgg = tsM[, c("Date", "Country.Region", "Confirmed", "Deaths", "Recovered", 
                      "Active", "Group")]
  
  tsCAgg = rbind(tsCAgg, tsMAgg)
  
  return(tsCAgg)
  
}



estSerie <- function(b, maxDays, name) {
  x = data.frame(Country.Region = name, 
                 Date= as.Date(1, origin = "2020-01-01"):as.Date(maxDays, origin = "2020-01-01"),
                 Confirmed = b^((1:maxDays)-1), Deaths =  b^((1:maxDays)-1), 
                 Recovered =  b^((1:maxDays)-1), Active = b^((1:maxDays)-1))
  x$Date = as.Date(x$Date, origin = "2020-01-01")
  x$Group = "EST"
  return(x)
}

estSeries <- function() {
  
  countDays = aggregate(Date~Country.Region, tsCAgg, length )
  maxDays = max(countDays$Date) + 1
  
  # b = 2
  # x = estSerie(b, 30, "EST:Double Every Day")
  # tsCAgg = rbind(tsCAgg, x)
  # b = sqrt(2)
  # x = estSerie(b, maxDays, "EST:Double Every 2 Days")
  # tsCAgg = rbind(tsCAgg, x)
  b = 2^ (1/3)
  x = estSerie(b, maxDays, "EST:Double Every 3 Days")
  tsCAgg = rbind(tsCAgg, x)
  b = 2^ (1/4)
  x = estSerie(b, maxDays, "EST:Double Every 4 Days")
  tsCAgg = rbind(tsCAgg, x)
  b = 2^ (1/7)
  x = estSerie(b, maxDays+100, "EST:Double Every Week")
  tsCAgg = rbind(tsCAgg, x)
  b = 1.33
  x = estSerie(b, maxDays, "EST:33% (Double ~2.5 Days)")
  tsCAgg = rbind(tsCAgg, x)
  
  return(tsCAgg)
  
}

newCasesDeaths <- function() {
  
  
  
  #NewCases
  tsCAgg = tsCAgg[ order(tsCAgg$Date, tsCAgg$Country.Region),]
  tsCAgg <- tsCAgg %>%
    group_by(Country.Region) %>%
    mutate(NewCases = Confirmed - lag(Confirmed))
  #NewDeaths
  tsCAgg <- tsCAgg %>%
    group_by(Country.Region) %>%
    mutate(NewDeaths = Deaths - lag(Deaths))
  return(data.frame(tsCAgg))
}

###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
###############################################################################################
# wishlist
# reset selection
# "ideal seria ter os controles em um overlay semitransparente e colapsável"
# pt_BR
# cidades filtro por UF
# US Counties, Cities
# use selectizeInput?
# R0
# OK Actual Date
# OK US States 


timeStamp = format(Sys.time(),"%Y%m%d_%H%M%S")
downloadJHU()
downloadJHU.US()

tsCAgg = prepareData()
tsCAgg[tsCAgg$Country.Region %in% "Brazil", ]

#last Day
x = data.frame(Date = as.Date("2020-04-17"),
               Country.Region = "Brazil", 
               Confirmed = 33682, #Boletim MS
               Deaths = 2141,
               Recovered = NA,
               Active = NA, Group = "JHU.C") #x$Confirmed - x$Deaths - x$Recovered
tsCAgg = rbind(tsCAgg, x)
#tsCAgg[tsCAgg$Country.Region %in% "Brazil", ]

tsCAgg = prepareDataJHU.Regions(tsCAgg)
#tsCAgg[tsCAgg$Country.Region %in% "CN:Hubei", ]

tsCAgg = prepareData.US(tsCAgg)


fileName = "~/Downloads/9968e11f13dd30c9831e3b1e8da3eb74_Download_COVID19_20200417.csv"
tsCAgg = downloadMSCSV(fileName)
tsCAgg[tsCAgg$Country.Region %in% "BRA:SP", ]


tsCAgg = downloadBrasil.io()


#tsCAgg = estSeries()
tsCAgg = newCasesDeaths()

 tail(tsCAgg[tsCAgg$Country.Region %in% "CT-BA:Salvador", ])
 tail(tsCAgg[tsCAgg$Country.Region %in% "AU:New South Wales", ])
 tail(tsCAgg[tsCAgg$Country.Region %in% "New York", ])
 tail(tsCAgg[tsCAgg$Country.Region %in% "BRA:Brasil", ])
 tail(tsCAgg[tsCAgg$Country.Region %in% "Brazil", ])

save(tsCAgg, timeStamp, file= "../tsCAgg.RData")
  
