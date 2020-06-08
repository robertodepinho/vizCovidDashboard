library(reshape2)
library(curl)
library(dplyr)
library(countrycode)



downloadJHU <- function() {
  
  #source
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  #Live file with download
  destFile = paste("upd/csv_how/time_series_19-covid-Confirmed_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "upd/time_series_19-covid-Confirmed_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
  #Live file with download
  destFile = paste("upd/csv_how/time_series_19-covid-Deaths_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "upd/time_series_19-covid-Deaths_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  #Live file with download
  destFile = paste("upd/csv_how/time_series_19-covid-Recovered_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "upd/time_series_19-covid-Recovered_last.csv", overwrite = TRUE)
  
  
}

downloadJHU.US <- function() {
  
  #source
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
  #Live file with download
  destFile = paste("upd/csv_how/time_series_19-covid-Confirmed_US_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "upd/time_series_19-covid-Confirmed_US_last.csv", overwrite = TRUE)
  
  sourceURL = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
  #Live file with download
  destFile = paste("upd/csv_how/time_series_19-covid-Deaths_US_", timeStamp, ".csv", sep = "")
  download.file(sourceURL, destfile = destFile, method = "curl")
  file.copy(destFile, "upd/time_series_19-covid-Deaths_US_last.csv", overwrite = TRUE)

  
}

downloadBrasil.io <- function() {
  
  #source
  #sourceURL =  "https://brasil.io/dataset/covid19/caso?format=csv"
  sourceURL = "https://data.brasil.io/dataset/covid19/caso_full.csv.gz"
  
  #Live file with download
  destFile = "upd/csv_how/Brasil.ioCaso_last.csv.gz"
  download.file(sourceURL, destfile = destFile, method = "libcurl")
  
}


get_last_date_brasilIo <- function(per = 80) {
  library(httr); library(jsonlite)
  source_url = "https://brasil.io/api/dataset/covid19/caso/data"  #"?is_last=True&place_type=state"
  
  request <- GET(source_url, query = list(
    is_last  = "True",
    place_type = "state"))
  response <- content(request, as = "text", encoding = "UTF-8")
  response_list = fromJSON(response, flatten = TRUE) 
  results = response_list[["results"]]
  
  pop_by_date = aggregate(estimated_population_2019 ~date, results, length)
  pop_by_date = pop_by_date[ order(pop_by_date$date, decreasing = T),]
  print(pop_by_date)
  
  
  pop_tot = sum(results$estimated_population_2019)
  pop_by_date = aggregate(estimated_population_2019 ~date, results, sum)
  pop_by_date = pop_by_date[ order(pop_by_date$date, decreasing = T),]
  pop_by_date$coverage = cumsum(pop_by_date$estimated_population_2019) / pop_tot *100
  print(pop_by_date)
  
  
  return(max(pop_by_date$date[pop_by_date$coverage > per]) )

}

preparaBrasil.io <- function(tsCAgg){
  
  zz=gzfile("upd/csv_how/Brasil.ioCaso_last.csv.gz",'rt')  
  df = read.csv(zz)
  close(zz)
  UFData = df[ df$place_type %in% "state",]
  UFData$Date = as.Date(as.character(UFData$date)) 
  UFData$Country.Region = UFData$state
  UFData$Confirmed = UFData$last_available_confirmed
  UFData$Deaths = UFData$last_available_deaths
  UFData$Recovered = NA
  UFData$Active = NA
  UFData$Group =  "BRA.UF" # "IO.UF"   
  
  UFData = UFData[ order(UFData$state, UFData$date),]
  
  #UFData[UFData$Country.Region %in% "BA" ,]
  ufCAgg = UFData[, c("Date", "Country.Region", "Confirmed", "Deaths", "Recovered", 
                      "Active", "Group")]
  ufCAgg$Country.Region = paste("BRA:", ufCAgg$Country.Region, sep = "")  # "IO.BRA:"
  tsCAgg = rbind(tsCAgg, ufCAgg)
  
  
  UFData = df[ df$place_type %in% "city",]
  UFData$Date = as.Date(as.character(UFData$date)) 
  UFData$Country.Region = paste(UFData$state, UFData$city, sep=":")
  UFData$Confirmed = UFData$last_available_confirmed
  UFData$Deaths = UFData$last_available_deaths
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
  
  tsConfirmed = read.csv("upd/time_series_19-covid-Confirmed_last.csv")
  tsC = reshape2::melt(tsConfirmed, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Confirmed"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAgg = aggregate( Confirmed ~ Date + Country.Region, tsC, sum, na.rm = T)
  
  tsDeaths = read.csv("upd/time_series_19-covid-Deaths_last.csv")
  tsC = reshape2::melt(tsDeaths, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Deaths"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAggD = aggregate( Deaths ~ Date + Country.Region, tsC, sum, na.rm = T)
  
  tsC = read.csv("upd/time_series_19-covid-Recovered_last.csv")
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
  
  tsConfirmed = read.csv("upd/time_series_19-covid-Confirmed_US_last.csv")
  tsC = reshape2::melt(tsConfirmed, id.vars=
                         c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", 
                           "Country_Region", "Lat", "Long_", "Combined_Key"))
  colnames(tsC)[colnames(tsC) %in% "value" ] = "Confirmed"
  tsC$Date = as.Date(as.character(tsC$variable), format = "X%m.%d.%y")
  tsCAgg = aggregate( Confirmed ~ Date + Province_State, tsC, sum, na.rm = T)
  
  tsDeaths = read.csv("upd/time_series_19-covid-Deaths_US_last.csv")
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
  
  tsConfirmed = read.csv("upd/time_series_19-covid-Confirmed_last.csv")
  ts = reshape2::melt(tsConfirmed, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(ts)[colnames(ts) %in% "value" ] = "Confirmed"
  ts$Date = as.Date(as.character(ts$variable), format = "X%m.%d.%y")
  ts$cnt = countrycode(sourcevar = ts$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  ts$Country.Region = paste(ts$cnt, ts$Province.State, sep=":")
  ts = ts[ ts$Province.State != "",]
  tsC = ts
  
  
  tsDeaths = read.csv("upd/time_series_19-covid-Deaths_last.csv")
  ts = reshape2::melt(tsDeaths, id.vars=c("Province.State", "Country.Region", "Lat" ,"Long"  ))
  colnames(ts)[colnames(ts) %in% "value" ] = "Deaths"
  ts$Date = as.Date(as.character(ts$variable), format = "X%m.%d.%y")
  ts$cnt = countrycode(sourcevar = ts$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  ts$Country.Region = paste(ts$cnt, ts$Province.State, sep=":")
  ts = ts[ ts$Province.State != "",]
  tsD = ts
  
  
  tsRecovered = read.csv("upd/time_series_19-covid-Recovered_last.csv")
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
  
  
  library(dplyr)
  library(zoo)
  tsCAgg <-arrange(tsCAgg,Country.Region,Date) %>%
    group_by(Country.Region) %>%
    mutate(NewCasesAvg=rollapply(NewCases,7,mean,na.rm= TRUE, align='right',fill=NA))
  
  tsCAgg <-arrange(tsCAgg,Country.Region,Date) %>%
    group_by(Country.Region) %>%
    mutate(NewDeathsAvg=rollapply(NewDeaths,7,mean,na.rm= TRUE, align='right',fill=NA))
  
  tsCAgg = as.data.frame(tsCAgg)
  
  tsCAgg$cnt.Code = countrycode(sourcevar = tsCAgg$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  tsCAgg$cnt.Code[tsCAgg$cnt.Code == " " ]=  tsCAgg$Country.Region[tsCAgg$cnt.Code == " " ]
  tsCAgg$cnt.Code = factor(tsCAgg$cnt.Code)
  
  
  
  return(data.frame(tsCAgg))
}

prepareBraBrasil <- function(tsCAgg) {
  
  df = subset(tsCAgg, Group == "BRA.UF")
  #opção agregado estados
  x = aggregate(cbind(Confirmed, Deaths) ~ Date , df,sum, na.rm=T)
  x$Country.Region = "BRA:Brasil"
  x$Recovered = NA
  x$Active = NA
  x$Group = "BRA.UF"   # "IO.UF"
  x = x[, colnames(tsCAgg)]
  tsCAgg = rbind(tsCAgg, x)
  
  #opção JHU
  #x = subset(tsCAgg, Group ==  "JHU.C" & Country.Region =="Brazil")
  #x$Country.Region = "BRA:Brasil"
  #x$Group = "BRA.UF"   # "IO.UF"
  #x = x[, colnames(tsCAgg)]
  #tsCAgg = rbind(tsCAgg, x)
  
  return(tsCAgg)
  
  
}

preparaMSCSV <- function(tsCAgg, fileName){
  
  # library(XLConnect)
  # UFData = readWorksheetFromFile(fileName, sheet = 1)
  # library(xlsx)
  # UFData = read.xlsx(fileName, 1)
  # UFData = read.delim(fileName, header = T, sep = ";", fileEncoding="latin1")
  # 
  
  UFData = read.csv(fileName)
  UFData = subset(UFData, is.na(codmun))
  UFData = subset(UFData, estado != "")
  
  
  UFData$Date =as.Date(as.character(UFData$data), format = , tryFormats = c("%Y-%m-%d","%d/%m/%Y"))
  #as.Date(as.character(UFData$data), format = "%Y-%m-%d")
  #as.Date(as.character(UFData$data), format = "%d/%m/%Y") 
  #as.Date(UFData$data, origin = as.Date("2020-01-30")-43860) 
  #as.Date(as.character(UFData$data), format = "%d/%m/%Y")
  UFData$Country.Region = UFData$estado
  UFData$Confirmed = UFData$casosAcumulado
  UFData$Deaths = UFData$obitosAcumulado
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

