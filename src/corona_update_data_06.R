# Roberto de Pinho 2020 CC-BY-SA  


source("src/updateFunc.R")

#get_last_date_brasilIo()

timeStamp = format(Sys.time(),"%Y%m%d_%H%M%S")
downloadJHU()
downloadJHU.US()
downloadBrasil.io()





tsCAgg = prepareData()
tsCAgg = prepareDataJHU.Regions(tsCAgg)
tsCAgg = prepareData.US(tsCAgg)
tsCAgg = preparaBrasil.io(tsCAgg)
#tsCAgg = prepara_delta_ufs(tsCAgg)

#tsCAgg = rbind(tsCAgg, delta_regions(tsCAgg, country_region_a = "BRA:SP", "CT-SP:São Paulo"))

tsCAgg = newCasesDeaths(tsCAgg)




tail(tsCAgg[tsCAgg$Country.Region %in% "IO.BRA:SP", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "BRA:BA", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "CT-BA:Salvador", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "BRA:BA_sem_cap", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "BRA:Brasil", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "BRA:Brasil_sem_cap", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "Brazil", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "AU:New South Wales", ])
tail(tsCAgg[tsCAgg$Country.Region %in% "New York", ])


save(tsCAgg, timeStamp, file= "tsCAgg.RData")


load("./../../twt.Rdata.RData")
library(RCurl)
ftpUpload(what = "tsCAgg.RData",to = paste(sftURL,"tsCAgg.RData", sep=""))
ftpUpload(what = "tsCAgg.RData",to = paste(sftURL,"../dashboardEN/tsCAgg.RData", sep=""))
writeLines(timeStamp, "restart.txt")
ftpUpload(what = "restart.txt",to = paste(sftURL,"restart.txt", sep=""))  
ftpUpload(what = "restart.txt",to = paste(sftURL,"../dashboardEN/restart.txt", sep=""))  


# ftpUpload(what = "app.R",to = paste(sftURL,"app.R", sep=""))
# ftpUpload(what = "src/chart.R",to = paste(sftURL,"src/chart.R", sep=""))
# ftpUpload(what = "app.R",to = paste(sftURL,"../dashboardEN/app.R", sep=""))
# ftpUpload(what = "src/chart.R",to = paste(sftURL,"../dashboardEN/src/chart.R", sep=""))


########################3
# https://t.co/RNZkEFJDT5?amp=1
#   https://xx9p7hp1p7.execute-api.us-east-1.amazonaws.com/prod/PortalGeralApi
# fileName = paste("~/Downloads/HIST_PAINEL_COVIDBR_", format(Sys.Date(),"%d%b%Y"), ".xlsx", sep = "") #"%Y%m%d"
# fileName.csv = paste("HIST_PAINEL_COVIDBR_", format(Sys.time(),"%d%b%Y"), ".csv", sep = "")
# if(!file.exists(fileName)) {
#   fileName = paste("~/Downloads/HIST_PAINEL_COVIDBR_", format(Sys.Date()-1,"%d%b%Y"), ".xlsx", sep = "") #"%Y%m%d"
#   fileName.csv = paste("HIST_PAINEL_COVIDBR_", format(Sys.Date()-1,"%d%b%Y"), ".csv", sep = "")
# } 
# 
# 
# Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/libreoffice/program/")
# command = paste("libreoffice --headless --convert-to csv ",
#                 fileName, sep = " ")
# system(command, wait = TRUE)


###############################################################################################
# wishlist
# reset selection
# "ideal seria ter os controles em um overlay semitransparente e colapsável"
# pt_BR
# cidades filtro por UF
# US Counties, Cities
# use selectizeInput?
# R0
# check medians
# OK Actual Date
# OK US States 
