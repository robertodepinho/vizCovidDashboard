#adaptado a partir de código de 
#https://github.com/covid19br/covid19br.github.io


## Bibliotecas necessarias
library(zoo)
## Funcoes
source("./est/funcoes.R")

getDoublingTime <- function(highlightCountry, tsCAgg, selVar) {
  
  
  selVar = gsub(" ", "", selVar)
  
  #serie de entrada
  # c("data", "novos.casos", "casos.acumulados", "obitos.novos", "obitos.acumulados")
  # data: "2020-01-30"
  #brasil.raw <- read.csv("./est/BrasilCov19.csv", as.is = TRUE)
  serie.raw = tsCAgg[tsCAgg$Country.Region %in% highlightCountry, c("Date",selVar, selVar)]
  
  ## Cria objeto da classe zoo 
  serie <- zoo(serie.raw[, 2:3], serie.raw$Date) 
  
  ## Tira os casos acumulados iniciais abaixo de um mínimo
  minimo <- 15 ## pelo menos 15 casos
  serie.d0 <- diazero(serie[,selVar], limite = minimo)
  
  ## Executa o ajuste em running windows com a largura indicada
  ## Retorna o tempo de duplicacao a cada data final de cada running window
  tempos.duplicacao <- dt.rw(serie.d0, window.width = 5)
  
  ## Conveniencia: reordena e renomeia as colunas do objeto resultante
  tempos.duplicacao <- tempos.duplicacao[,c(1,3,2)]
  names(tempos.duplicacao) <- c("estimativa", "ic.inf", "ic.sup")
  
  return(tempos.duplicacao)
  
}


estSerieApp <- function(b, maxDays, name) {
  x = data.frame( 
                 Date= as.Date(1, origin = "2020-01-01"):as.Date(maxDays, origin = "2020-01-01"),
                 Country.Region = name,
                 Confirmed = b^((1:maxDays)-1), Deaths =  b^((1:maxDays)-1), 
                 Recovered =  b^((1:maxDays)-1), Active = b^((1:maxDays)-1), 
                 NewCases =  b^((1:maxDays)-1), NewDeaths = b^((1:maxDays)-1), 
                 cnt.Code = name)
  x$Date = as.Date(x$Date, origin = "2020-01-01")
  x$Group = "EST"
  return(x)
}


# exp.5d <- forecast.exponential(serie[,2],
#                                start = length(time(serie))-4,
#                                days.forecast = 5)
