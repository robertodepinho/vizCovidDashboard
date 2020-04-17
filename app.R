#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Roberto de Pinho 2020 CC-BY-SA  

library(ggplot2)
library(ggrepel)
library(reshape2)
library(scales)
library(countrycode)
library(shiny)


source("./est/estima.R")
source("./src/chart.R")

# logifySlider javascript function
JS.logify <-
  "
// function to logify a sliderInput
function logifySlider (sliderId, sci = false) {
  if (sci) {
    // scientific style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return ('10<sup>'+num+'</sup>'); }
    })
  } else {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num)); }
    })
  }
}"

# call logifySlider for each relevant sliderInput
JS.onload <-
  "
// execute upon document loading
$(document).ready(function() {
  // wait a few ms to allow other scripts to execute
  setTimeout(function() {
    // include call for each slider
    logifySlider('log_slider', sci = false)
    
  }, 5)})
"

getCountryChoices <- function(tsCAgg) {
  countryChoices = aggregate( Confirmed ~  Country.Region +  Group, tsCAgg, max, na.rm=T)
  countryChoices = countryChoices[ order(as.character(countryChoices$Country.Region), decreasing = F),]
  countryChoices$cnt.Code = countrycode(sourcevar = countryChoices$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")
  countryChoices$cnt.Code[countryChoices$cnt.Code == " " ]=  countryChoices$Country.Region[countryChoices$cnt.Code == " " ]  
  return(countryChoices)
}

chartDataPrepare <- function(selVar,tsCAgg,anchorCases, days, cases.y, logscale.ctrl, countryList) {
  xlimBot = days[1]
  xlimSup = days[2]
  ylimSup = cases.y
  logScale = logscale.ctrl    
  
  selVarCol = which(colnames(tsCAgg) %in% gsub(" ", "", selVar))
  tsCAgg$selVarValue = tsCAgg[,selVarCol]
  
  qtdRows = sum( (tsCAgg$selVarValue >= anchorCases), na.rm = T )
  if(qtdRows < 1) {return(list(FALSE))}
  
  #for each country, 1st day with at least anchorNumber
  anchorDate = aggregate(Date ~ Country.Region, tsCAgg[tsCAgg$selVarValue >= anchorCases,], min)
  
  qtdRows = sum( (tsCAgg$Country.Region %in% countryList) & (tsCAgg$selVarValue >= anchorCases) & (tsCAgg$Country.Region %in% anchorDate$Country.Region), na.rm=T)
  if(qtdRows < 1) {return(list(FALSE))}
  
  colnames(anchorDate)[colnames(anchorDate) %in% "Date" ] = "anchorDate"
  #remove low cases countries
  tsCAgg = tsCAgg[ tsCAgg$Country.Region %in% anchorDate$Country.Region,]
  
  tsCShift = merge(tsCAgg, anchorDate, by = "Country.Region", all.x = T)
  
  
  tsCShift$diffDate = tsCShift$Date - tsCShift$anchorDate
  maxDays = max(as.numeric(tsCShift$diffDate), na.rm=T) + 1
  
  
  if(logScale){
    scale_y =   scale_y_log10( labels =  label_dollar(prefix=""), name = paste(selVar, " (log)", sep=" "))
    listaRows = which(!is.na(tsCShift$selVarValue) &  tsCShift$selVarValue == 0)
    if(length(listaRows)>0) {
      tsCShift$selVarValue[listaRows] = tsCShift$selVarValue[listaRows] + 1
    }
  } else {
    scale_y = scale_y_continuous( labels =  label_dollar(prefix=""), name = paste(selVar, " ", sep=" "))
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



########################################## SETUP ###########################################
load("tsCAgg.RData")
tsCAgg$cnt.Code = countrycode(sourcevar = tsCAgg$Country.Region, origin = "country.name", destination = "iso2c", nomatch = " ")


countryList = c("Brazil", "Italy", "Japan", "Korea, South", "France")


countryChoices = getCountryChoices(tsCAgg)
###############################################################################################



######################################## UI ###################################################

# Define UI for application that draws a histogram
ui <- fluidPage( theme = "united.min.css",
                 tags$head(tags$script(HTML(JS.logify))),
                 tags$head(tags$script(HTML(JS.onload))),
                 # Application title
                 titlePanel("vizCovid Dashboard:  Confirmed Cases, Deaths, Recovered"),
                 tags$head(includeHTML(("google-analytics.html"))),
                 tags$a(href="https://twitter.com/robertodepinho", "@robertodepinho"),
                 tags$a(href="#about", "about"),
                 
                 
                 plotOutput("covidPlot", height = "600px"),
                 hr(),
                 
                 
                 fluidRow(
                   column(3,
                          selectInput("var_ctrl", "Variable",
                                      choices = c("Confirmed", "Deaths", "Recovered", "Active", "New Cases", "New Deaths"),
                                      selected = "Confirmed"),
                          radioButtons("date_ctrl", "Use actual date?",
                                       choices= c("Yes", "No"),
                                       selected= "No"),
                          conditionalPanel(
                            condition = "input.date_ctrl == 'No'",
                            sliderInput("anchor",
                                        "Number of occurences to set day 0 at:",
                                        min = 0,
                                        max = 2000,
                                        value = 100),
                          sliderInput("days",
                                      "Days:",
                                      min = -15,
                                      max = 365,
                                      value = c(-1,45))),
                          checkboxInput("logscale", "Log (Value)", value = TRUE),
                          conditionalPanel(
                            condition = "input.logscale",
                            sliderInput("log_slider", "Value(log):",
                                        min = 0, max = 7, value = 5, step = 1)
                          ),
                          conditionalPanel(
                            condition = "!input.logscale",
                            sliderInput("cases.y",
                                        "Value:",
                                        min = 1,
                                        max = 200*10^3,
                                        value = 70*10^3),
                            
                            sliderInput("cases.y.fine",
                                        "Fine Tune:",
                                        min = 69*10^3,
                                        max = 71*10^3,
                                        value = 70*10^3))
                   ),
                   column(3, offset = 0,
                          
                          radioButtons("style", "Chart Style",
                                       choices = list("Single Chart (Colors)" = 1, "One Chart per Country (Blue)" = 2),selected = 1),
                          selectInput("est.ctrl", "Trend Line - Doubling Time (days)",
                                      choices = c("None","EST:Doubling Time (median)", "3", "4", "7"),
                                      selected = "None"
                          ),
                          selectInput("high.ctrl", "Highlight Country/Region",
                                      choices = c("None",countryList),
                                      selected = "Brazil"
                          ),
                          htmlOutput("doublingTime"),
                          br(),
                          checkboxGroupInput("mark.ctrl", 
                                             "Marker/Line Options", choices = c("Marker", "Line", "Line Style" , "Color", "Background Lines"),selected = c("Line", "Color"),
                                             inline = TRUE)
                          
                          
                   ),
                   column(6,
                          
                          
                          checkboxGroupInput("CR.ctrl", label = h3("Selected Country/Regions"), inline = TRUE,
                                             choices = countryList,
                                             selected = countryList),
                          hr(),
                          h5("Select Countries or Regions below to add them above:"),
                          selectInput("countries.ctrl.sel", label = h3("Countries"), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.C"])), 
                                      selected = " ", multiple = TRUE),
                          
                          selectInput("brauf.ctrl.sel", label = h3("Brazil: States"), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "BRA.UF"])), 
                                      selected = " ", multiple = TRUE),
                          
                          selectInput("bract.ctrl.sel", label = h3("Brazil: Cities"), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "IO.CT"])), 
                                      selected = " ", multiple = TRUE),
                          selectInput("regions.ctrl.sel", label = h3("World Provinces/States"), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.R"])), 
                                      selected = " ", multiple = TRUE),
                          selectInput("us50.ctrl.sel", label = h3("US: States"), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.US"])), 
                                      selected = " ", multiple = TRUE),
                          
                          
                          
                   )), 
                 fluidRow( 
                   hr(),
                   
                   h4("Chart developed by:"),
                   tags$a(id = "about"),
                   tags$a(href="https://twitter.com/robertodepinho", "@robertodepinho"),
                   br(),
                   p("Last updated:", timeStamp),
                   br(),
                   h4("This visualization is being generously hosted by:"),
                   tags$a(href="http://nbcgib.uesc.br/nbcgib/",
                          "NBCGIB/CCAM/PPGMC/UESC"),
                   p("Núcleo de Biologia Computacional e Gestão de Informações Biotecnológicas,"),
                   p("Centro de Computação Avançada e Modelagem, "),
                   p("Programa de Pós-Graduação em Modelagem Computacional em Ciência e Tecnologia "),
                   h5("Universidade Estadual de Santa Cruz "),
                   h5("Bahia, Brazil."),
                   tags$a(href='http://nbcgib.uesc.br/ppgmc/',
                          tags$img(src='http://nbcgib.uesc.br/ppgmc/imagens/logotopo.png',height='80')),
                   br(),
                   h4("Data Sources"),
                   h5("Countries:"),
                   tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", 
                          "CSSE COVID-19 Dataset - Johns Hopkins University"),
                   h5("Brazilian States data:"),
                   tags$a(href="https://covid.saude.gov.br/", 
                          "Ministério da Saúde (Brasil)"),
                   h5("Brazilian Cities data:"),
                   tags$a(href="https://brasil.io/", 
                          "Brasil.io"),
                   br(),
                   tags$a(id = "double"),
                   h4("Doubling time estimate model:"),
                   tags$a(href="https://github.com/covid19br/covid19br.github.io", 
                          "Observatório COVID-19 BR"),
                   p("Showing median of last 15 days and median of their confidence intervals (level=0.95)."),
                   h4("Source:"),
                   tags$a(href="https://github.com/robertodepinho/vizCovidDashboard", 
                          "github"),
                   
                   
                 )
)


######################################## UI ###################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  #load("tsCAgg.RData")
  
  
  
  output$covidPlot <- renderPlot({
    
    toListen <- reactive({
      list(input$countries.ctrl.sel , input$brauf.ctrl.sel , input$bract.ctrl.sel, input$regions.ctrl.sel, input$us50.ctrl.sel)
    })
    
    observeEvent(toListen(), {
      changeGroup = sort(unique(c(input$countries.ctrl.sel, input$brauf.ctrl.sel, input$bract.ctrl.sel, input$regions.ctrl.sel, input$us50.ctrl.sel)))
      TTTGroup = sort(unique(c(input$countries.ctrl.sel, input$brauf.ctrl.sel, input$bract.ctrl.sel, input$regions.ctrl.sel, input$us50.ctrl.sel, input$CR.ctrl)))
      TTTGroup = TTTGroup[TTTGroup != " "]
      
      if(!identical(session$userData$antGroup, changeGroup)) {
        session$userData$antGroup = changeGroup
        updateCheckboxGroupInput(session, "CR.ctrl", choices = TTTGroup, selected = TTTGroup, inline = TRUE)
        if(input$high.ctrl %in% TTTGroup) { highSel = input$high.ctrl} else { highSel = "None"}
        updateSelectInput(session, "high.ctrl", choices = c("None", TTTGroup), selected = highSel)      
      }
      return()
    } )
    
    
    
    
    observeEvent(input$cases.y,  {
      delta = 10^3
      newValue = input$cases.y.fine
      if(newValue>(input$cases.y+delta) | newValue<(input$cases.y-delta)) { 
        
        newValue = input$cases.y 
        
        updateSliderInput(session = session, inputId = "cases.y.fine",
                          value = newValue,
                          max = input$cases.y + delta,
                          min = max(0,input$cases.y - delta))
      } else {
        updateSliderInput(session = session, inputId = "cases.y.fine",
                          
                          max = input$cases.y + delta,
                          min = max(0,input$cases.y - delta))
      }
    })
    
    
    if(input$logscale){
      casesValue = 10^input$log_slider
    } else {
      casesValue = input$cases.y.fine
      
    }
    
    doublingTime = 7
    if(input$high.ctrl != "None" & input$est.ctrl != "EST:Doubling Time (median)") {
      doublingTime = as.numeric(input$est.ctrl)
    } 
    
    if(input$high.ctrl != "None"  & input$est.ctrl == "EST:Doubling Time (median)") {
      gDT = tail(getDoublingTime(input$high.ctrl, tsCAgg, input$var_ctrl),15)
      doublingTime = median(gDT$estimativa, na.rm=T)
      
      output$doublingTime <- renderText(
        paste("Median Doubling time: ", 
              format(round(as.numeric(doublingTime), 1)), " days",
              " (", format(round(as.numeric(median(gDT$ic.inf)), 1)), ",",
              format(round(as.numeric(median(gDT$ic.sup)), 1)), ")",
              
              tags$a(href="#double", "*") ,sep=""))
      
    } else  {output$doublingTime <- renderText("")}
    
    
    
    countryList = unique(c(input$CR.ctrl))
    
    anchorCases = input$anchor
    
    if(input$date_ctrl == "Yes") {
      anchorCases = 0
    }
    
    #prepare data
    listP = chartDataPrepare(input$var_ctrl,tsCAgg, anchorCases, input$days, casesValue, input$logscale, countryList)
    
    if(!listP[[1]]) {
      g = ggplot(data.frame(x =c(0,100), y = c(0,100)), aes(x= x, y= y)) + 
        geom_point() +
        annotate("text", label = "No data points to show, please change parameters.", x = 50, y = 50)
      g
      return(g)
    }
    
    #adjust max
    tsCShift = listP[[2]]
    
    
    
    
    
    nonEst = (substr(tsCShift$Country.Region, 1, 4) == "EST:")
    
    maxCases = max(tsCShift$selVarValue[!nonEst & tsCShift$Country.Region %in% countryList], na.rm=T) + 100
    maxCases = max(maxCases, input$cases.y, na.rm=T)
    updateSliderInput(session, inputId = "cases.y", max = maxCases)
    
    maxDays = listP[[6]] 
    maxDays = max(maxDays, input$days, na.rm=T)
    
    updateSliderInput(session, inputId = "days", max = maxDays)
    
    if(input$style == 1) {
      
      if(anchorCases!=0){
        covidColor(input$var_ctrl,tsCAgg,listP, anchorCases, 
                   input$days, casesValue, input$logscale, countryList, input$mark.ctrl, input$high.ctrl, doublingTime, input$est.ctrl)
      } else {
        covidColorDate(input$var_ctrl,tsCAgg,listP, anchorCases, 
                       input$days, casesValue, input$logscale, countryList, input$mark.ctrl, input$high.ctrl, doublingTime, input$est.ctrl)
        
        
      }
      
    } else {
      if(anchorCases!=0){
        covidBlue(input$var_ctrl,tsCAgg,listP, anchorCases, 
                  input$days, casesValue, input$logscale, countryList)
      } else {
        covidBlueDate(input$var_ctrl,tsCAgg,listP, anchorCases, 
                      input$days, casesValue, input$logscale, countryList)
      }
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
