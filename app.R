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
  countryChoices = aggregate( Confirmed ~  Country.Region +  Group + cnt.Code, tsCAgg, max, na.rm=T)
  countryChoices = countryChoices[ order(as.character(countryChoices$Country.Region), decreasing = F),]
  return(countryChoices)
}



########################################## SETUP ###########################################

lg = readLines("lg.txt")[1]

load("tsCAgg.RData")
load("upd/translation.bin")
dateLimits = c(min(tsCAgg$Date, na.rm=T)-1, max(tsCAgg$Date, na.rm=T)+1)

backgroundList = c("BRA:Brasil", "Italy", "Japan", "Korea, South", "France", "US", "China", "Mexico", "Portugal")
countryList = c("BRA:Brasil", "BRA:SP", "BRA:RJ", "BRA:AM", "BRA:BA")

choices_style = list(1,2)
names(choices_style) = c(translation[["single_chart"]][[lg]] , translation[["grid_charts"]][[lg]])

choices_var = c("Confirmed",
                "Deaths",
                "Recovered",
                "Active",
                "NewCases",
                "NewDeaths",
                "NewCasesAvg",
                "NewDeathsAvg")
names(choices_var) = c(translation[["Confirmed"]][[lg]], 
                       translation[["Deaths"]][[lg]], 
                       translation[["Recovered"]][[lg]], 
                       translation[["Active"]][[lg]], 
                       translation[["NewCases"]][[lg]], 
                       translation[["NewDeaths"]][[lg]], 
                       translation[["NewCasesAvg"]][[lg]], 
                       translation[["NewDeathsAvg"]][[lg]])

countryChoices = getCountryChoices(tsCAgg)
###############################################################################################



######################################## UI ###################################################

# Define UI for application that draws a histogram
ui <- fluidPage( theme = "sandstone.min.css",
                 tags$head(tags$script(HTML(JS.logify))),
                 tags$head(tags$script(HTML(JS.onload))),
                 # Application title
                 titlePanel(translation[['title']][[lg]] ),
                 tags$head(includeHTML(("google-analytics.html"))),
                 tags$a(href="https://twitter.com/robertodepinho", "@robertodepinho"),
                 tags$a(href="#about", translation[['about']][[lg]] ),
                 
                 
                 plotOutput("covidPlot", height = "600px"),
                 hr(),
                 
                 
                 fluidRow(
                   column(3,
                          selectInput("var_ctrl", translation[["Variable"]][[lg]],
                                      choices = choices_var,
                                      selected = "NewCasesAvg"),
                          radioButtons("date_ctrl", translation[["use_date_q"]][[lg]],
                                       choices= c(translation[["Yes"]][[lg]], translation[["No"]][[lg]]),
                                       selected= translation[["Yes"]][[lg]]),
                          conditionalPanel(
                            condition = paste("input.date_ctrl == '",translation[["No"]][[lg]],"'",sep=""),
                            sliderInput("anchor",
                                        translation[["anchor_q"]][[lg]],
                                        min = 0,
                                        max = 5000,
                                        value = 100),
                            sliderInput("days",
                                        translation[["days_q"]][[lg]],
                                        min = -15,
                                        max = 365,
                                        value = c(-1,90))),
                          conditionalPanel(
                            condition = paste("input.date_ctrl == '",translation[["Yes"]][[lg]],"'",sep=""),
                            sliderInput("date_range",
                                        translation[["date_range_q"]][[lg]],
                                        min = dateLimits[1],
                                        max = dateLimits[2],
                                        value = c(Sys.Date()-60,dateLimits[2]))),
                          
                          checkboxInput("logscale", translation[["logscale_q"]][[lg]], value = TRUE),
                          
                          conditionalPanel(
                            condition = "input.logscale",
                            sliderInput("log_slider", translation[["logvalue_q"]][[lg]],
                                        min = 0, max = 7, value = 5, step = 1)
                          ),
                          
                          conditionalPanel(
                            condition = "!input.logscale",
                            sliderInput("cases.y",
                                        translation[["value_q"]][[lg]],
                                        min = 1,
                                        max = 200*10^3,
                                        value = 30*10^3),
                            
                            sliderInput("cases.y.fine",
                                        translation[["fine_q"]][[lg]],
                                        min = 69*10^3,
                                        max = 71*10^3,
                                        value = 70*10^3))
                   ),
                   column(3, offset = 0,
                          radioButtons("style", translation[["style_q"]][[lg]],
                                       choices = choices_style),
                          selectInput("est.ctrl",translation[["trend_line"]][[lg]] ,
                                      choices = c(translation[["none"]][[lg]], "3", "4", "7", "15"),
                                      selected = translation[["none"]][[lg]]
                          ),
                          selectInput("high.ctrl",translation[["high_q"]][[lg]]  ,
                                      choices = c(translation[["none"]][[lg]] ,countryList),
                                      selected = "BRA:Brasil"
                          ),
                          htmlOutput("doublingTime"),
                          br(),
                          checkboxGroupInput("mark.ctrl", 
                                             translation[["marker_q"]][[lg]], 
                                             choices = c(translation[["marker"]][[lg]], translation[["line"]][[lg]], 
                                                         translation[["line_style"]][[lg]], translation[["color"]][[lg]], 
                                                         translation[["background_lines"]][[lg]]),
                                             selected = c(translation[["line"]][[lg]], translation[["color"]][[lg]]),
                                             inline = TRUE)
                          
                          
                   ),
                   column(6,
                          
                          
                          checkboxGroupInput("CR.ctrl", label = h3(translation[["selected_q"]][[lg]]), inline = TRUE,
                                             choices = countryList,
                                             selected = countryList),
                          hr(),
                          h5(translation[["add_below_q"]][[lg]]),
                          selectInput("countries.ctrl.sel", label = h3(translation[["countries"]][[lg]]), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.C"])), 
                                      selected = " ", multiple = TRUE),
                          
                          selectInput("brauf.ctrl.sel", label = h3(translation[["bra_uf"]][[lg]]), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "BRA.UF"])), 
                                      selected = " ", multiple = TRUE),
                          
                          selectInput("bract.ctrl.sel", label = h3(translation[["bra_ct"]][[lg]]), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "IO.CT"])), 
                                      selected = " ", multiple = TRUE),
                          selectInput("regions.ctrl.sel", label = h3(translation[["wd_prov"]][[lg]]), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.R"])), 
                                      selected = " ", multiple = TRUE),
                          selectInput("us50.ctrl.sel", label = h3(translation[["us_states"]][[lg]]), 
                                      choices = c(" ",as.character(countryChoices$Country.Region[countryChoices$Group %in% "JHU.US"])), 
                                      selected = " ", multiple = TRUE),
                          
                          
                          
                   )), 
                 fluidRow( 
                   hr(),
                   
                   h4(translation[["by"]][[lg]]),
                   tags$a(id = "about"),
                   tags$a(href="https://twitter.com/robertodepinho", "@robertodepinho"),
                   br(),
                   p(translation[["last"]][[lg]], timeStamp),
                   br(),
                   h4(translation[["host"]][[lg]]),
                   tags$a(href="http://nbcgib.uesc.br/nbcgib/",
                          "NBCGIB/CCAM/PPGMC/UESC"),
                   p("Núcleo de Biologia Computacional e Gestão de Informações Biotecnológicas,"),
                   p("Centro de Computação Avançada e Modelagem, "),
                   p("Programa de Pós-Graduação em Modelagem Computacional em Ciência e Tecnologia "),
                   h5("Universidade Estadual de Santa Cruz "),
                   h5("Bahia, Brasil."),
                   tags$a(href='http://nbcgib.uesc.br/ppgmc/',
                          tags$img(src='http://nbcgib.uesc.br/ppgmc/imagens/logotopo.png',height='80')),
                   br(),
                   h4(translation[["data_sources"]][[lg]]),
                   h5(translation[["countries"]][[lg]]),
                   tags$a(href="https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data", 
                          "CSSE COVID-19 Dataset - Johns Hopkins University"),
                   h5(translation[["bra_uf"]][[lg]]),
                   
                   tags$a(href="https://brasil.io/", 
                          "Brasil.io"),
                   #tags$a(href="https://covid.saude.gov.br/", 
                   #        "Ministério da Saúde (Brasil)"),
                   
                   h5(translation[["bra_ct"]][[lg]]),
                   tags$a(href="https://brasil.io/", 
                          "Brasil.io"),
                   br(),
                   tags$a(id = "double"),
                   #h4("Doubling time estimate model:"),
                   #tags$a(href="https://github.com/covid19br/covid19br.github.io", 
                   #      "Observatório COVID-19 BR"),
                   #p("Showing median of last 15 days and median of their confidence intervals (level=0.95)."),
                   h4(translation[["source"]][[lg]]),
                   tags$a(href="https://github.com/robertodepinho/vizCovidDashboard", 
                          "github"),
                   
                   
                 )
)


######################################## UI ###################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
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
        if(input$high.ctrl %in% TTTGroup) { highSel = input$high.ctrl} else { highSel = translation[["none"]][[lg]]}
        updateSelectInput(session, "high.ctrl", choices = c(translation[["none"]][[lg]], TTTGroup), selected = highSel)      
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
    if(input$high.ctrl != translation[["none"]][[lg]] & input$est.ctrl != "EST:Doubling Time (median)") {
      doublingTime = as.numeric(input$est.ctrl)
    } 
    
    if(input$high.ctrl != translation[["none"]][[lg]]  & input$est.ctrl == "EST:Doubling Time (median)") {
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
    
    if(input$date_ctrl == translation[["Yes"]][[lg]]) {
      anchorCases = 0
    }
    
    #prepare data
    listP = chartDataPrepare(input$var_ctrl,tsCAgg, anchorCases, input$days, 
                             casesValue, input$logscale, countryList, 
                             backgroundList, lg, translation)
    
    if(!listP[[1]]) {
      g = ggplot(data.frame(x =c(0,100), y = c(0,100)), aes(x= x, y= y)) + 
        geom_point() +
        annotate("text", label = translation[["no_data"]][[lg]], x = 50, y = 50)
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
                   input$days, casesValue, input$logscale, countryList, 
                   input$mark.ctrl, input$high.ctrl, doublingTime, 
                   input$est.ctrl, backgroundList, lg, translation)
      } else {
        covidColorDate(input$var_ctrl,tsCAgg,listP, anchorCases, 
                       input$days, casesValue, input$logscale, countryList, 
                       input$mark.ctrl, input$high.ctrl, doublingTime, 
                       input$est.ctrl, input$date_range, backgroundList, lg, translation)
        
        
      }
      
    } else {
      if(anchorCases!=0){
        covidBlue(input$var_ctrl,tsCAgg,listP, anchorCases, 
                  input$days, casesValue, input$logscale, countryList, lg, translation)
      } else {
        covidBlueDate(input$var_ctrl,tsCAgg,listP, anchorCases, 
                      input$days, casesValue, input$logscale, countryList,
                      input$date_range, lg = lg, translation = translation)
      }
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
