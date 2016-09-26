
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("NBA Shot Charts"),
  fluidRow(
    column(3,
           selectInput(inputId = "Home",
                       label = "Home Team:",
                       choices =  c("Atl", "Bos", "Bro", "Cha","Chi","Cle", "Dal", "Den", "Det", "GSW", 
                                    "Hou", "Ind", "LAC", "LAL", "Mia", "Mil", "Min", "NO", "NY", "Okc", "Orl", "Phi",  "Pho", "Por", "Sac", "Sas" , "Tor", "Uta", "Mem", "Was"),
                       selected = "Sas"
           ),
           selectInput(inputId = "Visitor",
                       label = "Visitor Team:",
                       choices = c("Atl", "Bos", "Bro", "Cha","Chi","Cle", "Dal", "Den", "Det", "GSW", 
                                   "Hou", "Ind", "LAC", "LAL", "Mia", "Mil", "Min", "NO", "NY", "Okc", "Orl", "Phi",  "Pho", "Por", "Sac", "Sas" , "Tor", "Uta", "Mem", "Was"),
                       selected = "Phi"
           )
    ),
    column(4,
           numericInput(inputId = "quant", label = "Quantile", value = 0.7, min = 0.05, max = 0.95, step = 0.05)
    ),
    column(4,h3("Expected results:"),
           p(textOutput("offAPPS")),
           p(textOutput("defAPPS"))
    )
  ),
  
      plotOutput("distPlot",height = "1000px"),
      downloadButton('downloadPlot', 'Download Plot')

    )
  )
