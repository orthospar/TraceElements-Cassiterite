library(shiny)
library(plotly)

fluidPage(
  
  titlePanel("Blue Tier Cassiterite", windowTitle = "Cassiterite"),
  
  tabsetPanel(
    tabPanel("Correlations",
  fluidRow(
    column(width=12,
           h3("This page allows for the dynamic exploration of LA-ICP-MS trace element concentrations across a transect of a cassiterite crystal from Blue Tier, Tasmania."),
           h5(em("To start, click on an element pair in the correlation matrix below. This will load the scatter plot and line transects for those elements.")),
           hr(),
           br())
  ),
  fluidRow(
    column(width=6, plotlyOutput("heat")),
    column(width=6, plotlyOutput("scatterplot"))
  ),
  fluidRow(
    column(width=12, plotlyOutput("lineplot"),
    column(width=12, plotlyOutput("gglineplot"),
    hr(), 
    br())
  )),
  tabPanel("Colouration",
  fluidRow(
    column(width=3, selectInput("element", h4("Select an element:"), 
                                choices = list("Al","Sc","Ti","V","Mn",
                                                 "Co","Ni","Ga","Y",
                                                 "Zr","Nb","Mo","In","Sb",
                                                 "Cs","Ba","La","Ce","Lu",
                                                 "Hf","Ta","W","U"), selected = "Al")),
    column(width=12, 
           h5("Close up of central banding, comparing colouration with trace element contents"),
           plotOutput("close_upA"),
           plotOutput("close_upB"),
           br(),
           hr())
  ))
))