
library(shiny)
library(leaflet)
library(RColorBrewer)
library(sf)
library(tidyverse)
library("dygraphs")
library("shinythemes")
library(plotly)
library(ggplot2)

load("dados.RData")


fluidPage(theme = shinytheme("lumen"),


    titlePanel("Sentinela da Dengue no Rio de Janeiro"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width = 3,
            selectInput("ano", label = "Selecione um ano:", choices = c(2012:2021), selected = 2012), 
            selectInput("mes", label = "Selecione um mes:", choices = c(1:12), selected = 1)
        ),

        mainPanel(
          tabsetPanel(
            tabPanel("Mapa", leafletOutput("map"), h5("*Explore informações específicas do bairro, selecionáveis no mapa."), dygraphOutput("graf1"), dygraphOutput("graf3")),
            tabPanel("Panorama mensal dos Casos", plotlyOutput("graf2"))
            #tabPanel('Tabela', DT::dataTableOutput("mytable1"))
          ),
            
            
            
        )
    )
)
