#

library(shiny)
#library(shinydashboard)
library(leaflet)
library(RColorBrewer)
library(sf)
library(tidyverse)
library("dygraphs")
library(plotly)
library(ggplot2)

load("dados.RData")
source("Global.R")

function(input, output, session) {

  output$graf2 <- plotly::renderPlotly({

    p<-data_graf2 |> 
        ggplot(aes(x=mes, y=casos)) +
      geom_boxplot() +
      theme_bw() +
      geom_point(aes(x = mes,y = casos, text = as.character(ano)))
    
    plotly::ggplotly(p, tooltip = c("x", "y", "text"))
    
  })

    output$map <- renderLeaflet({
      
    
  m <- shp %>% 
        left_join(
          base[which(base$anomes == paste0(input$ano,ifelse(nchar(input$mes)==1,paste0("0",input$mes),input$mes))),
               c("geocod","tx_1k","anomes")], by=c("cod_geom"="geocod"))
  
  
  
  m$tx_1k[is.na(m$tx_1k)]<-0
  
  
            labels <- sprintf(
        "<strong>%s</strong><br/>%g Tx. por 100.000 hab.",
        m$NOME, round(m$tx_1k,2)
      ) %>% lapply(htmltools::HTML)
      
      leaflet(data = m) %>% 
        setView(-43.4197075,-22.9352151, zoom = 10) %>% 
        addTiles() %>% 
        addPolygons(layerId = m$cod_geom,
          fillColor = ~pal(tx_1k),
          weight = 1,
          opacity = 1,
          color = ~pal(tx_1k),#"white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            fill = "yellow",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>% 
        addLegend(pal = pal, values = ~tx_1k, opacity = 0.7, title = 'Quantil do pedíodo de 2012-2021',
                  position = "bottomright")
      
      
    })    

    output$mytable1 <- DT::renderDataTable({
      DT::datatable(m[,c("cod_geom", "NOME","anomes","tx_1k") , drop = FALSE], options = list(pageLength = -1))
    })
        
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        
        output$graf1 <- renderDygraph({
          
          temperatura<-ts(data = base$temp[which(base$geocod==event$id&substring(base$anomes,1,4)==input$ano)], start = c(input$ano,1), end = c(input$ano,12), frequency = 12)
          taxa<-ts(data = base$tx_1k[which(base$geocod==event$id&substring(base$anomes,1,4)==input$ano)], start = c(input$ano,1), end = c(input$ano,12), frequency = 12)
          
          
          tmp <- cbind(temperatura, taxa)
          
          dygraph(tmp, main = m$NOME[which(m$cod_geom == event$id)]) %>% 
            dyOptions(drawGrid = F) %>%
            dyAxis("y", label = "Temperatura média mensal", independentTicks = TRUE) %>%
            dyAxis("y2", label = "Tx por 100.000 hab. ", independentTicks = TRUE) %>%
            dySeries("taxa", axis=('y'), color = "red") %>%
            dySeries("temperatura", axis=('y2'), color = "orange",stepPlot = F, fillGraph = T)
          
        })
        
        
        output$graf3 <- renderDygraph({
          

          precip<-ts(data = base$precip[which(base$geocod==1&substring(base$anomes,1,4)==input$ano)], start = c(input$ano,1), end = c(input$ano,12), frequency = 12)
          temperatura<-ts(data = base$temp[which(base$geocod==1&substring(base$anomes,1,4)==input$ano)], start = c(input$ano,1), end = c(input$ano,12), frequency = 12)
          
          tmp2 <- cbind(precip, temperatura)
          
          dygraph(tmp2, main = "Precipitação x Temperatura") %>% 
            dyOptions(drawGrid = F) %>%
            dyAxis("y", label = "Precipitação média acumulada", independentTicks = TRUE) %>%
            dyAxis("y2", label = "Temperatura média ", independentTicks = TRUE) %>%
            dySeries("precip", axis=('y'), strokeWidth = 2, strokePattern = "dashed", color = "blue") %>%
            #dySeries("taxa", axis=('y2')) %>% 
            dySeries("temperatura", axis=('y2'), stepPlot = F, fillGraph = T, color = "orange") 
          
              
        })
        

      })
    })
    
  

}
