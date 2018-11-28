rsconnect::setAccountInfo(name='fawen5566', token='3F2FB6DA9605C65945E00553A34D8028', secret='zk7w7oLowDgkpPYbBZzi8cpAq0DUexB/ON4Sto7+')
options(encoding = "UTF-8")
library(shiny)
library(datasets)
library(rvest)
library(dplyr)
library(plotly)
library(readr)
library(DT)


states = read_csv("Seasons_Stats.csv")

tsp_by_year = states %>% group_by(Year) %>% summarise(mean=mean(TSp, na.rm=TRUE))

ThreePp_by_year = states %>% group_by(Year) %>% summarise(mean=mean(ThreePp, na.rm=TRUE))

TwoPp_by_year = states %>% group_by(Year) %>% summarise(mean=mean(TwoPp, na.rm=TRUE))

FTA_by_year = states %>% group_by(Year) %>% summarise(mean=mean(FTA, na.rm=TRUE))


shinyServer(function(input, output) {
  
  
  output$tsPlot <- renderPlotly({
    
    plot_ly(tsp_by_year, x= tsp_by_year$Year ,y= tsp_by_year$mean, type = 'scatter',mode="markers",text = ~paste('Year: ', Year))
    
  })
  
  
  output$threePlot <- renderPlotly({
    
    plot_ly(ThreePp_by_year, x= TwoPp_by_year$Year ,y= ThreePp_by_year$mean, type = 'scatter',mode="markers",text = ~paste('Year: ', Year))
    
  })
  
  
  output$twoPlot <- renderPlotly({
    
    plot_ly(TwoPp_by_year, x= TwoPp_by_year$Year ,y= TwoPp_by_year$mean, type = 'scatter',mode="markers",text = ~paste('Year: ', Year))
    
  })
  
  
  output$FTAPlot <- renderPlotly({
    
    plot_ly(FTA_by_year, x= FTA_by_year$Year ,y= FTA_by_year$mean, type = 'scatter',mode="markers",text = ~paste('Year: ', Year))
    
  })
  
  
  output$mytable = DT::renderDataTable({
    states
  })  
  
})