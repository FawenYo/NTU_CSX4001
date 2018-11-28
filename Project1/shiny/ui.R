library(shiny)
library(DT)
library(plotly)
# Define UI for miles per gallon application

shinyUI(
  
  navbarPage("NBA數據",  
             tabPanel("Overall",
                      headerPanel("NBA 1950~2017數據變化分析"),
                      h2("此份數據是蒐集自Kaggle網站上提供1950年至2017年NBA比賽數據"),
                      h3("真實命中率(TS%)項目是綜合2分.3分及罰球得分進而做為衡量球員得分能力之指標"),
                      h2("TS%計算方式為: 總得分/2*(出手次數 + 0.44 * 罰球次數)"),
                      h2("而在這專題中，我們以罰球次數做為防守強度判斷之依據")
             )
             ,
             
             
             navbarMenu("Analysis",
                        tabPanel("真實命中率 (TS%) 變化",
                                 basicPage(
                                   h1("真實命中率變化"),
                                   plotlyOutput("tsPlot")
                                 )
                        ),
                        
                        tabPanel("2分球命中率 及 3分球命中率 變化",
                                 basicPage(
                                   h1("2分球命中率變化"),
                                   plotlyOutput("twoPlot"),
                                   
                                   h1("3分球命中率變化"),
                                   plotlyOutput("threePlot")
                                 )
                        ),
                        
                        tabPanel("FTA (罰球出手次數) 變化",
                                 basicPage(
                                   h1("罰球出手次數變化"),
                                   plotlyOutput("FTAPlot")
                                 )
                        )
                        
             ),
             
             
             
             
             tabPanel("數據資料表格",
                      basicPage(
                        h2("NBA 1950~2017比賽數據"),
                        DT::dataTableOutput("mytable")
                      )
             )
             
  )
)
