source("Global.R", local  = TRUE)

shinyUI(
  
  navbarPage(
    theme = shinythemes::shinytheme("flatly"),
    
    "NBA數據",  
             tabPanel("簡介",
                      tags$h2("簡介"),br(),
                      tags$h5("專案成員: 易行祐, 游博任, 鄭鈞瀚"),br(),
                      tags$h4("本專案共有三項功能，包含:"),br(),
                      tags$h4("1, 隊伍表現"),br(),
                      tags$h4("2, 近期表現"),br(),
                      tags$h4("3, 比賽預測"),br(),
                      tags$h5("資料來源： NBA Stats, PTT NBA")
             ),
             tabPanel("球隊表現",
                      sidebarPanel(
                        selectInput("Team_select", label = h3("球隊選擇"), 
                                    choices = c("Atlanta Hawks" , "Boston Celtics" , "Brooklyn Nets" , "Charlotte Hornets" , "Chicago Bulls" , "Cleveland Cavaliers" , "Dallas Mavericks" , "Denver Nuggets" , "Detroit Pistons" , "Golden State Warriors" , "Houston Rockets" , "Indiana Pacers" , "LA Clippers" , "Los Angeles Lakers" , "Memphis Grizzlies" , "Miami Heat" , "Milwaukee Bucks" , "Minnesota Timberwolves" , "New Orleans Pelicans" , "New York Knicks" , "Oklahoma City Thunder" , "Orlando Magic" , "Philadelphia 76ers" , "Phoenix Suns" , "Portland Trail Blazers" , "Sacramento Kings" , "San Antonio Spurs" , "Toronto Raptors" , "Utah Jazz" , "Washington Wizards" ), 
                                    selected = "Philadelphia 76ers"),
                        
                        hr(),
                        selectInput("Xselect_1", label = h3("X軸數據選擇"), 
                                    choices = list("Games Played" = 1, "Wins" = 2, "Losses" = 3, "Win Percentage" = 4, "Minutes Played" = 5, "Points" = 6, "Field Goalds Made" = 7, "Field Goals Attempted" = 8, "Field Goal Percentage" = 9, "3 Point Field Goals Made" = 10, "3 Point Field Goals Attempted" = 11, "3 Point Field Goals Percentage" = 12, "Free Throws Made" = 13, "Free Throws Attempted" = 14, "Free Throw Percentage" = 15, "Offensive Rebounds" = 16, "Defensive Rebounds" = 17, "Rebounds" = 18, "Assists" = 19, "Turnovers" = 20, "Steals" = 21, "Blocks" = 22, "Blocked Field Goal Attempts" = 23, "Personal Fouls" = 24, "Personal Fouls Drawn" = 25, "Plus Minus" = 26, "Offensive Rating" = 27, "Defensive Rating" = 28, "Net Rating" = 29, "Assist Percentage" = 30, "AST/TO" = 31, "Assist to Turnover Ratio" = 32, "Offensive Rebound Percentage" = 33, "Defensive Rebound Percentage" = 34, "Rebound Percentage" = 35, "Turnover Percentage" = 36, "Effective Field Goal Percentage" = 37, "True Shooting Percentage" = 38, "Pace" = 39, "Player Impact Estimate" = 40), 
                                    selected = 27),
                        
                        hr(),
                        selectInput("Yselect_1", label = h3("Y軸數據選擇"), 
                                    choices = list("Games Played" = 1, "Wins" = 2, "Losses" = 3, "Win Percentage" = 4, "Minutes Played" = 5, "Points" = 6, "Field Goalds Made" = 7, "Field Goals Attempted" = 8, "Field Goal Percentage" = 9, "3 Point Field Goals Made" = 10, "3 Point Field Goals Attempted" = 11, "3 Point Field Goals Percentage" = 12, "Free Throws Made" = 13, "Free Throws Attempted" = 14, "Free Throw Percentage" = 15, "Offensive Rebounds" = 16, "Defensive Rebounds" = 17, "Rebounds" = 18, "Assists" = 19, "Turnovers" = 20, "Steals" = 21, "Blocks" = 22, "Blocked Field Goal Attempts" = 23, "Personal Fouls" = 24, "Personal Fouls Drawn" = 25, "Plus Minus" = 26, "Offensive Rating" = 27, "Defensive Rating" = 28, "Net Rating" = 29, "Assist Percentage" = 30, "AST/TO" = 31, "Assist to Turnover Ratio" = 32, "Offensive Rebound Percentage" = 33, "Defensive Rebound Percentage" = 34, "Rebound Percentage" = 35, "Turnover Percentage" = 36, "Effective Field Goal Percentage" = 37, "True Shooting Percentage" = 38, "Pace" = 39, "Player Impact Estimate" = 40), 
                                    selected = 37),
                        
                        hr()
                      ),
                      mainPanel(
                        plotlyOutput("teamPlot")
                      )
             ),
             tabPanel("賽季表現",
                       sidebarPanel(
                         selectInput("month_select", label = h3("日期選擇"), 
                                     choices = c("All Games" , "Last 5 Games" , "Last 10 Games" , "Last 15 Games"), 
                                     selected = "Last 15 Games"),
                         
                         hr(),
                         selectInput("Xselect_2", label = h3("X軸數據選擇"), 
                                     choices = list("Games Played" = 1, "Wins" = 2, "Losses" = 3, "Win Percentage" = 4, "Minutes Played" = 5, "Points" = 6, "Field Goalds Made" = 7, "Field Goals Attempted" = 8, "Field Goal Percentage" = 9, "3 Point Field Goals Made" = 10, "3 Point Field Goals Attempted" = 11, "3 Point Field Goals Percentage" = 12, "Free Throws Made" = 13, "Free Throws Attempted" = 14, "Free Throw Percentage" = 15, "Offensive Rebounds" = 16, "Defensive Rebounds" = 17, "Rebounds" = 18, "Assists" = 19, "Turnovers" = 20, "Steals" = 21, "Blocks" = 22, "Blocked Field Goal Attempts" = 23, "Personal Fouls" = 24, "Personal Fouls Drawn" = 25, "Plus Minus" = 26), 
                                     selected = 16),
                         
                         hr(),
                         selectInput("Yselect_2", label = h3("Y軸數據選擇"), 
                                     choices = list("Games Played" = 1, "Wins" = 2, "Losses" = 3, "Win Percentage" = 4, "Minutes Played" = 5, "Points" = 6, "Field Goalds Made" = 7, "Field Goals Attempted" = 8, "Field Goal Percentage" = 9, "3 Point Field Goals Made" = 10, "3 Point Field Goals Attempted" = 11, "3 Point Field Goals Percentage" = 12, "Free Throws Made" = 13, "Free Throws Attempted" = 14, "Free Throw Percentage" = 15, "Offensive Rebounds" = 16, "Defensive Rebounds" = 17, "Rebounds" = 18, "Assists" = 19, "Turnovers" = 20, "Steals" = 21, "Blocks" = 22, "Blocked Field Goal Attempts" = 23, "Personal Fouls" = 24, "Personal Fouls Drawn" = 25, "Plus Minus" = 26), 
                                     selected = 9),
                         
                         hr(),
                         fluidRow(column(10, verbatimTextOutput("value")))
                       ),
                       mainPanel(
                         plotlyOutput("monthPlot")
                       )
             ),
             tabPanel(
               "比賽預測",
               sidebarPanel(
                 selectInput("m", label = h3("method"), 
                             choices = list("random_forest_raw", "nn_raw","ensemble_raw"
                                            ,"random_forest_raw+advanced", "nn_raw+advanced","ensemble_raw+advanced"
                                            ,"random_forest_self_select1", "nn_self_select1"
                                            ,"random_forest__self_select2", "nn__self_select2","ensemble_total2"))),
               tags$h2("今日比賽預測及賠率預估"),br(),
               mainPanel(
                 tableOutput("view")
               )
             ),
             tabPanel("數據資料表格",
                      sidebarPanel(
                        selectInput("table_select", label = h3("表格選擇"), 
                                    choices = c("賽季球隊數據", "本季數據", "近5場比賽數據", "近10場比賽數據", "近15場比賽數據"), 
                                    selected = "賽季球隊數據"),
                        
                        hr()
                      ),
                      mainPanel(
                        DT::dataTableOutput("mytable")
                      )
             )
             
  )
)