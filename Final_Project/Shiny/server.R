rsconnect::setAccountInfo(name='fawen5566', token='3F2FB6DA9605C65945E00553A34D8028', secret='zk7w7oLowDgkpPYbBZzi8cpAq0DUexB/ON4Sto7+')
options(encoding = "UTF-8")
source("Global.R", local  = TRUE)


data_1 = read.csv("DATA2018.csv")
print("step1")
data_all = read.csv("all.csv")
data_last5 = read.csv("last5.csv")
data_last10 = read.csv("last10.csv")
data_last15 = read.csv("last15.csv")

load_predict_data <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..",	"X.PTS.",	"X.FGM.",	"X.FGA.", "X.FG..",	"X.3PM.", "X.3PA.",	"X.3P..", "X.FTM.", "X.FTA.",	"X.FT..","X.OREB.",	"X.DREB.",	"X.REB.",	"X.AST.", "X.TOV.",	"X.STL.", "X.BLK.",	"X.BLKA.", "X.PF.",	"X.PFD.",	"X.....",	"X.WIN...1", "X.PTS..1",	"X.FGM..1",	"X.FGA..1"	,"X.FG...1", "X.3PM..1",	"X.3PA..1",	"X.3P...1",	"X.FTM..1", "X.FTA..1",	"X.FT...1",	"X.OREB..1",	"X.DREB..1", "X.REB..1",	"X.AST..1",	"X.TOV..1",	"X.STL..1", "X.BLK..1", "X.BLKA..1",	"X.PF..1",	"X.PFD..1","X......1")
  team_data = total %>%select("team1", "team2")
  return(list(total_data, team_data))
  }
load_predict_data1 <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..",	"X.PTS.",	"X.FGM.",	"X.FGA.", "X.FG..",	"X.3PM.", "X.3PA.",	"X.3P..", "X.FTM.", "X.FTA.",	"X.FT..","X.OREB.",	"X.DREB.",	"X.REB.",	"X.AST.", "X.TOV.",	"X.STL.", "X.BLK.",	"X.BLKA.", "X.PF.",	"X.PFD.",	"X.....",	"X.WIN...1", "X.PTS..1",	"X.FGM..1",	"X.FGA..1"	,"X.FG...1", "X.3PM..1",	"X.3PA..1",	"X.3P...1",	"X.FTM..1", "X.FTA..1",	"X.FT...1",	"X.OREB..1",	"X.DREB..1", "X.REB..1",	"X.AST..1",	"X.TOV..1",	"X.STL..1", "X.BLK..1", "X.BLKA..1",	"X.PF..1",	"X.PFD..1","X......1"
                               , "X.OFFRTG.", "X.DEFRTG.",	"X.NETRTG.",	"X.AST..", "X.AST.TO.",	"X.ASTRATIO.",	"X.OREB..",	"X.DREB..", "X.REB..",	"X.TOV..",	"X.EFG..",	"X.TS..",	"X.PACE.",	"X.PIE.",	"X.OFFRTG..1",	"X.DEFRTG..1",	"X.NETRTG..1",	"X.AST...1",	"X.AST.TO..1",	"X.ASTRATIO..1",	"X.OREB...1",	"X.DREB...1",	"X.REB...1",	"X.TOV...1",	"X.EFG...1",	"X.TS...1",	"X.PACE..1",	"X.PIE..1"
  )
  team_data = total %>%select("team1", "team2")
  return(list(total_data, team_data))
}
load_predict_data2 <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN...1", "X......1", "X.WIN..", "X....."
                               , "X.NETRTG..1", "X.NETRTG.", "X.PACE.", "X.PACE..1"
  )
  team_data = total %>%select("team1", "team2")
  return(list(total_data, team_data))
}
load_predict_data3 <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..","X.WIN...1","X.PFD.","X.PFD..1",	"X.....","X......1","X.DEFRTG.","X.DEFRTG..1","X.EFG..","X.EFG...1", 	"X.PACE.",	"X.PACE..1")
  
  team_data = total %>%select("team1", "team2")
  return(list(total_data, team_data))
}




mode_predict<- function(test_data, team_list, which_model){
  
  prediction_pro = predict(which_model, newdata = test_data)
  prediction <- ifelse(prediction_pro > 0.5, 1, 0)
  team1 = list()
  team2 = list()
  winner = list()
  fair_rate_1 = list()
  fair_rate_2 = list()
  for( i in 1:length(prediction_pro)){
    team1 = append(team1,as.character(team_list[[1]][i]))
    team2 = append(team2,as.character(team_list[[2]][i]))
    winner = append(winner,as.character(team_list[[prediction[i]+1]][i]))
    fair_rate_1 = append(fair_rate_1, fair_rate(1-prediction_pro[i]))
    fair_rate_2 = append(fair_rate_2, fair_rate( prediction_pro[i]))
    
  }
  pre <- lapply(list(team1, team2, winner, fair_rate_1, fair_rate_2), unlist)
  pre = data.frame(lapply(pre, `length<-`, max(lengths(pre))))
  colnames(pre) = c("team1", "team2", "winner", "fair_rate_1", "fair_rate_2")
  row.names(pre)<-1:nrow(pre)
  return(pre)
}


  ens_predict<- function(test_data, team_list){
    
    prediction_pro1 = predict(random_forest_model, newdata = test_data)
    prediction_pro2 = predict(nn_model, newdata = test_data)
    prediction_pro = (prediction_pro1+prediction_pro2)/2
    prediction <- ifelse(prediction_pro > 0.5, 1, 0)
    #print(prediction)
    team1 = list()
    team2 = list()
    winner = list()
    fair_rate_1 = list()
    fair_rate_2 = list()
    for( i in 1:length(prediction_pro)){
      team1 = append(team1,as.character(team_list[[1]][i]))
      team2 = append(team2,as.character(team_list[[2]][i]))
      winner = append(winner,as.character(team_list[[prediction[i]+1]][i]))
      fair_rate_1 = append(fair_rate_1, fair_rate(1-prediction_pro[i]))
      fair_rate_2 = append(fair_rate_2, fair_rate( prediction_pro[i]))
      
    }
    
  pre <- lapply(list(team1, team2, winner, fair_rate_1, fair_rate_2), unlist)
  pre = data.frame(lapply(pre, `length<-`, max(lengths(pre))))
  colnames(pre) = c("team1", "team2", "winner", "fair_rate_1", "fair_rate_2")
  row.names(pre)<-1:nrow(pre)
  return(pre)
  }
  ens_predict1<- function(test_data, team_list){
    
    prediction_pro1 = predict(random_forest_model1, newdata = test_data)
    prediction_pro2 = predict(nn_model1, newdata = test_data)
    prediction_pro = (prediction_pro1+prediction_pro2)/2
    prediction <- ifelse(prediction_pro > 0.5, 1, 0)
    #print(prediction)
    team1 = list()
    team2 = list()
    winner = list()
    fair_rate_1 = list()
    fair_rate_2 = list()
    for( i in 1:length(prediction_pro)){
      team1 = append(team1,as.character(team_list[[1]][i]))
      team2 = append(team2,as.character(team_list[[2]][i]))
      winner = append(winner,as.character(team_list[[prediction[i]+1]][i]))
      fair_rate_1 = append(fair_rate_1, fair_rate(1-prediction_pro[i]))
      fair_rate_2 = append(fair_rate_2, fair_rate( prediction_pro[i]))
      
    }
    
    pre <- lapply(list(team1, team2, winner, fair_rate_1, fair_rate_2), unlist)
    pre = data.frame(lapply(pre, `length<-`, max(lengths(pre))))
    colnames(pre) = c("team1", "team2", "winner", "fair_rate_1", "fair_rate_2")
    row.names(pre)<-1:nrow(pre)
    return(pre)
  }
  ens_predict3<- function(team_list){
    
    prediction_pro1 = predict(random_forest_model, newdata = X_t)
    prediction_pro2 = predict(nn_model, newdata = X_t)
    prediction_pro3 = predict(random_forest_model1, newdata = X_t1)
    prediction_pro4 = predict(nn_model1, newdata = X_t1)
    prediction_pro5 = predict(random_forest_model2, newdata = X_t2)
    prediction_pro6 = predict(nn_model2, newdata = X_t2)
    prediction_pro7 = predict(random_forest_model3, newdata = X_t3)
    prediction_pro8 = predict(nn_model3, newdata = X_t3)
    prediction_pro = (prediction_pro1+prediction_pro2+prediction_pro3+prediction_pro4+prediction_pro5+prediction_pro6+prediction_pro7+prediction_pro8)/8
    prediction <- ifelse(prediction_pro > 0.5, 1, 0)
    #print(prediction)
    team1 = list()
    team2 = list()
    winner = list()
    fair_rate_1 = list()
    fair_rate_2 = list()
    for( i in 1:length(prediction_pro)){
      team1 = append(team1,as.character(team_list[[1]][i]))
      team2 = append(team2,as.character(team_list[[2]][i]))
      winner = append(winner,as.character(team_list[[prediction[i]+1]][i]))
      fair_rate_1 = append(fair_rate_1, fair_rate(1-prediction_pro[i]))
      fair_rate_2 = append(fair_rate_2, fair_rate( prediction_pro[i]))
      
    }
    
    pre <- lapply(list(team1, team2, winner, fair_rate_1, fair_rate_2), unlist)
    pre = data.frame(lapply(pre, `length<-`, max(lengths(pre))))
    colnames(pre) = c("team1", "team2", "winner", "fair_rate_1", "fair_rate_2")
    row.names(pre)<-1:nrow(pre)
    return(pre)
  }

fair_rate <- function(pro ){
  
  return(1/pro)}

random_forest_model <- readRDS("random_forest.rds")
nn_model <- readRDS("nn_one_hidden.rds")
random_forest_model1 <- readRDS("random_forest_raw_and_advanced.rds")
nn_model1 <- readRDS("nn_one_hidden_raw_and_advanced.rds")
random_forest_model2 <- readRDS("random_forest_sel1.rds")
nn_model2 <- readRDS("nn_one_hidden_sel1.rds")
random_forest_model3 <- readRDS("random_forest_sel2.rds")
nn_model3 <- readRDS("nn_one_hidden_sel2.rds")
print("step2")
rt_t = load_predict_data("fake_data.csv")
rt_t1 = load_predict_data1("fake_data.csv")
rt_t2 = load_predict_data2("fake_data.csv")
rt_t3 = load_predict_data3("fake_data.csv")
print("step3")
X_t = rt_t[[1]]
team_t = rt_t[[2]]
X_t1 = rt_t1[[1]]
team_t1 = rt_t1[[2]]
X_t2 = rt_t2[[1]]
team_t2 = rt_t2[[2]]
X_t3 = rt_t3[[1]]
team_t3 = rt_t3[[2]]
pre1  = mode_predict(X_t, team_t, random_forest_model)
pre2  = mode_predict(X_t, team_t, nn_model)
pre3  = ens_predict(X_t, team_t)
print("step4")
pre11  = mode_predict(X_t1, team_t1, random_forest_model1)
pre21  = mode_predict(X_t1, team_t1, nn_model1)
pre31  = ens_predict1(X_t1, team_t1)
print("step5")
pre12  = mode_predict(X_t2, team_t2, random_forest_model2)
pre22  = mode_predict(X_t2, team_t2, nn_model2)

print("step6")
pre13  = mode_predict(X_t3, team_t3, random_forest_model3)
pre23  = mode_predict(X_t3, team_t3, nn_model3)
print("step7")
pre_t  = ens_predict3(team_t1)
shinyServer(function(input, output) {
  
  
  output$teamPlot <- renderPlotly({
    print(input$Xselect_1)
    print(input$Yselect_1)
    x_ax = colnames(data_1)[as.integer(input$Xselect_1) + 2]
    y_ax = colnames(data_1)[as.integer(input$Yselect_1) + 2]
    print(x_ax)
    print(y_ax)
    #ggplot(data = data_1[data_1$Team == input$Team_select,], aes_string(x= x_ax, y= y_ax)) + geom_point()+geom_text(aes(label=Time),hjust=0, vjust=0, nudge_x = 0.1, nudge_y = 0.1, check_overlap = TRUE)
    ggplot(data = data_1[data_1$Team == input$Team_select,], aes_string(x= x_ax, y= y_ax, color= "Time")) + geom_point()
  })
  
  output$monthPlot <- renderPlotly({
    print(input$Xselect_2)
    print(input$Yselect_2)
    x_ax1 = colnames(data_1)[as.integer(input$Xselect_2) + 2]
    y_ax1 = colnames(data_1)[as.integer(input$Yselect_2) + 2]
    print(x_ax1)
    print(y_ax1)
    
    if(input$month_select == "All Games"){
      ggplot(data = data_all, aes_string(x= x_ax1, y= y_ax1, color= "TEAM")) + geom_point()
    }
    else if (input$month_select == "Last 5 Games"){
      ggplot(data = data_last5, aes_string(x= x_ax1, y= y_ax1, color= "TEAM")) + geom_point()
    }
    else if (input$month_select == "Last 10 Games"){
      ggplot(data = data_last10, aes_string(x= x_ax1, y= y_ax1, color= "TEAM")) + geom_point()
    }
    else {
      ggplot(data = data_last15, aes_string(x= x_ax1, y= y_ax1, color= "TEAM")) + geom_point()
    }
  })
  
  output$view = renderTable({
    if (input$m == "random_forest_raw"){
      print(pre1)}
    else if (input$m == "nn_raw"){
      print(pre2)}
    else if (input$m == "ensemble_raw"){
      print(pre3)}
    else if (input$m == "random_forest_raw+advanced"){
      print(pre11)}
    else if (input$m == "nn_raw+advanced"){
      print(pre21)}
    else if (input$m == "ensemble_raw+advanced"){
      print(pre31)}
    else if (input$m == "random_forest_self_select1"){
      print(pre12)}
    else if (input$m == "nn_self_select1"){
      print(pre22)}
    else if (input$m == "random_forest_self_select2"){
      print(pre13)}
    else if (input$m == "nn_self_select2"){
      print(pre23)}
    else {
      print(pre_t)}
  })  
  
  output$mytable = DT::renderDataTable({
    if (input$table_select == "Team datatable"){
      data_1
    }
    else if (input$table_select == "All games datatable"){
      data_all
    }
    else if (input$table_select == "Last 5 datatable") {
      data_last5
    }
    else if (input$table_select == "Last 10 datatable") {
      data_last10
    }
    else {
      data_last15
    }
  })  

})