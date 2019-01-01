library(data.table)
library(randomForest)
library(dplyr)
require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters
library(ggplot2)


set.seed(5497)
#將數據順序重新排列
part = sample(n)

load_data <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..",	"X.PTS.",	"X.FGM.",	"X.FGA.", "X.FG..",	"X.3PM.", "X.3PA.",	"X.3P..", "X.FTM.", "X.FTA.",	"X.FT..","X.OREB.",	"X.DREB.",	"X.REB.",	"X.AST.", "X.TOV.",	"X.STL.", "X.BLK.",	"X.BLKA.", "X.PF.",	"X.PFD.",	"X.....",	"X.WIN...1", "X.PTS..1",	"X.FGM..1",	"X.FGA..1"	,"X.FG...1", "X.3PM..1",	"X.3PA..1",	"X.3P...1",	"X.FTM..1", "X.FTA..1",	"X.FT...1",	"X.OREB..1",	"X.DREB..1", "X.REB..1",	"X.AST..1",	"X.TOV..1",	"X.STL..1", "X.BLK..1", "X.BLKA..1",	"X.PF..1",	"X.PFD..1","X......1")
  date = total %>%select("date")
  who_win = total["score1"]<total["score2"]
  team_data = total %>%select("team1", "team2")
  return(list(total_data, who_win, team_data))
}



load_data_advanced <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..",	"X.PTS.",	"X.FGM.",	"X.FGA.", "X.FG..",	"X.3PM.", "X.3PA.",	"X.3P..", "X.FTM.", "X.FTA.",	"X.FT..","X.OREB.",	"X.DREB.",	"X.REB.",	"X.AST.", "X.TOV.",	"X.STL.", "X.BLK.",	"X.BLKA.", "X.PF.",	"X.PFD.",	"X.....",	"X.WIN...1", "X.PTS..1",	"X.FGM..1",	"X.FGA..1"	,"X.FG...1", "X.3PM..1",	"X.3PA..1",	"X.3P...1",	"X.FTM..1", "X.FTA..1",	"X.FT...1",	"X.OREB..1",	"X.DREB..1", "X.REB..1",	"X.AST..1",	"X.TOV..1",	"X.STL..1", "X.BLK..1", "X.BLKA..1",	"X.PF..1",	"X.PFD..1","X......1"
                               , "X.OFFRTG.", "X.DEFRTG.",	"X.NETRTG.",	"X.AST..", "X.AST.TO.",	"X.ASTRATIO.",	"X.OREB..",	"X.DREB..", "X.REB..",	"X.TOV..",	"X.EFG..",	"X.TS..",	"X.PACE.",	"X.PIE.",	"X.OFFRTG..1",	"X.DEFRTG..1",	"X.NETRTG..1",	"X.AST...1",	"X.AST.TO..1",	"X.ASTRATIO..1",	"X.OREB...1",	"X.DREB...1",	"X.REB...1",	"X.TOV...1",	"X.EFG...1",	"X.TS...1",	"X.PACE..1",	"X.PIE..1"
  )
  date = total %>%select("date")
  who_win = total["score1"]<total["score2"]
  team_data = total %>%select("team1", "team2")
  return(list(total_data, who_win, team_data))
}

load_data_1 <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..","X.WIN...1","X.PFD.","X.PFD..1",	"X.....","X......1","X.DEFRTG.","X.DEFRTG..1","X.EFG..","X.EFG...1", 	"X.PACE.",	"X.PACE..1")
  
  date = total %>%select("date")
  who_win = total["score1"]<total["score2"]
  team_data = total %>%select("team1", "team2")
  return(list(total_data, who_win, team_data))
}


rt = load_data_1("NBA.csv")
rt2 = 
X = rt[[1]]
Y = rt[[2]]



colnames(data)[4]
team = rt[[3]]
Y <- as.numeric(Y)
head(Y)

train = cbind(X,Y)

featrure_size = length(X)

#(2)測試模型
#取得總筆數
n <- nrow(train)
#設定隨機數種子
set.seed(1117)
#將數據順序重新排列
part = sample(n)
newtotal <- train[part,]
team <-team[part,]



#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.7 * n))

#訓練資料與測試資料比例: 70%建模，30%驗證
traindata <- newtotal[t_idx,]
testdata <- newtotal[ - t_idx,]

trainteam <- team[t_idx,]
testteam <- team[ - t_idx,]

randomforestM <- randomForest(Y ~ ., data = traindata, importane = T, proximity = T, do.trace = 200)

randomforestM

#錯誤率: 利用OOB(Out Of Bag)運算出來的
plot(randomforestM)

round(importance(randomforestM), 2)

#(3)預測
result2 <- predict(randomforestM, newdata = testdata[1:featrure_size])
result_Approved2 <- ifelse(result2 > 0.5, 1, 0)

#(4)建立混淆矩陣(confusion matrix)觀察模型表現
cm2 <- table(testdata$Y, result_Approved2, dnn = c("實際", "預測"))

#(5)正確率
#計算核準卡正確率
cm2[4] / sum(cm2[, 2])

#計算拒補件正確率
cm2[1] / sum(cm2[, 1])

#整體準確率(取出對角/總數)
accuracy2 <- sum(diag(cm2)) / sum(cm2)
accuracy2




nnetM <- nnet(formula = Y ~ ., linout = F, size = 5, decay = 0.001, maxit = 1000, trace = T, data = traindata)

#(3)畫圖 
#plot.nnet(nnetM, wts.only = F)


#(4)預測
#test組執行預測 
prediction <- predict(nnetM, testdata[1:featrure_size])
predict_Approved <- ifelse(prediction > 0.5, 1, 0)
#預測結果 
cm <- table(x = testdata$Y, y = predict_Approved, dnn = c("實際", "預測"))
cm

#(5)正確率
#計算核準卡正確率
cm[4] / sum(cm[, 2])

#計算拒補件正確率
cm[1] / sum(cm[, 1])

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy

length(X)

fair_rate <- function(pro ){
  
  return(1/pro)}




mode_predict<- function(test_data, team_list, which_model){
  
      prediction_pro = predict(which_model, newdata = test_data)
      prediction <- ifelse(prediction_pro > 0.5, 1, 0)
      print(prediction)
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
output = mode_predict(testdata[1:10, 1:featrure_size], testteam[1:10,], randomforestM)


saveRDS(randomforestM, "random_forest.rds")
saveRDS(nnetM, "nn_one_hidden.rds")


data_today = read.csv("fake_data.csv")

load_predict_data <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN..",	"X.PTS.",	"X.FGM.",	"X.FGA.", "X.FG..",	"X.3PM.", "X.3PA.",	"X.3P..", "X.FTM.", "X.FTA.",	"X.FT..","X.OREB.",	"X.DREB.",	"X.REB.",	"X.AST.", "X.TOV.",	"X.STL.", "X.BLK.",	"X.BLKA.", "X.PF.",	"X.PFD.",	"X.....",	"X.WIN...1", "X.PTS..1",	"X.FGM..1",	"X.FGA..1"	,"X.FG...1", "X.3PM..1",	"X.3PA..1",	"X.3P...1",	"X.FTM..1", "X.FTA..1",	"X.FT...1",	"X.OREB..1",	"X.DREB..1", "X.REB..1",	"X.AST..1",	"X.TOV..1",	"X.STL..1", "X.BLK..1", "X.BLKA..1",	"X.PF..1",	"X.PFD..1","X......1")
  team_data = total %>%select("team1", "team2")
  return(list(total_data, team_data))
}

rt_t = load_predict_data("fake_data.csv")
X_t = rt_t[[1]]
team_t = rt_t[[2]]
output_t = mode_predict(X_t, team_t, randomforestM)