library(data.table)
library(randomForest)
library(dplyr)
require(neuralnet) # for neuralnet(), nn model
require(nnet)      # for class.ind()
require(caret)     # for train(), tune parameters
library(ggplot2)

## 排列數據的方法 不改此項目的話 資料排序就為固定的
set.seed(5)
part = sample(5497)
n = 5497


## 
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
## 可以改變此函數total_data%>%select部分選擇上面 total select的部分 看要利用那幾項來進行train，後面有加.1就是第二隊的數據
load_data_1 <- function(ad){
  total = read.csv(ad)
  total_data = total %>%select("X.WIN...1", "X......1", "X.WIN..", "X....."
                               , "X.NETRTG..1", "X.NETRTG.", "X.PACE.", "X.PACE..1"
  )
  date = total %>%select("date")
  who_win = total["score1"]<total["score2"]
  team_data = total %>%select("team1", "team2")
  return(list(total_data, who_win, team_data))
}


rt = load_data_1("NBADATA3.csv")
X = rt[[1]]
Y = rt[[2]]



colnames(data)[4]
team = rt[[3]]
Y <- as.numeric(Y)
head(Y)

train = cbind(X,Y)

featrure_size = length(X)


newtotal <- train[part,]
team <-team[part,]




#取出樣本數的idx
t_idx <- sample(seq_len(n), size = round(0.8 * n))

#訓練資料與測試資料比例: 80%建模，20%驗證
traindata <- newtotal[t_idx,]
testdata <- newtotal[ - t_idx,]

trainteam <- team[t_idx,]
testteam <- team[ - t_idx,]


### ranndom forest train過程
##有興趣可以自己更改裡面的參數 上網查一下參數
randomforestM <- randomForest(Y ~ ., data = traindata, importane = T, proximity = T, do.trace = 50)#,ntree = 200)
randomforestM
plot(randomforestM)
round(importance(randomforestM), 2)




result2 <- predict(randomforestM, newdata = traindata[1:featrure_size])
result_Approved2 <- ifelse(result2 > 0.5, 1, 0)

print("random forest train error")
cm2 <- table(traindata$Y, result_Approved2, dnn = c("實際", "預測"))
cm2

accuracy2 <- sum(diag(cm2)) / sum(cm2)
accuracy2


result2 <- predict(randomforestM, newdata = testdata[1:featrure_size])
result_Approved2 <- ifelse(result2 > 0.5, 1, 0)



#(4)建立混淆矩陣(confusion matrix)觀察模型表現
print("random forest test error")
cm2 <- table(testdata$Y, result_Approved2, dnn = c("實際", "預測"))
cm2

#整體準確率(取出對角/總數)
accuracy2 <- sum(diag(cm2)) / sum(cm2)
accuracy2


##one layer neural network train process

##有興趣可以自己更改裡面的參數 上網查一下參數
nnetM <- nnet(formula = Y ~ ., linout = F, size = 4, decay = 0.02, maxit = 1000, trace = T, data = traindata)

#(3)畫圖 
#plot.nnet(nnetM, wts.only = F)

#(4)預測
#train組執行預測
 

prediction <- predict(nnetM, traindata[1:featrure_size])
predict_Approved <- ifelse(prediction > 0.5, 1, 0)
#預測結果 

print("nnet train error")
cm <- table(x = traindata$Y, y = predict_Approved, dnn = c("實際", "預測"))
cm

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy


#test組執行預測

prediction <- predict(nnetM, testdata[1:featrure_size])
predict_Approved <- ifelse(prediction > 0.5, 1, 0)
#預測結果
print("nnet test error")
cm <- table(x = testdata$Y, y = predict_Approved, dnn = c("實際", "預測"))
cm

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy



ens = (result2 + prediction)/2

#test組執行預測

predict_Approved <- ifelse(ens > 0.5, 1, 0)
#預測結果
print("ensemble test error")
cm <- table(x = testdata$Y, y = predict_Approved, dnn = c("實際", "預測"))
cm

#整體準確率(取出對角/總數)
accuracy <- sum(diag(cm)) / sum(cm)
accuracy


### 更改rds名稱可以存取模型為那個名稱
saveRDS(randomforestM, "random_forest_sel1.rds")
saveRDS(nnetM, "nn_one_hidden_sel1.rds")

