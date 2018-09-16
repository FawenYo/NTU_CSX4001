# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)

ans <- sample(0:9,4)
ans
GuessTimes <- 0
cat("題目已由亂數設定完成","\n")

repeat
{
    cat("請輸入4個不重複的數字(0~9)","\n")
    EnterNumber <- scan(n=1,quiet=T)
	
	while(!grepl("^[0-9]+$",EnterNumber))
    {
       print("輸入非整數")
       cat("請輸入4個不重複的數字(0~9)","\n")
       EnterNumber <- scan(n=4,quiet=T)
    }
  
    GuessTimes <- GuessTimes +1
	
	EnterNum1 <- EnterNumber %/% 1000
	EnterNum2 <- EnterNumber %/% 100 - EnterNum1 *10
	EnterNum3 <- EnterNumber %/% 10 - EnterNum1 * 100 -EnterNum2 * 10
	EnterNum4 <- EnterNumber - EnterNum1 * 1000 -EnterNum2 * 100 - EnterNum3 * 10
	GuessNumber <- c(EnterNum1, EnterNum2, EnterNum3, EnterNum4)
	
    a <- 0
    b <- 0
	
    for (i in 1:4)
    {
       if( GuessNumber[i] == ans[i])
       {
          a <- a+1
       }
       else
       for (j in 1:4)
       {
          if (GuessNumber[i] == ans[j])
          {
            b <- b+1
          }

       }
    }
	
	if(a == 4)
    {
      cat("恭喜答對!，本次共花費了", GuessTimes, "次")
      break
    }
	else
	{
    cat(a,"A", b ,"B", "，這是您第", GuessTimes, "次嘗試","\n")
	}
}
