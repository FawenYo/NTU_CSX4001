install.packages('rvest')  #安裝rvest套件
library(rvest)

title <- read_html("https://technews.tw/")   
#輸入你要爬取網頁的網址，爬取此網頁的html資訊 

title <- html_nodes(title,".entry-title a")   
#函數首先代入剛剛的網頁資訊，逗號後就是很重要的輸入你前面使用Selector複製的程式碼，他會從此網頁中篩選出剛剛你想要爬的新聞標題。 

title <- html_text(title)   # 只篩選出文字
title <- iconv(title,"UTF-8")
news_title<- title

url <- read_html("https://technews.tw/")
url <- html_nodes(url,".maintitle a")
#這裡要注意，使用Selector選區塊時不要按在標題上，要點在旁邊，框框會比點在標題上面大喔，
#所以所選出來的程式碼和剛剛的不一樣。

url <- html_attr(url,"href") 
url
news_url <- url

news <- data.frame(news_title,news_url)
View(news)
