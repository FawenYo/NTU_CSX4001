#WEB CRAWLER
library(rvest)
library(magrittr)
title=read_html("https://technews.tw/") %>%
html_nodes(".entry-title a , #album_test1 a , #album_test1 h3") %>%
html_text() %>% iconv("UTF-8")
title


#Phrasing and Clean meaningless text
library("jiebaR")
library("tm")
Sys.setlocale(category = "LC_ALL", locale = "cht")
cc = worker()
new_user_word(cc,'Pixel3',"n")
new_user_word(cc,'¾Ô¾÷',"n")
cc[title]

tabledata <-table(cc[title])
tabledata

data.frame(tabledata)

doc <- Corpus(VectorSource(title))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
doc <- tm_map(doc, toSpace, "ªº")

#Show wordcloud
library(wordcloud2)
wordcloud2(tabledata)
