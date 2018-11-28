library(Rfacebook)

fb_oauth = fbOAuth (app_id = "1895630287184773", app_secret = "51a9fd268ea7542952ec2aa9b624a21a")
dir_fb_oauth = "F:/Github/key/FB_oath/fb_oauth"
save(dir_fb_oauth, file = dir_fb_oauth )
load( dir_fb_oauth )
fb_page = getPage (page= "logitech.taiwan", token = token, n = 600)
names(fb_page)
messageTXT = fb_page$message[ 1:600 ]
dirDATA = "路徑"
write(messageTXT, file = dirDATA, ncolumns = length(messageTXT), sep=",")


library(tm)           # Text Mining Package
library(Rwordseg)     #中文斷詞
library(tmcn)         #處理中文字的輔助套件
library(wordcloud)    # Word Clouds
library(colorspace)   # Color Space Manipulation

#Representing and computing on corpora. - tm
FbData = Corpus(DirSource("F:/Github/key/FB_oath", encoding = "big5"))
#Interface to apply transformation functions to corpora. - tm
FbData = tm_map(FbData, stripWhitespace)
FbData = tm_map(FbData, removePunctuation)
FbData = tm_map(FbData, removeNumbers)
FbData = tm_map(FbData, function(word){gsub("[A-Za-z0-9]", "", word)})
#Create plain text documents
FbData = tm_map(FbData, PlainTextDocument)

StopWords = stopwordsCN( )
FbData = tm_map(FbData, removeWords, StopWords)
FbData2 = tm_map(FbData, ontent_transformer(segmentCN), nature = TRUE, returnType = 'tm')
FbData3 = Corpus(VectorSource(FbData2))

Fbcloud = segmentCN(FbData3[[1]]$content, nature = TRUE)
Fbcloud = unlist(Fbcloud)
noun = Fbcloud[ names (Fbcloud) == "n" ]
tab = table(noun)
Data = as.data.frame(tab[tab >= 1])

wordcloud(
  words = Data$noun, freq = Data$Freq,
  min.freq = 8,
  random.order = F, ordered.colors = T, scale=c(9,.8),
  colors = rainbow_hcl(nrow(Data))
)