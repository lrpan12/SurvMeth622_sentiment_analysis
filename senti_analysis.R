library(rtweet)
library(tidyverse)
library(tidytext)
library(ggmap)
library(ROAuth)
library(webshot)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)
library(httpuv)
# Emotions for each tweet using NRC dictionary
load("data/combined0310.RData")
######

tweets.df = combined_data %>%
  select(title) %>% 
  rename(text= title) %>% 
  distinct()

# tweets.df = tweets.df %>% 
#   data.frame()
# tweets.df$text
#tweets.df$text = tweets.df$.

tweets.df$text=gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("&amp", "", tweets.df$text)
tweets.df$text = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$text)
tweets.df$text = gsub("@\\w+", "", tweets.df$text)
tweets.df$text = gsub("[[:punct:]]", "", tweets.df$text)
tweets.df$text = gsub("[[:digit:]]", "", tweets.df$text)
tweets.df$text = gsub("http\\w+", "", tweets.df$text)
tweets.df$text = gsub("[ \t]{2,}", "", tweets.df$text)
tweets.df$text = gsub("^\\s+|\\s+$", "", tweets.df$text)

tweets.df$text <- iconv(tweets.df$text, "UTF-8", "ASCII", sub="")

emotions <- get_nrc_sentiment(tweets.df$text)
emo_bar = colSums(emotions)
emo_sum = data.frame(count=emo_bar, emotion=names(emo_bar))
emo_sum$emotion = factor(emo_sum$emotion, levels=emo_sum$emotion[order(emo_sum$count, decreasing = TRUE)])



wordcloud_tweet = c(
  paste(tweets.df$text[emotions$anger > 0], collapse=" "),
  paste(tweets.df$text[emotions$anticipation > 0], collapse=" "),
  paste(tweets.df$text[emotions$disgust > 0], collapse=" "),
  paste(tweets.df$text[emotions$fear > 0], collapse=" "),
  paste(tweets.df$text[emotions$joy > 0], collapse=" "),
  paste(tweets.df$text[emotions$negative > 0], collapse=" "),
  paste(tweets.df$text[emotions$positive > 0], collapse=" "),
  paste(tweets.df$text[emotions$sadness > 0], collapse=" "),
  paste(tweets.df$text[emotions$surprise > 0], collapse=" "),
  paste(tweets.df$text[emotions$trust > 0], collapse=" ")
  
)

# create corpus
corpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("english")))
corpus = tm_map(corpus, stemDocument)

# create document term matrix

tdm = TermDocumentMatrix(corpus)

# convert as matrix
tdm = as.matrix(tdm)
tdmnew <- tdm[nchar(rownames(tdm)) < 11,]

dtm_v <- sort(rowSums(tdm),decreasing=TRUE)
dtm_d <- data.frame(word = names(dtm_v),freq=dtm_v)
# Display the top 20 most frequent words
head(dtm_d, 20)
# Plot the most frequent words
par(mfrow=c(1,1))
barplot(dtm_d[2:20,]$freq, las = 2, names.arg = dtm_d[2:20,]$word,
        col ="lightblue", main ="Top frequent words",
        ylab = "Word frequencies")

set.seed(1234)
wordcloud(words = dtm_d[-1,]$word, freq = dtm_d$freq, min.freq = 5,
          max.words=100, random.order=FALSE, rot.per=0.40, 
          colors=brewer.pal(8, "Dark2"))

# Visualize the emotions from NRC sentiments
library(plotly)
p <- plot_ly(emo_sum, x=~emotion, y=~count, type="bar", color=~emotion) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Emotion Type for hashtag")
p                 

# column name binding
colnames(tdm) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'negative', 'positive', 'sadness', 'surprise', 'trust')
colnames(tdmnew) <- colnames(tdm)

#png("comparison_cloud.png")
comparison.cloud(tdmnew, random.order=TRUE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown", "black", "purple"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

mdata<-tdmnew

par(mfrow=c(3,4))
for(i in 1:10){
  vect<-mdata[order(mdata[,i],decreasing = TRUE)[1:10],i]
  
  par(las=2)
  barplot(vect, 
          main=colnames(mdata)[i], 
          horiz=TRUE,
          col=rainbow(10)[i])
}
