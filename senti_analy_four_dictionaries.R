# Exploratory Analysis
library(lubridate)
library(dplyr)
tweets.df = combined_data 
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
tweets.df$text = gsub("i", "", tweets.df$text)
tweets.df$text = gsub("the", "", tweets.df$text)
tweets.df$text = gsub("it", "", tweets.df$text)
tweets.df$text = gsub("will", "", tweets.df$text)
tweets.df$text = gsub("x", "", tweets.df$text)
tweets.df$text = gsub("you", "", tweets.df$text)
tweets.df$text = gsub("dont", "", tweets.df$text)


tweets.df$title=gsub("&amp", "", tweets.df$title)
tweets.df$title = gsub("&amp", "", tweets.df$title)
tweets.df$title = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df$title)
tweets.df$title = gsub("@\\w+", "", tweets.df$title)
tweets.df$title = gsub("[[:punct:]]", "", tweets.df$title)
tweets.df$title = gsub("[[:digit:]]", "", tweets.df$title)
tweets.df$title = gsub("http\\w+", "", tweets.df$title)
tweets.df$title = gsub("[ \t]{2,}", "", tweets.df$title)
tweets.df$title = gsub("^\\s+|\\s+$", "", tweets.df$title)
tweets.df$title = gsub("i", "", tweets.df$title)
tweets.df$title = gsub("the", "", tweets.df$title)
tweets.df$title = gsub("it", "", tweets.df$title)
tweets.df$title = gsub("will", "", tweets.df$title)
tweets.df$title = gsub("x", "", tweets.df$title)
tweets.df$title = gsub("you", "", tweets.df$title)
tweets.df$title = gsub("dont", "", tweets.df$title)

library(tm)

tweets.df$title <- removeWords(tweets.df$title, stopwords("english"))
tweets.df$text <- removeWords(tweets.df$text, stopwords("english"))
tweets.df$title <- removeWords(tweets.df$title, stopwords("spanish"))
tweets.df$text <- removeWords(tweets.df$text, stopwords("spanish"))

# remove NA in text
tweets.df <- tweets.df[complete.cases(tweets.df$text), ]

tweets.df <- tweets.df[nzchar(tweets.df$text), ]

# split the dataset 
sample = tweets.df %>%
  #filter(!is.na(text)) %>% 
  sample_n(200)

sampleNia = sample[1:66,]
sampleCarlos = sample[67:133,]
sampleLR = sample[134:200,]

write.csv(sampleNia,file="Nia.csv")
write.csv(sampleCarlos,file="Carlos.csv")
write.csv(sampleLR,file="LongRong.csv")

cats_urls = tweets.df
cats_urls$datetime = as_datetime(cats_urls$timestamp)
head(cats_urls[,c('date_utc', 'datetime')])

library(ggplot2)
ggplot(cats_urls, aes(x=datetime)) +
  geom_histogram() +
  xlab('Date') + ylab('Count') + ggtitle('Frequency of Top Posts in Past five days')


patterns = 'chatgpt|bard'
subset_dog_posts = cats_urls[grepl(patterns, cats_urls$title, ignore.case=TRUE) |
                                         grepl(patterns, cats_urls$text, ignore.case=TRUE),]
dim(subset_dog_posts)
head(subset_dog_posts)

# Analysis

## Frequent Terms

library(qdap)
subset_dog_posts$title_text = paste(subset_dog_posts$title, subset_dog_posts$text)

frequent_terms = freq_terms(subset_dog_posts$title_text, 30)
plot(frequent_terms)



########
## Sentiment Analysis

library(SentimentAnalysis)
DictionaryGI$positive[1:100]
DictionaryGI$negative[1:100]

library(quanteda)
data_dictionary_LSD2015$negative[1:50]
data_dictionary_LSD2015$positive[1:50]
data_dictionary_LSD2015$neg_positive[1:50]
data_dictionary_LSD2015$neg_negative[1:50]

subset_dog_posts = subset_dog_posts[1:1000,]
sentiments = analyzeSentiment(iconv(as.character(subset_dog_posts$title_text), to='UTF-8'))
head(sentiments)

tokenized = tokens_lookup(tokens(subset_dog_posts$title_text), dictionary = data_dictionary_LSD2015, exclusive=FALSE)
sentiments$LCpos = sapply(tokenized, function(x)sum(x=='POSITIVE')-sum(x=='NEG_POSITIVE') + sum(x =='NEG_NEGATIVE'))
sentiments$LCneg = sapply(tokenized, function(x)sum(x=='NEGATIVE')-sum(x=='NEG_NEGATIVE') + sum(x=='NEG_POSITIVE'))
sentiments$LC = (sentiments$LCpos - sentiments$LCneg)/sentiments$WordCount

### Another method for sentiment analysis

library(vader)

vader_scores = vader_df(subset_dog_posts$title_text)
sentiments$Vader = vader_scores$compound

library(GGally)  

with(sentiments, 
     ggpairs(data.frame(SentimentGI, SentimentHE, SentimentLM, SentimentQDAP, LC, Vader)))

all_subset_dog_data = cbind(subset_dog_posts, sentiments)
ggplot(all_subset_dog_data, aes(x=as.Date(date_utc), y=Vader)) +
  geom_point() + geom_smooth()

table(all_subset_dog_data$subreddit)
all_subset_dog_data$subreddit
par(mfrow = c(1, 2))
hist(all_subset_dog_data$Vader[all_subset_dog_data$subreddit=='ChatGPT'], main='ChatGPT', xlab='Vader')
hist(all_subset_dog_data$Vader[all_subset_dog_data$subreddit=='bardmains'|all_subset_dog_data$subreddit=='BaldursGate3'|all_subset_dog_data$subreddit=='SteamGameSwap'|all_subset_dog_data$subreddit=='DnD'], main='bardmains', xlab='Vader')

t.test(all_subset_dog_data$Vader[all_subset_dog_data$subreddit=='ChatGPT'],
       all_subset_dog_data$Vader[all_subset_dog_data$subreddit=='bardmains'])
