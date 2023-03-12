load("C:/Users/ccris/Dropbox (University of Michigan)/carlos/Academy/university/GitHub/SurvMeth622_sentiment_analysis/data/combined0310.RData")
library(dplyr)
library(tidyr)
library(ggplot2)
plot = combined_data %>% 
  select(title,date_utc) %>% 
  distinct() %>% 
  separate(date_utc, 
           into = c("year", "month", "day"), sep = "-") %>% 
  group_by(day) %>% 
  mutate(freq = n()) %>% 
  distinct(freq,day) %>% 
  drop_na()

ggplot(plot, aes(x = day, y = freq, fill=day)) +
  geom_bar(stat = "identity")+
  labs(x = "Day", 
       y = "Frequency",
       title = "Barplot")+
  geom_text(aes(label = freq), vjust = -0.5)+
  scale_x_discrete(labels = c( "03/06", "03/07" ,
                               "03/08","03/09","03/10"))+
  theme(legend.position = "none")
  

plot$date_utc = as.Date(plot$date_utc)



dir_path <- "data/"

# get the list of files in the directory
files <- list.files(dir_path)

# create an empty data frame to store the combined data
combined_data <- data.frame()

# create a for loop to load each file and rbind it to the combined data
for (file in files) {
  #file = files[1]
  full_path <- paste(dir_path, file, sep = "")
  load(full_path) # or whatever function is appropriate for your file type
  combined_data <- rbind(combined_data, chatgpt_urls)
}
#save(combined_data,file = "data/combined0310.RData")


library(dplyr)
library(tidyverse)
library(quanteda)
library(topicmodels)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(rtweet)
library(reshape2)
library(ggplot2)
library(textmineR)
library(ggwordcloud)




corpus_sotu_orig <- corpus(data, 
                           docid_field = "doc_id",
                           text_field = "full_text")

corpus_sotu_proc <- tokens(corpus_sotu_orig, 
                           remove_punct = TRUE, 
                           remove_numbers = TRUE, 
                           remove_symbols = TRUE) %>% 
  tokens_tolower() 

lemmaData <- read.csv2("baseform_en.tsv",
                       sep="\t", 
                       header=FALSE, 
                       encoding = "UTF-8", 
                       stringsAsFactors = F)
lemmaData = lemmaData %>% 
  filter(!is.na(V1))
corpus_sotu_proc <-  tokens_replace(corpus_sotu_proc, 
                                    lemmaData$V1, 
                                    lemmaData$V2,
                                    valuetype = "fixed") 


corpus_sotu_proc <- corpus_sotu_proc %>%
  tokens_remove(stopwords("english")) %>%
  tokens_ngrams(1)

cloud =lemmaData %>% 
  group_by(V2) %>% 
  mutate(freq=n()) %>% 
  distinct(freq,V2) %>% 
  filter(freq<40) %>% 
  arrange(desc(freq))
cloud = cloud[1:200,]
wordcloud(words = cloud$V2, freq = cloud$freq, min.freq = 1,
          max.words=10, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

DTM <- dfm(corpus_sotu_proc)
minimumFrequency <- 10
DTM <- dfm_trim(DTM, 
                min_docfreq = minimumFrequency,
                max_docfreq = 100)
DTM  <- dfm_select(DTM, 
                   pattern = "[a-z]", 
                   valuetype = "regex", 
                   selection = 'keep')
colnames(DTM) <- stringi::stri_replace_all_regex(colnames(DTM), 
                                                 "[^_a-z]","")

DTM <- dfm_compress(DTM, "features")
sel_idx <- rowSums(DTM) > 0
DTM <- DTM[sel_idx, ]
textdata <- data[sel_idx, ]
dim(DTM)

model <- FitLdaModel(dtm = DTM,
                     k = 20,
                     iterations = 200, 
                     burnin = 180,
                     alpha = 0.1,
                     beta = 0.05,
                     optimize_alpha = TRUE,
                     calc_likelihood = TRUE,
                     calc_coherence = TRUE,
                     calc_r2 = TRUE,
                     cpus = 2)
model2=as.data.frame(model$log_likelihood)
ggplot(model2,aes(x=iteration,y=log_likelihood))+
  geom_line()+
  geom_vline(xintercept = 10, col="red")+
  labs(title = "k = 10")

K <- 10
set.seed(1)
topicModel <- LDA(DTM,
                  K,
                  method="Gibbs",
                  control=list(iter = 500,
                               verbose = 25))
tmResult <- modeltools::posterior(topicModel)
beta <- tmResult$terms

theta <- tmResult$topics
top5termsPerTopic <- terms(topicModel,
                           5)
topicNames <- apply(top5termsPerTopic,
                    2,
                    paste,
                    collapse=" ")
topicProportions <- colSums(theta) / nrow(DTM)
names(topicProportions) <- topicNames
topicModel2 <- LDA(DTM, 
                   K, 
                   method="Gibbs", 
                   control=list(iter = 500, 
                                verbose = 25, 
                                alpha = 0.2))
tmResult <- modeltools::posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicProportions <- colSums(theta) / nrow(DTM)
names(topicProportions) <- topicNames      
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")
exampleIds <- c(2, 100, 200)
N <- length(exampleIds)
topicProportionExamples <- as.tibble(theta) %>%
  slice(exampleIds)
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), 
                           document = factor(1:N)), 
                     variable.name = "topic", 
                     id.vars = "document")  
ggplot(data = vizDataFrame, 
       aes(topic, value, 
           fill = document), 
       ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, 
             ncol = N)

# load(paste0("data/",combined_data$X.bard_subreddit_posts.[1]))
# check the structure of the combined data frame
str(combined_data)
