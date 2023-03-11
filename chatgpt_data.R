library(RedditExtractoR)

#<<<<<<< HEAD
bard_urls = find_thread_urls(keywords='bard', period='day')

# loop
bard_subreddit_posts = data.frame()
for(subreddit in bard_urls$subreddit[1:25]){
  subreddit_posts = find_thread_urls(subreddit = subreddit, period='day')
  bard_subreddit_posts = rbind(bard_subreddit_posts, subreddit_posts)
}

# save
save(bard_urls, file="data/bard0310.RData")
save(bard_subreddit_posts, file="data/bard_subreddit0310.RData")
#load("bard_subreddit0306.RData")
#=======
chatgpt_urls = find_thread_urls(keywords='chatgpt', period='day')

# loop
chatgpt_subreddit_posts = data.frame()
for(subreddit in chatgpt_urls$subreddit[1:15]){
  subreddit_posts = find_thread_urls(period='day', subreddit = subreddit)
  chatgpt_subreddit_posts = rbind(chatgpt_subreddit_posts, subreddit_posts)
}
dim(chatgpt_subreddit_posts)
table(chatgpt_subreddit_posts$subreddit)

length(unique(chatgpt_subreddit_posts$timestamp))

# save
save(chatgpt_subreddit_posts, file="data/chatgpt_subreddit0310.RData")
save(chatgpt_urls, file="data/chatgpt0310.RData")

#
load("data/chatgpt_subreddit0310.RData")
#>>>>>>> 57b875f23f047dc699ab713a197cd63bf863ba66
#getwd()
library(dplyr)
load(paste0("data/",combined_data$X.bard_subreddit_posts.[1],".RData"))
combined_data2 = combined_data %>% 
  select(title)
write.csv(combined_data2,file ="combined_data.csv",
           fileEncoding = "utf-8")
