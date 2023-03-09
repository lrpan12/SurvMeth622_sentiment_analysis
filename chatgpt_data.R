library(RedditExtractoR)

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
save(chatgpt_subreddit_posts, file="chatgpt_subreddit0308.RData")
load("chatgpt_subreddit0308.RData")
getwd()
