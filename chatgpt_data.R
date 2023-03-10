library(RedditExtractoR)

<<<<<<< HEAD
bard_urls = find_thread_urls(keywords='bard', period='day')

# loop
bard_subreddit_posts = data.frame()
for(subreddit in bard_urls$subreddit[1:25]){
  subreddit_posts = find_thread_urls(subreddit = subreddit, period='day')
  bard_subreddit_posts = rbind(bard_subreddit_posts, subreddit_posts)
}

# save
save(bard_urls, file="bard0307.RData")
save(bard_subreddit_posts, file="bard_subreddit0307.RData")
#load("bard_subreddit0306.RData")
=======
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
>>>>>>> 57b875f23f047dc699ab713a197cd63bf863ba66
getwd()
