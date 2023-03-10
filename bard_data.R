library(RedditExtractoR)

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
getwd()
