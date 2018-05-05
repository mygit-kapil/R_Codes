Twitter <- function(twittwerId, tweetcount) {
  ##Below libraries are required for function to run. Install if not installed already##
  library("ROAuth")
  library("twitteR")
  library("syuzhet")
  
  consumer_key <- "E4k9ZMEMdDWelxs9KqJIaJc77"
  consumer_secret <- "nT83nJtZdEMfkfF1IytOTld4D7lBwDnjwgrdmCGvIFhBAuDAMj"
  access_token <- "294513738-GxeFiJZg4kOuvNDfWfHMpPhs7n6RNe8sCuKnC2PH"
  access_secret<- "FFWk9TiuYyzjfZCFvQVL7TGuruq3igbQ2aiE2EPmFR5kc"
  
  ##Twitter connection is established using twitter api##
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
  ##Tweets of the account to be analysed are fetched##
  tweets <- userTimeline(user=twittwerId, n=tweetcount)
  
  n.tweet <- length(tweets)
  tweetsDF <- twListToDF(tweets)
  
  ##Special characters are removed##
  tweetsDF2 <- gsub("http.*","",tweetsDF$text)
  tweetsDF2 <- gsub("https.*","",tweetsDF2)
  tweetsDF2 <- gsub("#.*","",tweetsDF2)
  tweetsDF2 <- gsub("@.*","",tweetsDF2) 
  
  wordDF <- as.vector(tweetsDF2)
  
  ##Sentiment scores are assigned to the tweets##
  sent_value <- get_sentiment(wordDF)
  
  ##Tweets are categorized based on the sentiment scores##
  category_senti <- ifelse(sent_value < 0, "Negative", ifelse(sent_value > 0, "Positive", "Neutral"))
  
  ##Bar plot is created based on the sentiment scores##
  barplot(table(category_senti))
  
}

  