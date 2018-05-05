account <- "mipaltan"
count <- 100
source("E:/Data Science/R/RPrograms/Twitter Sentiment/TwitterAnalysis.R")
tweets <- Twitter(account, count)
#source("E:/Data Science/R/RPrograms/Twitter Sentiment/Sentiment.R")
#sentiments <- GetSentiment(tweets)

#barplot(table(sentiments))