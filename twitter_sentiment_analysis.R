library(twitteR)
library(ROAuth)
library(plyr)
library(dplyr)
library(ggplot2)
library(httr)
library(wordcloud)
library(syuzhet)
library(tm)
library(stringr)
library(RCurl)
library(openssl)
library(httpuv)
library(base64enc)
library(tidyverse)
library(tidytext)
library(forcats)
library(scales)
library(reshape2)

#end points for OAuth Open authorization
api_key <- "XXXX"
api_secret <- "XXXX"
access_token <- "XXXX"
access_secret <- "XXXX"

#Setup the OAuth
setup_twitter_oauth(api_key, api_secret, access_token, access_secret)

# search for tweets of different service providers
tweets = searchTwitter(searchString = "Jio", n=5000, lang = "en")
#tweets #show the tweets fetched
#length(tweets) #shows the no. of tweets fetched

#Convert it to Dataframe to save in a file
tweets_df = ldply(tweets,function(t)t$toDataFrame())
#View(tweets_df)

#Save to a CSV file
write.csv(tweets_df, "F:\\College\\Course\2 Semester\\SMA\\workspace_R\\airtelTweets.csv")

#Step 3 - Data Cleaning
text = sapply(tweets, function(x)x$getText())
#View(text)
#text

#Remove all the @
text = gsub("@\\w+", "", text)
#text

# Cleaning steps, remove people name, RT etc.
some_txt1 = gsub("(RT)((?:\\b\\w*@\\w+)+)","",text)
some_txt1 = gsub("RT : ","",text)
#some_txt1

#cleaning 2 remove hyperlinks
some_txt2 = gsub("http[^[:blank:]]+","", some_txt1)
#some_txt2

#cleaning 3 remove people names
some_txt3 = gsub("@\\w+","",some_txt2)
#some_txt3

#Cleaning 4 remove punctuations
some_txt4 = gsub("[[:punct:]]","",some_txt3)
#some_txt4

#Cleaning 5 remove non alpha numeric words
some_txt5 = gsub("[^[:alnum:]],[^[:space:]]","",some_txt4)
#some_txt5

#remove emoji
some_txt_remove_emoji = gsub("[^\x01-\x7F]", "", some_txt5)
#some_txt_remove_emoji

#remove newline
some_txt_remove_newline = gsub("[\r\n]", "", some_txt_remove_emoji)
#some_txt_remove_newline
some_txt = some_txt_remove_newline

#Exporting to Excel
write.csv(some_txt, "F:\\College\\Course\2 Semester\\SMA\\workspace_R\\vodaFone_tweets.csv")
getwd() #get working directory
#setwd() #set working directory

#Cleaning for Word Corpus
some_txt6 = Corpus(VectorSource(some_txt5))
some_txt6 [[1]]$content #shows 1st tweet content

some_txt6 = tm_map(some_txt6, content_transformer(tolower))
some_txt6 = tm_map(some_txt6,removeWords,stopwords("english"))
#some_txt6
some_txt6 = tm_map(some_txt6, stripWhitespace)

par(mar=c(1,1,1,1))

#Building word cloud
pal = brewer.pal(8,"Dark2")
pal

#RUN FROM DATAFRAME SOME_TXT5 ONLY AND NOT WITH CORPUS
#sentiment analysis code

# NRC
mysentiment <- get_nrc_sentiment(some_txt5)
mysentiment
SentimentScores <- data.frame(id=c(1:10), total = colSums(mysentiment[,]))
SentimentScores
sentScoreNegative <- SentimentScores$total[1]+SentimentScores$total[3]+SentimentScores$total[4]+SentimentScores$total[6]+SentimentScores$total[9]
sentScoreNegative
sentScorePositive <- SentimentScores$total[2]+SentimentScores$total[5]+SentimentScores$total[7]+SentimentScores$total[8]+SentimentScores$total[10]
sentScorePositive

names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")

# NRC - wordcloud
wordcloud(some_txt6, min.freq = 20,max.words = Inf, width = 2000, height = 1000,
          random.order = FALSE, colors = pal)

#########################################################################
# BING

my_sent_bing <- get_sentiments(some_txt5, "bing")
my_sent_bing
get_sentiments(some_txt5, lexicon="bing") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  count(sentiment)
bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)