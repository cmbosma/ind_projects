## Twitter Political Sentiment Analysis
## Colin M. Bosma

##------------------------------------------------------------------------------

## WEBSITES

 # Tutorial for sentiment Analysis
browseURL("https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment")

# Github repository with twitter analysis recipes
browseURL("https://github.com/SMAPPNYU/smappR#a-installing-r-packages")

# Wes Anderson color palette
browseURL("https://github.com/karthik/wesanderson")

## SET WORKING DIRECTORY

getwd()
setwd("PATH") 

## LOADING PACKAGES

library(twitteR)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer) # color palette
library(wesanderson) # color palette
library(qdap) # used for documentation on polarity function


# If needed, install archived version of sentiment package
#require(devtools)
#install_url("https://cran.r-project.org/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz") 
# First need the Rstem package
#install_url("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz")
require(sentiment)


## Setting up the `twitteR` R package for text analytics

# browseURL("https://apps.twitter.com/app/12575556")

consumer_key <- "5hxjjnUpUz3Gw76i5In1LfWc6"
consumer_secret <- "24RMzUZbYmQ7hl07jmx4Tfmm0CgymDvTQgvK6uBaqKrGXZBsH8"
access_token <- "1939812020-ruJMGuB3PC948zboNYOXrE8bZg0ryrSSuo9Ulxy"
access_secret <- "Uz2ptkE1Pe0OvS8A3RmszhF7eq7K4ZNdVtFcbTRtoWfls"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret) # authentication

# Cache locally
# Yes Run if needed

searchTwitter("starbucks") # test authentication by searching for Colin's tweets!

## SCRAPING TWEETS

# Let's scrape some HIllary Clinton tweets with the hashtag `#imwithher`

some_tweets = searchTwitter("#hillary2016", n=3000, lang="en") # wait for R to process

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


## PREPARING THE TEXT FOR SENTIMENT ANALYSIS

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function
try.error = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
   y = tolower(x)
   # result
   return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

## SENTIMENT ANALYSIS

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown" # wait for R to process

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes") # wait for processing
# get polarity best fit
polarity = class_pol[,4]

## CREATING A DATA FRAME WITH RESULTS AND GENERAL STATISTICS

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
  emotion <- factor(emotion, levels=names(sort(table(emotion),
  decreasing=TRUE))))

# Print data frame

View(sent_df)

## PLOTTING THE RESULTS!

# Palette URL
# browseURL("https://cran.rstudio.com/web/packages/RColorBrewer/RColorBrewer.pdf")

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Blues") +
  ggtitle("Sentiment Analysis of Tweets about Hillary Clinton's Campaign\n(classification by emotion)") +
  labs(x="Emotion Categories", y="Number of Tweets") +
  theme(plot.title = element_text(face="bold", size=12))

# plot distribution of polarity (with Wes Anderson Palettes)
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_manual(values = wes_palette("Darjeeling2")) +
  ggtitle("Sentiment Analysis of Tweets about Hillary Clinton's Campaign\n(classification by polarity)") +
  labs(x="Polarity Categories", y="Number of Tweets") +
  theme(plot.title = element_text(face="bold", size=12))


## SEPARATING TEXT BY EMOTIONAL VALENCE AND VISUALIZING WORDS WITH A COMPARISON CLOUD

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = some_txt[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)

?comparison.cloud


# Exporting data frame

## Exporting data to .csv for hypothesis testing

setwd("/Users/colinbosma/Desktop") # Set working directory to desktop
getwd() # checking working directory


write.csv(sent_df, file = "twitter-sentiment-analysis.csv",
 row.names = FALSE, na = "")

# TRUMP TWEET ANALYSIS  --------------------------------------------------------

## SCRAPING TWEETS

# Let's scrape some Donald Trump tweets with the hashtag `#imwithher`

some_tweets = searchTwitter("#trump2016", n=100, lang="en") # wait processing

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


## PREPARING THE TEXT FOR SENTIMENT ANALYSIS

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function
try.error = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
   y = tolower(x)
   # result
   return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

## SENTIMENT ANALYSIS

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown" # wait for R to process

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes") # wait for processing
# get polarity best fit
polarity = class_pol[,4]

## CREATING A DATA FRAME WITH RESULTS AND GENERAL STATISTICS

# data frame with results
sent_df_d = data.frame(text=some_txt, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df_d = within(sent_df,
  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Print data frame

View(sent_df_d)

## PLOTTING THE RESULTS!

# Palette URL
# browseURL("https://cran.rstudio.com/web/packages/RColorBrewer/RColorBrewer.pdf")

# plot distribution of emotions
ggplot(sent_df_d, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("Sentiment Analysis of Tweets about the Donald Trump Campaign\n(classification by emotion)") +
  labs(x="emotion categories", y="number of tweets") +
  theme(plot.title = element_text(face="bold", size=12))

# plot distribution of polarity (with Wes Anderson Palettes)
ggplot(sent_df_d, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_manual(values = wes_palette("Zissou")) +
  ggtitle("Sentiment Analysis of Tweets about Donald Trump\n(classification by polarity)") +
  labs(x="polarity categories", y="number of tweets") +
  theme(plot.title = element_text(face="bold", size=12))


## SEPARATING TEXT BY EMOTIONAL VALENCE AND VISUALIZING WORDS WITH A COMPARISON CLOUD

# separating text by emotion
emos = levels(factor(sent_df_d$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = some_txt[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)


## May not be needed!
## Exporting data to .csv for hypothesis testing

sent_data <- c(sent_df, sent_df_d)

write.csv(sent_data, file = "twitter-sentiment-analysis.csv", row.names = FALSE,
	na = "")






## ----------------------TRUMP TWEET ANALYSIS  ---------------------------------

## SCRAPING TWEETS

# Let's scrape some Donald Trump tweets with the hashtag `#imwithher`

some_tweets = searchTwitter("#trump2016", n=3000, lang="en") # wait for R to process

# get the text
some_txt = sapply(some_tweets, function(x) x$getText())


## PREPARING THE TEXT FOR SENTIMENT ANALYSIS

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)

# define "tolower error handling" function
try.error = function(x)
{
   # create missing value
   y = NA
   # tryCatch error
   try_error = tryCatch(tolower(x), error=function(e) e)
   # if not an error
   if (!inherits(try_error, "error"))
   y = tolower(x)
   # result
   return(y)
}
# lower case using try.error with sapply
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

## SENTIMENT ANALYSIS

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown" # wait for R to process

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes") # wait for R to process
# get polarity best fit
polarity = class_pol[,4]

## CREATING A DATA FRAME WITH RESULTS AND GENERAL STATISTICS

# data frame with results
sent_df_d = data.frame(text=some_txt, emotion=emotion,
polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df_d = within(sent_df_d,
  emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# Print data frame

View(sent_df_d)

## PLOTTING THE RESULTS!

# Palette URL
# browseURL("https://cran.rstudio.com/web/packages/RColorBrewer/RColorBrewer.pdf")

# plot distribution of emotions
ggplot(sent_df_d, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Reds") +
  ggtitle("Sentiment Analysis of Tweets about the Donald Trump's Campaign\n(classification by emotion)") +
  labs(x="Emotion Categories", y="Number of Tweets") +
  theme(plot.title = element_text(face="bold", size=12))

# plot distribution of polarity (with Wes Anderson Palettes)
ggplot(sent_df_d, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_manual(values = wes_palette("Zissou")) +
  ggtitle("Sentiment Analysis of Tweets about Donald Trump's Campaign\n(classification by polarity)") +
  labs(x="Polarity Categories", y="Number of Tweets") +
  theme(plot.title = element_text(face="bold", size=12))


## SEPARATING TEXT BY EMOTIONAL VALENCE AND VISUALIZING WORDS WITH A COMPARISON CLOUD

# separating text by emotion
emos = levels(factor(sent_df_d$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
   tmp = some_txt[emotion == emos[i]]
   emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Accent"),
   scale = c(3,.5), random.order = FALSE, title.size = 1.5)





