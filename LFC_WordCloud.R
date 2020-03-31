###load packages needed###
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

##package if required###
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")

###Twitter token###
twitter_token <- create_token(
  app = "Social Media Analytics01",
  consumer_key = "zv7vpFpTfmS0VFq4Im1gLAkbS",
  consumer_secret = "rsjGeQya4hPGjGiFrQDj7h0XOG5eMLGs1fDD2Rf2TZhDGvw1e7",
  access_token = "1218632669661220864-LGwovlK0dZq6MOQTguZmUyV1RLxgc5",
  access_secret = "wi5NoCsYSmOUYIk9T6IDPiGJAYCBgp4iu7H1Tc6nPjFoa",
  set_renv=FALSE)


###To search tweets specifically conataining LFC###
LFCtweets <- rtweet::search_tweets(q = "LiverpoolFC",
                                 n = 100,include_rts = FALSE, token = twitter_token)


# Remove punctuation and numbers with regular expressions
LFcComments <- mutate(LFCtweets, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

?gsub

# This compares for the first letter of a token# omit hashtags
LFCBigramDTM <- LFcComments  %>% cast_dtm(id,bigram,n)

# Tokenization - Univiriate 
#LFcComments <- LFcComments %>% unnest_tokens(output ="word",
#                                            input = text,
#                                           token = "words", drop = FALSE, to_lower = TRUE)

##Create LFC Base ###
LFCBase <- select(LFcComments, user_id, text)
LFCBase <- mutate(LFCBase, ID = row_number())
LFCBase <- rename(LFCBase, tweet = text)

# Tokenization - Biviriate (+ going to lowercase)
LFCBigramCount <- LFCBase %>% unnest_tokens(output = "bigram",
                                            input = tweet,
                                            token = "ngrams",n=2, drop=FALSE) %>%
  count(ID,bigram)

# Remove some other elements such as # and @ signs if they might occur
LFCTokenized <- filter(LFCBase, substr(tweet, 1, 2) != '#', 
                       substr(tweet, 1, 2) != '@')

LFCTokenized <- filter(LFCBase, gsub("http.*","",tweet))

?gsub

LFCBigramCount <- LFCBigramCount %>% group_by(bigram) %>% summarize(freq = n())
wordcloud(LFCBigramCount$bigram,LFCBigramCount$freq,max.words = 40)






###To Select sepcifcly the text###
tf <- termFreq(LFCBase$tweet)
###To prodcue Word cloud###
wordcloud(names(tf),tf,
          max.words=40,
          scale=c(3,1), ordered.colors = TRUE)

