####################################
### Social Media Analytics  ########
####################################

########################################
### Basic Text processing  #
########################################


# Get data into R; these originate from Facebook
# replace this with your own path
load("C:/Users/kkusterer/Documents/GitHub/SMA/Session2/oxfamComments.Rdata")

# load some packages that we will use
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

###
# Preprocessing with tidytext
###

# 1. Remove punctuation and numbers with regular expressions
oxfamComments <- mutate(oxfamComments, message = gsub(x = message, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
  
# 2. Tokenization (+ going to lowercase)
oxfamTokenized <- oxfamComments %>% unnest_tokens(output = "word", # how should the new column be named?
                                                    input = message, # where can we find the text? 
                                                    token = "words", # which tokenization scheme should we follow?
                                                    drop=FALSE,to_lower=TRUE) # drop=FALSE specifies that we want to keep our text; to_lower puts everyting to lowercase

# 3. Remove some other elements such as # and @ signs if they might occur
oxfamTokenized <- filter(oxfamTokenized, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags


# 4. Spelling correction (certainly not always done)

# General approach: you have a dictionary of the language you are investigating
# Next, you check for each word whether you find it in the dictionary
# if you do not find the word in the dictionary, use the closest word/suggestion (based on a similarity measure)
# if we do not find a word that is close enough, keep the original word

# typically, there are not a lot of standard implemented approaches and it takes quite some time
# You can use the following function to perform spelling correction 
# see https://gregrs-uk.github.io/2018-02-03/tidytext-walkthrough-correcting-spellings-reproducible-word-clouds/


correct_spelling <- function(input) {
  output <- case_when(
    # any manual corrections
    input == 'license' ~ 'licence',
    # check and (if required) correct spelling
    !hunspell_check(input, dictionary('en_GB')) ~
      hunspell_suggest(input, dictionary('en_GB')) %>%
      # get first suggestion, or NA if suggestions list is empty
      map(1, .default = NA) %>%
      unlist(),
    TRUE ~ input # if word is correct
  )
  # if input incorrectly spelled but no suggestions, return input word
  ifelse(is.na(output), input, output)
}

# now, apply the function above to each word in our dataset.
# create a new variable that contains the 'corrected' word
oxfamTokenized <- oxfamTokenized %>%  mutate(suggestion = correct_spelling(word))

# 5. remove stopwords

oxfamTokenized <- oxfamTokenized %>% anti_join(get_stopwords()) # note that I continue with the 'uncorrected' words here
# this strongly reduces the number of words

# 6. Stemming
oxfamTokenized <- oxfamTokenized %>% mutate(word = wordStem(word)) 
  # note that stemming may not always be appropriate e.g. when performing sentiment analysis

# 7. Create the document-term matrix

 # first, we need to get the number of times a word occurred in each document (or status, tweet)

oxfamTokenized <- oxfamTokenized %>% count(id,word)
head(oxfamTokenized)
  # then, we could perform weighting (e.g., tfidf) using the bind_tf_idf(word,id,n) function
  # however, we will integreate this directly when making the document term matrix:

oxfamDTM <- oxfamTokenized %>% cast_dtm(id,word,n,weighting = tm::weightTfIdf)

# let's inspect this matrix
# we can see that it is a special R format, a DocumenttermMatrix object, which is a special form of a sparse matrix
# we can convert this to a normal matrix as as.matrix(oxfamDTM)
# we can also convert it back to tidy format as tidy(oxfamDTM)

# This is a very sparse matrix
# we can reduce sparseness by removing the most sparse terms:
oxfamDTMDense <- removeSparseTerms(oxfamDTM,0.7)

  # you can see we only kept 1 term. Thus, the number of 0.7 is certainly too low, because we threw away almost all information


######
### 8.  inspect our text
######
# Now let us explore this. Let's assume we want to quickly see what the texts are talking about. 


# A natural next step is to have a quick look at our text
# We can do this both by tables and figures
# note that for this purpose, the tibble format is most useful (oxfamTokenized)

# 1. we can look at associations/correlations between words (this is with the dtm):
findAssocs(oxfamDTM, terms = "oxfam", corlimit = 0.1)


# 2. investigate the most frequent terms

oxfamFreq <- oxfamTokenized %>% group_by(word) %>% # for this, we need to have the sum over all documents
                  summarize(freq = n()) %>%
                  arrange(-freq)                  # arrange = order; from most frequent term to lowest frequent
head(oxfamFreq)


# 3. We can also build a wordcloud in order to give this insight visually
# what is the basis for a wordcloud? Term frequencies

# Load the package wordcloud
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}

# 1. Word cloud based on the original text
#
# use the termFreq of the tm package
# This also uses a tokenizer inside
tf <- termFreq(oxfamComments$message)
wordcloud(names(tf),tf,
          max.words=40,
          scale=c(3,1))

# 2. Word cloud based on the tibble and all text pre-processing

#create word cloud
wordcloud(oxfamFreq$word, oxfamFreq$freq,
          max.words=40,
          scale=c(3,1))



###package if required###
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")

###Twitter token###
twitter_token <- create_token(
  app = "Social Media Analytics01",
  consumer_key = "zv7vpFpTfmS0VFq4Im1gLAkbS",
  consumer_secret = "rsjGeQya4hPGjGiFrQDj7h0XOG5eMLGs1fDD2Rf2TZhDGvw1e7",
  access_token = "1218632669661220864-J2IF2hzDd1ZfIDaWy0j6TinUg4lMxM",
  access_secret = "HwA9IzQJCalHhGX5oIrV3DF91MYKNQXkADhORyZSHfeEo",
  set_renv=FALSE)



rtweets <- rtweet::search_tweets(q = "#LiverpoolFC",
                                 n = 500,include_rts = FALSE, token = twitter_token)

tf <- termFreq(rtweets$text)

wordcloud(names(tf),tf,
          max.words=40,
          scale=c(3,1), ordered.colors = TRUE)


####Using TM Packages #####

#############
#############
## 2. Do this with tm package
#############
#############
# this will work a little different, since everything starts from a Corpus, so we first need to define this
# Then we can use transformer functions for the preprocessing steps
# these can be predefined functions (e.g. for stemming) or you can define them yourself
# again, we need to tackle spelling correction in a special way (getting text out of the corpus, applying spelling correction, and putting it back into the corpus)
# finally, we can create the document term matrix

######
######
### Step 1: Read in the data into a corpus
######
######


# load text packages
# The tm package expects the SnowballC and slam packages to be installed.

for (i in c('SnowballC','slam','tm','RWeka','Matrix')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

# first, order the comments by date
oxfamComments[,1] <- as.POSIXct(oxfamComments[,1], format="%Y-%m-%dT%H:%M:%S%z",tz="UTC")
attributes(oxfamComments[,1])$tzone <- "CET"
oxfamComments <- oxfamComments[order(Comments[,1]),]
# First we will delete non-recognizable characters, otherwise the tm package will get in trouble later. 
# Note that this can also delete some emoticons
CommentsText <- sapply(oxfamComments[,2],function(x) iconv(x, 'utf8', 'ascii',""))

comments <- VCorpus(VectorSource(CommentsText))

# inspect the corpus

comments[[1]]
as.character(comments[[1]])

# note that we can also tidy this corpus back to a dataframe:
# this also gives us some metadata that is stored in the corpus
df_comment <- tidy(comments)

######
######
### Step 2: Pre-processing
######
######

# Which transformer functions are available? 
getTransformations()


# 1. Existing transformer functions
# We can apply these transformer functions to the entire corpus using a 'mapper', tm_map

comments <- tm_map(comments, removePunctuation)
comments <- tm_map(comments, removeNumbers)
comments <- tm_map(comments, stripWhitespace)

# 2. Create new transformation functions
# we can create new tranformation functions using the content_transformer wrapper

comments <- tm_map(comments,content_transformer(tolower))

# note that in tweets for instance, you may also want to remove hashtags, links, ... from the tweet
# Typically, this will involve some regular expressions
# e.g, we could remove @persons via a function like this: 
# some_txt = gsub("@\\w+", "", some_txt)
# Note that this can also be useful for further analysis (e.g. an analysis of hashtags used)
# Application with content_transformer:
gsubtransfo <- content_transformer(function(x,from, to) gsub(from, to, x))
comments <- tm_map(comments, gsubtransfo, "@\\w+",  "")

# you'll notice that this does not work
# Why not? How to solve it? 

#3.  Do a spelling check
# there is no clear and nice spelling check available
# this is also a very difficult task to achieve
#Based on Rasmus Baath, research blog,
#http://www.sumsar.net/blog/2014/12/peter-norvigs-spell-checker-in-two-lines-of-r/

#Download a list of words sorted by frequency of
#appearance in natural language
# Why is this important?
load("C:/Users/m.meire/OneDrive - IESEG/Courses/MBD - Social Media Analytics/2018/wordListSpelling.RData")


#Function to correct misspelled words
correct <- function(word) {
  # How dissimilar is this word from all words in the wordlist?
  edit_dist <- adist(word, wordlist)
  # Is there a word that reasonably similar?
  # If yes, which ones?
  # If no, append the original word to
  # Select the first result (because wordlist is sorted
  # from most common to least common)
  c(wordlist[edit_dist <= min(edit_dist,2)],word)[1]
}

# Note that the above function is for words, not for documents/sentences. 
# However, in the corpus, we look at documents
# So we have two options: 
#   - adapt the function above to work on sentences (e.g., by performing tokenization within)
#   - Work directly on the words. 
# Here, we will show the second approach, which means we will take words out of the corpus first

commentsUnlisted <- unlist(sapply(comments,'[',"content"))

comments_spell_checked <- character(length(commentsUnlisted))

for (i in 1: length(commentsUnlisted)){
  words <- unlist(strsplit(commentsUnlisted[i],' '))
  words <- as.character(sapply(words,correct))
  
  #Concatenate back to a string
  comments_spell_checked[i] <-
    paste(words, collapse=" ")
}

comments <- VCorpus(VectorSource(comments_spell_checked))

# 4. Remove stopwords
#remove stopwords
forremoval <- stopwords('english')
comments <- tm_map(comments, removeWords,c(forremoval[!forremoval %in% c("no","not","nor")])) 
# note that you can also provide or add your own words

# 5. Stemming

comments <- tm_map(comments,stemDocument)


######
######
### Step 3:  Create the term-document matrix
######
######



# Make the document-term matrix. 
# Next, we use this function in the control argument of the DocumentTermMatrix


dtmComments <- DocumentTermMatrix(comments, control = list( weighting =function(x) weightTf(x)))
View(as.matrix(dtmComments[1:6,1:6]))

# Have a look at the tm package documentation. What other controls can we add? 

# remove terms that are too short (e.g., a , an, the, ...)
# remove sparse terms: they hardly occur in the dataset
dtmComments <- DocumentTermMatrix(comments, control = list( wordlengths=c(2,Inf),
                                                            weighting =function(x) weightTf(x)))

#Remove terms with too much sparsity. How much sparsity do we allow?
#Let's try different values for the sparse parameter
#Allow that 70% of the documents do not use a word
#At least 30% of the documents need to use the word for the
#word to stay in the dtm.
dtmCommentsDense <- removeSparseTerms(dtmComments,0.7)



