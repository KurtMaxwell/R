
####Read in Data###
Zappos<-read_delim("C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/ZappposBaseComplete.csv", delim = ",")
Zappos_Tokenized<-read_delim("C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/Zappos_Tokenized.csv", delim = ",")

###load packages needed###
for (i in c('SnowballC','slam','tm','Matrix','tidytext','dplyr','hunspell','purrr','plyr','readr','tidyr','textdata','qdap')){
  if (!require(i, character.only=TRUE)) install.packages(i, repos = "http://cran.us.r-project.org")
  require(i, character.only=TRUE)
}

##package if required###
if(!require("rtweet")) install.packages("rtweet"); library("rtweet")


###Create Twitter token###
twitter_token <- create_token(
  app = "Social Media Analytics01",
  consumer_key = "zv7vpFpTfmS0VFq4Im1gLAkbS",
  consumer_secret = "rsjGeQya4hPGjGiFrQDj7h0XOG5eMLGs1fDD2Rf2TZhDGvw1e7",
  access_token = "1218632669661220864-LGwovlK0dZq6MOQTguZmUyV1RLxgc5",
  access_secret = "wi5NoCsYSmOUYIk9T6IDPiGJAYCBgp4iu7H1Tc6nPjFoa",
  set_renv=FALSE)




###Search the last month, multiple calls combine into one final table ZapposComplete30days###
#ZapposTweets <-search_fullarchive(q = "Zappos OR \"-filter:retweets\"",
 #                                   n =2000, 
  #                                  fromDate ="201901012345",
   #                                 toDate ="201901312345",
    #                                env_name = "FullArch",
     #                               token = twitter_token)  

#ZapposTweets <-search_fullarchive(q = "Zappos OR \"-filter:retweets\"",
 #                                 n =2000, 
  #                                fromDate ="201902012345",
   #                               toDate ="201902282345",
    #                              env_name = "FullArch",
     #                             token = twitter_token)

#Zappos30dayTweets <- search_30day(q = "Zappos OR \"-filter:retwteets\"",
 #                                 n = 100 ,
  #                                env_name = "First",
   #                               token = twitter_token)

#Zappos30dayTweets2ndCollect<- search_30day(q ="Zappos OR \"-filter:retwteets\"",
 #                                n = 24900,
  #                               fromDate ="201912301220",
   #                              toDate = "202001272002",
    #                             env_name = "First",
     #                            token = twitter_token)

#Zappos30dayTweets3rdCollect <- search_30day(q ="Zappos OR \"-filter:retwteets\"",
 #                                         n = 20116,
  #                                        fromDate ="201912301220",
   #                                       toDate ="202001162144",
    #                                      env_name = "First",
     #                                     token = twitter_token)

#Zappos30dayTweets4thCollect <- search_30day(q ="Zappos OR \"-filter:retwteets\"",
 #                                           n = 20116,
  #                                          fromDate ="201912291300",
   #                                         toDate ="201912301345",
    #                                        env_name = "First",
     #                                       token = twitter_token)


###combine all tables into one table###
#ZapposComplete30day <- bind_rows(Zappos30dayTweets,Zappos30dayTweets2ndCollect,Zappos30dayTweets3rdCollect,Zappos30dayTweets4thCollect)


###Create a dataframe from desired variables###
#ZapposBase <- select(ZapposComplete30day, user_id, status_id, created_at, screen_name, text, source, reply_to_status_id, reply_to_user_id, lang, is_retweet)

###Write the datafrme to a csv###
write.csv(ZapposBase,file = "C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/ZappposBaseComplete.csv", row.names = TRUE)

###Table preporcessing ###
###remove punctuation from tweets###
ZapposBase_clean <- mutate(ZapposBase, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))


##Tokenization (+ going to lowercase)##
Zappos_Tokenized <- ZapposBase_clean %>% unnest_tokens(output = "word", # how should the new column be named?
                                                  input = text, # where can we find the text? 
                                                  token = "words", # which tokenization scheme should we follow?
                                                  drop=FALSE,to_lower=TRUE)
###To keep pnly English type of lang###
Zappos_Tokenized <- filter(Zappos_Tokenized, lang =="en")
unique(Zappos_Tokenized$lang)

#Remove some other elements such as # and @ signs if they might occur
ZapposBase_Tokenized <- filter(Zappos_Tokenized, substr(word, 1, 1) != '#', 
                         substr(word, 1, 1) != '@') # This compares for the first letter of a token# omit hashtags


###Correct the spelling in the tweets###
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

###Apply spelling correct function to the Data###
Zappos_Tokenized <- Zappos_Tokenized %>%mutate(suggestion = correct_spelling(word))

#ave table to CSV##
write.csv(Zappos_Tokenized, "C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/Zappos_Tokenized.csv", row.names = TRUE)

## Remove stopwords ##
Zappos_Tokenized <- Zappos_Tokenized %>% anti_join(get_stopwords(language = "en")) # note that I continue with the 'uncorrected' words here
# this strongly reduces the number of words

##Remove the word Zappos###
Zappos_Tokenized_wd <- filter(Zappos_Tokenized, word != "zappos")

###Stem words for colud###
Zappos_Tokenized_wd <- Zappos_Tokenized_wd %>% mutate(word_stem = wordStem(word)) 

###Count the occurance of words before creating a wordcloud###
Zappos_Tokenized_wd <- Zappos_Tokenized_wd %>% group_by(status_id) %>%
                                              count(c("word"))%>%
                                              as.data.frame(Zappos_Tokenized_wd)


###Create a dtm###
Zappos_dtm <- Zappos_Tokenized_wd %>% cast_dtm(word,freq,weighting = tm::weightTfIdf)                                         

###Reduce sparsity###
Zappos_dense <- removeSparseTerms(Zappos_dtm,0.999)

###Finding word associations###
findAssocs(Zappos_dense, terms = "shoes", corlimit = 0.1)

###For WordCloud###
ZapposFreq <- Zappos_Tokenized %>% group_by(word) %>%
              summarize(freq = count(word))

# Load the package wordcloud
if (!require("wordcloud")) {
  install.packages("wordcloud",repos="https://cran.rstudio.com/",
                   quiet=TRUE)
  require("wordcloud")
}

#create word cloud
wordcloud(ZapposFreq$freq$x, ZapposFreq$freq$freq,
          max.words=45,
          scale=c(3,1))

###########Using the dictionary to get a sentiment score for the words #########

###Using bing####
# Get bing dictonary and count words
bing <- get_sentiments("bing")

# join dictionary and words
z_bing_sent <- inner_join(Zappos_Tokenized_wd,get_sentiments("bing"))


###Using AFFIN ###
AFFIN <- get_sentiments("afinn")

z_afinn_sent <- inner_join(Zappos_Tokenized_wd,get_sentiments("afinn"))
z_afinn_sent <- select(z_afinn_sent, ID, word, freq, value)
z_afinn_sent <- mutate(z_afinn_sent, ID = row_number(word))
z_afinn_sent <- as.data.frame(z_afinn_sent)

###Write to CSV###
ZapposTweets <- select(ZapposTweets, user_id, status_id, created_at, screen_name, text, source, reply_to_status_id, reply_to_user_id, lang, is_retweet)
write.csv(ZapposTweets, file = "C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/ZapposTweets_Fullarchive.csv", row.names = TRUE )


##########Begin of the machine learning model process#############


####Removing the which conatin no sentiment#####
SentimentZappos <- z_afinn_sent[which(z_afinn_sent$value != 0),]

####Count the most frequnetly occuring terms###
count_top <- z_afinn_sent %>% top_n(n= 20, wt = freq)
                              arrange(freq, desc = TRUE)

count_top <-  count_top %>% select(word, freq) %>%
                            as.data.frame()
count_top$freq <- as.numeric(count_top$freq)

###Clean table further for labeling###
Zappos_labeling <- Zappos %>% filter(lang =='en')%>%
                              filter(gsub("#", "",text) != "#")%>%
                              filter(gsub("@*", "",text) != "@")

write.csv(Zappos_labeling, file= "C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/Zappos_labeling.csv")

###Read in table post labeling ###
Zappos_ML <- read_delim("C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/Zappos_labeling.csv", delim = ",")

###Prepare dataframe to be used in tm for ML####
Zappos_ML <- as.data.frame(Zappos_ML)
Zappos_ML <- mutate(Zappos_ML, text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
Zappos_ML$text <- tolower(Zappos_ML$text)
Zappos_ML$text <- stripWhitespace(Zappos_ML$text)

###Preprocessing###
stopwords("en")
Zappos_ML$text <- removeWords(Zappos_ML$text, stopwords("en"))

char_vec <- unlist(strsplit(Zappos_ML$text, split = " "))
stem_doc <- stemDocument(char_vec)

###Create the Corpus###
Z_Corpus <- VCorpus(VectorSource(stem_doc))
###Create a functin to clean the corpus ###
clean_corpus <- function(corpus){
                        corpus <- tm_map(corpus, removePunctuation)
                        corpus <- tm_map(corpus, content_transformer(tolower))
                        corpus <- tm_map (corpus, removeWords, words = stopwords("en"))
                        corpus <- tm_map (corpus, stripWhitespace)
                        return(corpus)
}

###Clean the corpus###
clean_corp <- clean_corpus(Z_Corpus)
###Created document term matrix ###
zappos_dtm <- DocumentTermMatrix(clean_corp)
###Transforl the DTM into a matrix###
zappos_matrix <- as.matrix(zappos_dtm)
###Term frequnecy plot###
term_frequency <- colSums(zappos_matrix)
term_frequency <- sort(term_frequency, decreasing = TRUE)

#Create plot ###
barplot(term_frequency[1:20], col = "red", las = 2)

###Create the training and the test set for the machine learnong model ###
##Labeling###
                                    
##Creating a training and a test set

SentimentReal <- read_delim("C:/Users/kkusterer/Documents/MBD Semester 2/Social Media Analytics/Group_project/Data/Zappos_labeling.csv", delim = ",")

##Setting the seed for our model ###
set.seed(2)

##et the sample###
z_afinn_sent[,"value"] <- as.factor(z_afinn_sent[,"value"])
y <- as.factor(z_afinn_sent[,"value"])
levels(y)


#Define our training set###
tr <- 0.5

# Define observations to be in training set (we use proportional sampling)
class1_train <- sample(which(y==as.integer(levels(y)[1])), floor(tr*table(y)[1]),replace=FALSE)
class2_train <- sample(which(y==as.integer(levels(y)[2])), floor(tr*table(y)[2]),replace=FALSE)


training_sets <- c(class1_train,class2_train)

# Create a term frequency table for the training set
train_dtm <- z_afinn_sent[training_sets,] %>% anti_join(get_stopwords()) %>%
                                              #count(id,word)%>%
                                              cast_dtm(document = id, term = word,
                                                        value = value, weighting = tm::weightTf) 

                                                                 

# Make a vocabulary (list) of all the terms in the training table
train_vocab <- tidy(train_dtm) %>%
  distinct(term) 


test_dtm <- z_afinn_sent[-training_sets,] %>% anti_join(get_stopwords()) %>%
                                              cast_dtm(document = id, term = word,
                                                       value = value, weighting = tm::weightTf) 



