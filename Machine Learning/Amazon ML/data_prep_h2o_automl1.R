#1.0 Import Libraries ----

list.of.packages <- c("pacman")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(pacman)
pacman::p_load(h2o,tidyverse,tidytext,topicmodels,tm,SnowballC)

library(tidyverse) # general utility & workflow functions
library(tidytext) # tidy implimentation of NLP methods
library(topicmodels) # for LDA topic modelling 
library(tm) # general text mining functions, making document term matrixes
library(SnowballC) # for stemming
library(janitor) # clean column names
library(h2o) # Predicitve Modelling


# 2.0  Import Dataset ----
train <- read.csv(paste(getwd(),"/Dataset/train.csv",sep = ""), na.strings="", stringsAsFactors=FALSE)
test <- read.csv(paste(getwd(),"/Dataset/test.csv",sep = ""), na.strings="", stringsAsFactors=FALSE)



# 3.0 Exploratory Data Analysis ----

# Class Distribution
table(train$topic)

# Title Distribution
table(train$Review.Title)



# Check if Review.Title in Train and Test are same or not if not Collate Merge Title and Text to avoid feature missingness.
unique(unique(training_set$Review.Title) %in% unique(test_set$Review.Title))




# Merge Title and Text as there are too many Titles to treat it as a categorical variable
train$Review.Text <- paste(train$Review.Title,train$Review.Text,sep = ":")
test$Review.Text <- paste(test$Review.Title,test$Review.Text,sep = ":")

train$Review.Title <- NULL
test$Review.Title <- NULL

# 3.1 Topic modeling: ----

# The NLP task of identifying automatically identifying major themes in a text,
# usually by identifying informative words.

# Use is to identify which words are important for text that is labeled for topic 



# function that takes in a dataframe and the name of the columns
# with the document texts and the topic labels. If plot is set to
# false it will return the tf-idf output rather than a plot.
top_terms_by_topic_tfidf <- function(text_df, text_column, group_column, plot = T){
  # name for the column we're going to unnest_tokens_ to
  # (you only need to worry about enquo stuff if you're
  # writing a function using using tidyverse packages)
  group_column <- enquo(group_column)
  text_column <- enquo(text_column)
  
  # get the count of each word in each review
  words <- text_df %>%
    unnest_tokens(word, !!text_column) %>%
    count(!!group_column, word) %>% 
    ungroup()
  
  # get the number of words per text
  total_words <- words %>% 
    group_by(!!group_column) %>% 
    summarize(total = sum(n))
  
  # combine the two dataframes we just made
  words <- left_join(words, total_words)
  
  # get the tf_idf & order the words by degree of relevence
  tf_idf <- words %>%
    bind_tf_idf(word, !!group_column, n) %>%
    select(-total) %>%
    arrange(desc(tf_idf)) %>%
    mutate(word = factor(word, levels = rev(unique(word))))
  
  if(plot == T){
    # convert "group" into a quote of a name
    # (this is due to funkiness with calling ggplot2
    # in functions)
    group_name <- quo_name(group_column)
    
    # plot the 5 most informative terms per topic
    tf_idf %>% 
      group_by(!!group_column) %>% 
      top_n(5) %>% 
      ungroup %>%
      ggplot(aes(word, tf_idf, fill = as.factor(group_name))) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") +
      facet_wrap(reformulate(group_name), scales = "free") +
      coord_flip()
  }else{
    # return the entire tf_idf dataframe
    return(tf_idf)
  }
}



# let's see what our most informative deceptive top 5 words are
top_terms_by_topic <- top_terms_by_topic_tfidf(text_df = train, # dataframe
                                               text_column = Review.Text, # column with text
                                               group_column = topic, # column with topic label
                                               plot = F) # return a plot

# do our own plotting
top_terms_by_topic  %>% 
  group_by(topic) %>% 
  top_n(5) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = topic)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~topic, ncol = 4, scales = "free", ) +
  coord_flip()



# 4.0 Feature Engineering ----

# Merge train and test Corpus

data <- c(train$Review.Text,test$Review.Text)

# Create Corpus
data_corpus <- VCorpus(VectorSource(data))

##Removing Punctuation
data_corpus <- tm_map(data_corpus, content_transformer(removePunctuation))

##Removing numbers
data_corpus <- tm_map(data_corpus, removeNumbers)

##Converting to lower case
data_corpus <- tm_map(data_corpus, content_transformer(tolower))

##Removing stop words
data_corpus <- tm_map(data_corpus, content_transformer(removeWords), stopwords("english"))

##Stemming
data_corpus <- tm_map(data_corpus, stemDocument)

##Whitespace
data_corpus <- tm_map(data_corpus, stripWhitespace)

# Create Document Term Matrix
dtm_data <- DocumentTermMatrix(data_corpus)

dtm_data_matrix <- as.matrix(dtm_data)

dtm_data_frame <- as.data.frame(dtm_data_matrix)

#Remove Sparse Terms
data1 <- removeSparseTerms(dtm_data, .95)

dtm_data_frame <- as.data.frame(as.matrix(data1))

# rm(dtm_data_matrix,data_corpus,data1)





dtm_data_frame = dtm_data_frame %>% clean_names()


# Split data into Train & Test Set

train_set <- dtm_data_frame[1:nrow(train),]

train_set$topic <- train$topic


test_set <- dtm_data_frame[(nrow(train)+1):nrow(dtm_data_frame),]



#Convert Frequencies into Numerics
train_set[,1:80] <- data.frame(lapply(train_set[,1:80], as.numeric), stringsAsFactors=FALSE)

test_set[,1:80] <- data.frame(lapply(test_set[,1:80], as.numeric), stringsAsFactors=FALSE)


train_set$topic <- as.factor(train_set$topic)



#5.0 Predictive Modelling ----

# Running on 6GB RAm

h2o.init(max_mem_size = '6G')


output <- "topic"
input  <- setdiff( names(train_set), output )

train = as.h2o(train_set)


# Predictive Modelling 2hrs
aml <- h2o.automl(y = output,x = input,training_frame = train,nfolds = 10,seed = 123,balance_classes = T,max_runtime_secs = 7200,
                  exclude_algos = c("DRF","GLM"),stopping_metric = "lift_top_group")


# check the leaderboard
lb <- aml@leaderboard
lb

# get model ids  for all models 
model_ids <- as.data.frame(aml@leaderboard$model_id)[,1]

# Get all model stacked ensembled model
se <- h2o.getModel(grep("StackedEnsemble_AllModels",model_ids,value = TRUE)[1])

metalearner <- h2o.getModel(se@model$metalearner$name)

h2o.varimp(metalearner)

gbm <- h2o.getModel(grep("StackedEnsemble_BestOfFamily",model_ids,value=TRUE)[1])


#-------------- Make Predictions -------------------------#
test = as.h2o(test_set)

pred <- h2o.predict(gbm, test)

pred = as.data.frame(pred)

test <- read.csv(paste(getwd(),"/Dataset/test.csv",sep = ""), na.strings="", stringsAsFactors=FALSE)

test$topic <- pred$predict

names(test)[1] <- "Review Text"
names(test)[2] <- "Review Title"


write.csv(test,"h2o_Automl_1.csv",row.names = F)





