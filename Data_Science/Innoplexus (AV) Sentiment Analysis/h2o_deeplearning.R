##global:==============================================================================================================================
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#--------Script to install missing  Libraries------------------#

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

rm(list=ls())



# #Downloading and loading required packages
# 
req_packages <- c("keras","grid","gridExtra","wordcloud","tidytext","tidyverse")





# Libraries ------------------------------------------------------------------

library(tidyverse) # importing, cleaning, visualising 
library(tidytext) # working with text
library(wordcloud) # visualising text
library(gridExtra) # extra plot options
library(grid) # extra plot options
library(keras) # deep learning with keras


options(scipen=999) # turn off scientific display

# Import ------------------------------------------------------------------

train = read.csv("train_F3WbcTw.csv",encoding = "UTF-8",header = T,sep = ",",na.strings = "",stringsAsFactors = F)

test = read.csv("test_tOlRoBf.csv",encoding = "UTF-8",header = T,sep = ",",na.strings = "",stringsAsFactors = F)


#--------------- Remove ID: as modelling should be on features not IDS --------------------#
train$unique_hash <- NULL
test$unique_hash <- NULL


#-------------- Check for missingness ----------------------------#
colSums(is.na(train))
colSums(is.na(test))


# In cases where we have a single word, we won’t actually need a model that considers sequences, 
# for obvious reasons. It may even be worth splitting out single word observations 
# and creating a model just for these,
# but I will leave them in for now.

# Combine -----------------------------------------------------------------

train = train %>% mutate(Split = "train")
test = test %>% mutate(Split = "test")

full = data.frame(rbind(train %>% select(-sentiment), test))


# Top words ---------------------------------------------------------------


# Have a look at the most common words (having removed stop words) grouping by drug

top_words_train = full %>% 
  filter(Split == "train") %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(drug,word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


top_words_test = full %>% 
  filter(Split == "test") %>% 
  unnest_tokens(output = word, input = text) %>% 
  group_by(drug,word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))


# Plot the top 10 words for train/test with and without stop words
grobs = list(
  tableGrob(head(top_words_train,10), theme = ttheme_minimal()),
  tableGrob(head(top_words_train %>% anti_join(stop_words),10), theme = ttheme_minimal()),
  tableGrob(head(top_words_test,10), theme = ttheme_minimal()),
  tableGrob(head(top_words_test %>% anti_join(stop_words),10), theme = ttheme_minimal())
)


lg <- tableGrob(c("", "Train", "Test"), theme= ttheme_minimal())
rg <- arrangeGrob(grobs = grobs, ncol=2,
                  top = textGrob("Top 10 Words",gp=gpar(fontsize=18)))
grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))


# Let’s look at what’s going on with the apostrophes by considering bi-grams where the second word 
# is " a|i|to|the|of" (i.e. where there was originally " ’s“):

# Dodgy bi-grams ---------------------------------------------------------------

# word i
top_dodgy_bigrams_train = full %>% 
  filter(Split == "train") %>% 
  unnest_tokens(output = bigrams, input = text, token = "ngrams", n = 2) %>% 
  filter(str_sub(bigrams, start = (nchar(bigrams)-1), end = nchar(bigrams)) == ' i') %>%
  group_by(bigrams) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top_dodgy_bigrams_test = full %>% 
  filter(Split == "test") %>% 
  unnest_tokens(output = bigrams, input = text, token = "ngrams", n = 2) %>% 
  filter(str_sub(bigrams, start = (nchar(bigrams)-1), end = nchar(bigrams)) == ' i') %>%
  group_by(bigrams) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

grobs = list(
  tableGrob(head(top_dodgy_bigrams_train,10), theme = ttheme_minimal()),
  tableGrob(head(top_dodgy_bigrams_test,10), theme = ttheme_minimal())
)

lg <- tableGrob(c("", "Train", "Test"), theme= ttheme_minimal())
rg <- arrangeGrob(grobs = grobs, ncol=1,
                  top = textGrob("Top 10 Dodgy Apostrophes",gp=gpar(fontsize=18)))
grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))



# word a
top_dodgy_bigrams_train = full %>% 
  filter(Split == "train") %>% 
  unnest_tokens(output = bigrams, input = text, token = "ngrams", n = 2) %>% 
  filter(str_sub(bigrams, start = (nchar(bigrams)-1), end = nchar(bigrams)) == ' a') %>%
  group_by(bigrams) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

top_dodgy_bigrams_test = full %>% 
  filter(Split == "test") %>% 
  unnest_tokens(output = bigrams, input = text, token = "ngrams", n = 2) %>% 
  filter(str_sub(bigrams, start = (nchar(bigrams)-1), end = nchar(bigrams)) == ' a') %>%
  group_by(bigrams) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))

grobs = list(
  tableGrob(head(top_dodgy_bigrams_train,10), theme = ttheme_minimal()),
  tableGrob(head(top_dodgy_bigrams_test,10), theme = ttheme_minimal())
)

lg <- tableGrob(c("", "Train", "Test"), theme= ttheme_minimal())
rg <- arrangeGrob(grobs = grobs, ncol=1,
                  top = textGrob("Top 10 Dodgy Apostrophes",gp=gpar(fontsize=18)))
grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))


# Adjustments -----------------------------------------------------
# But instances such as “he’s” can just be replaced with “he is”. 
# For the moment I will settle on replacing abbreviations, 
# and for instances of potential possession I will simply remove the " s" 
# that remains after removing punctuation.

# There are a few odd things in the data based on the above that I want to make adjustments for

full = full %>% mutate(
  text = gsub(" n't"," not", text), 
  text = gsub(" it's","it is", text),
  # Going to remove all instances of "'s" that remain (nearly always possession)
  # This way we retain the immediate connection between the possession and possessor in our sequence
  # Otherwise we will end up padding it with zeros and lose some information
  
  text = gsub(" 's "," ", text)
  )              


# Tokenizer -------------------------------------------------------------------

# Setup some parameters

max_words = 15000 # Maximum number of words to consider as features
maxlen = 32 # Text cutoff after n words


# Prepare to tokenize the text

texts = full$text

tokenizer = text_tokenizer(num_words = max_words) %>% 
  fit_text_tokenizer(texts)

# Tokenize - i.e. convert text into a sequence of integers

sequences = texts_to_sequences(tokenizer, texts)
word_index = tokenizer$word_index


# Pad out texts so everything is the same length

data = pad_sequences(sequences, maxlen = maxlen)


# Split back into train and test

train_matrix = data[1:nrow(train),]
test_matrix = data[(nrow(train)+1):nrow(data),]

train_matrix <- as.data.frame(train_matrix)
test_matrix <- as.data.frame(test_matrix)


# append class label
train_matrix$sentiment <- train$sentiment
train_matrix$sentiment[train_matrix$sentiment==0] <- "positive"
train_matrix$sentiment[train_matrix$sentiment==1] <- "nagative"
train_matrix$sentiment[train_matrix$sentiment==2] <- "neutral"

train_matrix$sentiment <- as.factor(train_matrix$sentiment)

# check for drugs
a <- unique(train$drug)
b <- unique(test$drug)

# check if drugs in test are in train if it returns 0 means we can train data adding drug as feature
setdiff(b,a)

# we cannot add drug as feature

#---------------------------- H2o framework ------------------------------#
library(h2o)
## Use H2O's deeplearning
## Start cluster with all available threads
h2o.init(nthreads=-1,max_mem_size='6G')
## Load data into cluster from R
trainHex<-as.h2o(train_matrix)

## Set up variable to use all features other than those specified here
features<-colnames(train_matrix)[!(colnames(train_matrix) %in% c("sentiment"))]

## Train a deeplearning using all default parameters
dlHex <- h2o.automl(x=features,y="sentiment", 
                    exclude_algos =  c("GLM", "GBM", "DRF","XGBoost","StackedEnsemble"),
                    training_frame=trainHex)

summary(dlHex)


# check the leaderboard
lb <- dlHex@leaderboard
lb




## Load test data into cluster from R
testHex<-as.h2o(test_matrix)

## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(dlHex,testHex))

## Return the predictions to the original scale of the Sales data
sample_submission_i5xnIZD <- read.csv("sample_submission_i5xnIZD.csv", stringsAsFactors=FALSE)
sample_submission_i5xnIZD$sentiment <- as.character(predictions$predict)
sample_submission_i5xnIZD$sentiment[sample_submission_i5xnIZD$sentiment=="positive"] <- "0"
sample_submission_i5xnIZD$sentiment[sample_submission_i5xnIZD$sentiment=="nagative"] <- "1"
sample_submission_i5xnIZD$sentiment[sample_submission_i5xnIZD$sentiment=="neutral"] <-  "2"

sample_submission_i5xnIZD$sentiment <- as.numeric(sample_submission_i5xnIZD$sentiment)


write.csv(sample_submission_i5xnIZD, "h2o_dl_1.csv",row.names=F)


