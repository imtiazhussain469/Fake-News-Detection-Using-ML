library(class)
library(tidyverse)
library(tidytext)
library(tm)
library(dsEssex)
library(wordcloud)
library(syuzhet)
library(gridExtra)
library(pander)
library(car)
library(caret)
library(purrr) 
library(e1071)


# Read the data file in a data frame
data <- read.csv("train.csv", sep = ",")

# Ignoring un-allowed characters
data$text <- iconv(data$text, 'UTF-8', 'ASCII')

# Converting to data frame.
data <- as.data.frame(data)


# Getting a glimpse of dataset columns and its observations.
str(data)

# Setting seed value to reproduce the results
set.seed(1)

# Sampling 5000 rows randomly because the original data is pretty much large in terms of textual data and takes a alot of time.
sap <- sample(nrow(data), 5000)
data <- data[sap,]

# Checking dimensions of final data set.
dim(data)


# Checking the distribution of target variables.
table(data$target)

# Genearting a histogram to show th distribution.
hist(data$target, 
     main = "Distribution of Target Column", 
     col = "red",
     xlab = "Target Variable")

------------------------------
  ## Processing of Location
------------------------------
  
# Separating data of each class i.e fake and real.
Target_1 <- data %>%
  filter(target == 1)

Target_0 <- data %>%
  filter(target == 0)

head(Target_1)


#### Cleaning Location For Fake News

# Loading the stop words file. This file is downloaded from internet which consists of stop words that needs to be removed from the dataset.
x <- scan("stop_words_english.txt", 
          character(), 
          quote = "")

# Creating a corpus for target value of 1. This is created to perform further data cleaning on the data set.
text_1 <- Target_1$location

docs_1 <- Corpus(VectorSource(text_1))

# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Convert / to white space.
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "/")

# Convert @ to whitespace
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "@")

# Convert | to whitespace
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\|")

# Convert | to whitespace
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\-")

# Convert text / tweets to lower case.
docs_1 <- tm_map(docs_1, 
                 content_transformer(tolower))

# Remove numbers from the test.
docs_1 <- tm_map(docs_1, 
                 removeNumbers)

# Remove english common stopwords. This is done using a reporsitory of stop words present in R.
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 stopwords("english"))

# Remove your own stop word
# specify your stopwords as a character vector. This is done using the file downloaded from internet.
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 x)

# Remove punctuation marks
docs_1 <- tm_map(docs_1, 
                 removePunctuation)

# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, 
                 stripWhitespace)

# Convert corpus to a suitable format encoding.
docs_1 <- tm_map(docs_1, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))


# Generating term document matrix. Read more about it to understand it.
dtm_1 <- TermDocumentMatrix(docs_1)

# Convert term document to a matrix
m_1 <- as.matrix(dtm_1)

# Sort rows in descending order
v_1 <- sort(rowSums(m_1),
            decreasing=TRUE)

# Convert to dataframe
d_1 <- data.frame(word = names(v_1),
                  freq = v_1)

# Filter values that are left unremoved from cleaning step
d_1 <- d_1 %>% 
  filter(word != "—")

# Check top 10 rows of term document matrix
d_1 <- head(d_1, 10)


ggplot(data = d_1, 
       mapping = aes(x = word, 
                     y = freq, 
                     fill = word)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Locations By Fake News") +
  xlab("Country Name") +
  ylab("Count of Occurrence in Tweets")

#### Cleaning Location For Real News

# Creating a corpus.
text_1 <- Target_0$location
docs_1 <- Corpus(VectorSource(text_1))


# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "/")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "@")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\|")

docs_1 <- tm_map(docs_1, 
                 content_transformer(tolower))
# Remove numbers
docs_1 <- tm_map(docs_1, 
                 removeNumbers)
# Remove english common stopwords
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 x) 
# Remove punctuations
docs_1 <- tm_map(docs_1, 
                 removePunctuation)
# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, 
                 stripWhitespace)

docs_1 <- tm_map(docs_1, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))


# Generating term document matrix
dtm_1 <- TermDocumentMatrix(docs_1)
m_1 <- as.matrix(dtm_1)
v_1 <- sort(rowSums(m_1),
            decreasing=TRUE)
d_1 <- data.frame(word = names(v_1),
                  freq = v_1)
d_1 <- d_1 %>% 
  filter(word != "—")
d_1 <- head(d_1, 10)

ggplot(data = d_1, 
       mapping = aes(x = word, 
                     y = freq, 
                     fill = word)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Locations By Real News") +
  xlab("Country Name") +
  ylab("Count of Occurrence in Tweets") + theme(axis.text.x = element_text(angle=90, hjust=1))

------------------------------
  ## Processing of Keywords
------------------------------

#### Cleaning Keywords For Fake News
  
# Creating a corpus.
text_1 <- Target_1$keyword
docs_1 <- Corpus(VectorSource(text_1))

# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "/")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "@")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\|")

docs_1 <- tm_map(docs_1, 
                 content_transformer(tolower))
# Remove numbers
docs_1 <- tm_map(docs_1, 
                 removeNumbers)
# Remove english common stopwords
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 x) 
# Remove punctuations
docs_1 <- tm_map(docs_1, 
                 removePunctuation)
# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, 
                 stripWhitespace)

docs_1 <- tm_map(docs_1, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))


# Generating term document matrix
dtm_1 <- TermDocumentMatrix(docs_1)
m_1 <- as.matrix(dtm_1)
v_1 <- sort(rowSums(m_1),
            decreasing=TRUE)
d_1 <- data.frame(word = names(v_1),
                  freq = v_1)
d_1 <- d_1 %>% 
  filter(word != "—")
d_1 <- head(d_1, 10)


ggplot(data = d_1, mapping = aes(x = word, y = freq, fill = word)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Keywords By Fake News") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))

#### Cleaning Keyword For Real News


# Creating a corpus.
text_1 <- Target_0$keyword
docs_1 <- Corpus(VectorSource(text_1))

# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "/")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "@")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\|")

docs_1 <- tm_map(docs_1, 
                 content_transformer(tolower))
# Remove numbers
docs_1 <- tm_map(docs_1, 
                 removeNumbers)
# Remove english common stopwords
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 x) 
# Remove punctuations
docs_1 <- tm_map(docs_1, 
                 removePunctuation)
# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, 
                 stripWhitespace)

docs_1 <- tm_map(docs_1, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))


# Generating term document matrix
dtm_1 <- TermDocumentMatrix(docs_1)
m_1 <- as.matrix(dtm_1)
v_1 <- sort(rowSums(m_1),
            decreasing=TRUE)
d_1 <- data.frame(word = names(v_1),
                  freq = v_1)
d_1 <- d_1 %>% 
  filter(word != "—")
d_1 <- head(d_1, 10)

ggplot(data = d_1, mapping = aes(x = word, y = freq, fill = word)) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Top 10 Keywords By Real News") + 
  theme(axis.text.x = element_text(angle=90, hjust=1))


-------------------------------
  ## Processing of Tweets
--------------------------------
  
Target_1 <- data %>%
  filter(target == 1)

Target_0 <- data %>%
  filter(target == 0)


#### Data Pre-Processing & Exploration For Target "1" (Fake News)



# Creating a corpus.
text_1 <- Target_1$text
docs_1 <- Corpus(VectorSource(text_1))

# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "/")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "@")
docs_1 <- tm_map(docs_1, 
                 toSpace, 
                 "\\|")

docs_1 <- tm_map(docs_1, 
                 content_transformer(tolower))
# Remove numbers
docs_1 <- tm_map(docs_1, 
                 removeNumbers)
# Remove english common stopwords
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_1 <- tm_map(docs_1, 
                 removeWords, 
                 x) 
# Remove punctuations
docs_1 <- tm_map(docs_1, 
                 removePunctuation)
# Eliminate extra white spaces
docs_1 <- tm_map(docs_1, 
                 stripWhitespace)

docs_1 <- tm_map(docs_1, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))

# Generating term document matrix
dtm_1 <- TermDocumentMatrix(docs_1)
m_1 <- as.matrix(dtm_1)
v_1 <- sort(rowSums(m_1),
            decreasing=TRUE)
d_1 <- data.frame(word = names(v_1),
                  freq = v_1)
d_1 <- d_1 %>% 
  filter(word != "—")

# Tokenizing the words
poa_word_v <- get_tokens(d_1$word, 
                         pattern = "\\w")


# Get sentiments using NRC method and treating the words as english language words.
nrc_vector <- get_sentiment(poa_word_v, 
                            method = "nrc", 
                            lang = "english")

# Convert NRC sentiments to data frame.
nrc_vector <- as.data.frame(nrc_vector)

# Count occurrence of each sentiments in the data frame.
nrc_vector <- nrc_vector %>% 
  group_by(nrc_vector) %>% 
  summarise(Counts = n())

# Plot the NRC vector for target value of 1 and plot results.
Target_1_sent <- ggplot(data = nrc_vector, 
                        mapping = aes(x = as.factor(nrc_vector), 
                                      y=Counts, 
                                      fill = as.factor(nrc_vector))) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Sentiments Graph For Fake News")

Target_1_sent

# Get NRC Sentiments.
result <- get_nrc_sentiment(as.character(d_1$word))

result1<-data.frame(t(result))

new_result <- data.frame(rowSums(result1))

names(new_result)[1] <- "count"

new_result <- cbind("sentiment" = rownames(new_result), 
                    new_result)

rownames(new_result) <- NULL
Target_1sent_class <- qplot(sentiment, 
                            data=new_result[1:8,], 
                            weight=count, 
                            geom="bar",
                            fill = sentiment) + 
  ggtitle("Sentiments For Target Variable 1")


Target_1sent_class




# Generating wordcloud
set.seed(1234)
wordcloud(words = d_1$word, 
          freq = d_1$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#### Data Pre-Processing and Exploration For Target "0" (Not a Fake News)



# Creating a corpus.
text_0 <- Target_0$text
docs_0 <- Corpus(VectorSource(text_0))

# Performing data cleaning.
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs_0 <- tm_map(docs_0, 
                 toSpace, 
                 "/")
docs_0 <- tm_map(docs_0, 
                 toSpace, 
                 "@")
docs_0 <- tm_map(docs_0, 
                 toSpace, 
                 "\\|")

docs_0 <- tm_map(docs_0, 
                 content_transformer(tolower))
# Remove numbers
docs_0 <- tm_map(docs_0, 
                 removeNumbers)
# Remove english common stopwords
docs_0 <- tm_map(docs_0, 
                 removeWords, 
                 stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
docs_0 <- tm_map(docs_0, 
                 removeWords, 
                 x) 
# Remove punctuations
docs_0 <- tm_map(docs_0, 
                 removePunctuation)
# Eliminate extra white spaces
docs_0 <- tm_map(docs_0, 
                 stripWhitespace)

docs_0 <- tm_map(docs_0, 
                 function(x) iconv(enc2utf8(x), sub = "byte"))

# Generating term document matrix
dtm_0 <- TermDocumentMatrix(docs_0)
m_0 <- as.matrix(dtm_0)
v_0 <- sort(rowSums(m_0),
            decreasing=TRUE)
d_0 <- data.frame(word = names(v_0),
                  freq = v_0)
d_0 <- d_0 %>% 
  filter(word != "—")

poa_word_v <- get_tokens(d_0$word, 
                         pattern = "\\W")
nrc_vector <- get_sentiment(poa_word_v, 
                            method = "nrc", 
                            lang = "english")

nrc_vector <- as.data.frame(nrc_vector)
nrc_vector <- nrc_vector %>% 
  group_by(nrc_vector) %>% 
  summarise(Counts = n())

Target_0_sent <- ggplot(data = nrc_vector, 
                        mapping = aes(x = as.factor(nrc_vector), 
                                      y=Counts, 
                                      fill = as.factor(nrc_vector))) +
  geom_bar(stat="identity", position = "dodge") +
  labs(title = "Sentiments Graph For Target Value 0")

Target_0_sent

result <- get_nrc_sentiment(as.character(d_0$word))
result0<-data.frame(t(result))
new_result <- data.frame(rowSums(result0))
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), 
                    new_result)
rownames(new_result) <- NULL
Target_0sent_class <- qplot(sentiment, 
                            data=new_result[1:8,], 
                            weight=count, 
                            geom="bar",
                            fill = sentiment) + 
  ggtitle("Sentiments For Target 0")


Target_0sent_class

# Generating wordcloud
set.seed(1234)
wordcloud(words = d_0$word, 
          freq = d_0$freq, 
          min.freq = 1,
          max.words=200, 
          random.order=FALSE, 
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))



## Data Modelling



# Reading the file, subsetting only text column and category column. Renaming it and then checking distribution of both target values.
corpus <- read.csv("train.csv", 
                   sep = ",")

corpus$text <- iconv(corpus$text, 
                     'UTF-8', 
                     'ASCII')

corpus <- as.data.frame(corpus)

corpus <- corpus[,4:5]

colnames(corpus) <- c("text", 
                      "cat")

corpus$cat <- factor(corpus$cat)

pander(table(corpus$cat), 
       caption="Fake and Real Count in Raw Data")

# first put the corpus in tm format
corpus2 <- Corpus(VectorSource(corpus$text))

# standardize to lowercase
corpus2 <- tm_map(corpus2, 
                  content_transformer(tolower))

# remove tm stopwords
corpus2 <- tm_map(corpus2, 
                  removeWords, 
                  stopwords())

# standardize whitespaces
corpus2 <- tm_map(corpus2, 
                  stripWhitespace)

# remove punctuation
corpus2 <- tm_map(corpus2, 
                  removePunctuation)

dtm <- DocumentTermMatrix(corpus2)

# words appearing more than 10x
features <- findFreqTerms(dtm, 
                          10)

# limit to frequent terms, i.e., 10 or more appearances using the dictionary parameter
dtm2 <- DocumentTermMatrix(corpus2, list(global = c(2, Inf),
                                         dictionary = features))
inspect(dtm2)

set.seed(1)
train_idx <- createDataPartition(corpus$cat, 
                                 p = 0.75, 
                                 list = FALSE)

# set for the original raw data 
train1 <- corpus[train_idx,]
test1 <- corpus[-train_idx,]

# set for the cleaned-up data
train2 <- corpus2[train_idx]
test2 <- corpus2[-train_idx]




#/////********
dict2 <- findFreqTerms(dtm2, 
                       lowfreq = 10)


news_train <- DocumentTermMatrix(train2, 
                                 list(dictionary = dict2))

news_test <- DocumentTermMatrix(test2, 
                                list(dictionary = dict2))

# this step further converts the DTM-shaped data into a categorical form for modeling
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
}

news_train <- news_train %>% apply(MARGIN=2, FUN=convert_counts)
news_test <- news_test %>% apply(MARGIN=2, FUN=convert_counts)

news_train <- as.data.frame(news_train)
news_test <- as.data.frame(news_test)


head(news_train)



### SVM Classifier


# SVM 
news_train1 <- cbind(cat=factor(train1$cat), news_train)
news_test1 <- cbind(cat=factor(test1$cat), news_test)

news_train1<-as.data.frame(news_train1)
news_test1<-as.data.frame(news_test1)

# model specification
fit1 <- svm(cat ~ ., 
            data = news_train1)

# print a summary
fit1




fit1.pred <- predict(fit1, 
                     na.omit(news_test1))

confMatrix1 <- confusionMatrix(fit1.pred, 
                               news_test1$cat, 
                               positive = "0")
confMatrix1



### KNN Classifier


library(class)
classifier_knn <- knn(train = news_train1,
                      test = news_test1,
                      cl = news_train1$cat,
                      k = 2)

cm <- table(news_test1$cat, 
            classifier_knn)

# Model Evaluation
confusionMatrix(cm)


### Naive Bayes Classifier


news_test1$cat <- as.factor(news_test1$cat)
news_train1$cat <- as.factor(news_train1$cat)
classifier_cl <- naiveBayes(cat ~ ., 
                            data = news_train1)

y_pred <- predict(classifier_cl, 
                  newdata = news_test1)

# Confusion Matrix
cm <- table(news_test1$cat, y_pred)

# Model Evaluation
confusionMatrix(cm)
