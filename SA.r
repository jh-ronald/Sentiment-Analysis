# https://raw.githubusercontent.com/jh-ronald/Sentiment-Analysis/master/Corona_NLP_train.csv

# https://raw.githubusercontent.com/jh-ronald/Sentiment-Analysis/master/Corona_NLP_test.csv

dl <- tempfile()
download.file("https://raw.githubusercontent.com/jh-ronald/Sentiment-Analysis/master/Corona_NLP_train.csv",
              destfile = dl)
train <- read_csv(dl)


dl <- tempfile()
download.file("https://raw.githubusercontent.com/jh-ronald/Sentiment-Analysis/master/Corona_NLP_test.csv",
              destfile = dl)
validation <- read_csv(dl)

Source <- "Twitter"
train <- cbind(Source, train)
validation <- cbind(Source, validation)

train$Sentiment <- factor(train$Sentiment,
                          levels = c("Neutral",
                                     "Positive","Negative",
                                     "Extremely Positive",
                                     "Extremely Negative"))
validation$Sentiment <- factor(validation$Sentiment,
                          levels = c("Neutral",
                                     "Positive","Negative",
                                     "Extremely Positive",
                                     "Extremely Negative"))

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = train$Sentiment, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)
train_nlp <- train[-test_index,]
test_nlp <- train[test_index,]
validation_nlp <- validation

remove_reg <- "&amp;|&lt;|&gt;"

train_nlp <- train_nlp %>%
  mutate(OriginalTweet = str_remove_all(OriginalTweet, remove_reg)) %>%
  unnest_tokens(word, OriginalTweet)

test_nlp <- test_nlp %>%
  mutate(OriginalTweet = str_remove_all(OriginalTweet, remove_reg)) %>%
  unnest_tokens(word, OriginalTweet)

validation_nlp <- validation_nlp %>%
  mutate(OriginalTweet = str_remove_all(OriginalTweet, remove_reg)) %>%
  unnest_tokens(word, OriginalTweet) 

if(!require(tidytext)) install.packages("tidytext", repos = "http://cran.us.r-project.org")
if(!require(textdata)) install.packages("textdata", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")

library(tidytext)
library(textdata)
library(tidyr)

train_sentiment <- train_nlp %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(UserName) %>%
  summarise(score = sum(value))

library(ggplot2)

ggplot(train_sentiment, aes(UserName, score, fill = Source)) +
  geom_col(show.legend = FALSE)

gimme_accuracy <- function(thres,prediction,actual){
  prediction <- prediction %>%
    mutate(sentiment_class = ifelse(score == 0,"Neutral",
                                    ifelse(score > 0,
                                           ifelse(score <= thres, "Positive",
                                                  "Extremely Positive"),
                                           ifelse(score >= -thres, "Negative",
                                                  "Extremely Negative"))))
  
  prediction$sentiment_class <- factor(prediction$sentiment_class, 
                                       levels = c("Neutral",
                                                  "Positive","Negative",
                                                  "Extremely Positive",
                                                  "Extremely Negative"))
  
  comparison <- merge(x = prediction, y = actual, by = "UserName")
  return(sum(comparison$sentiment_class == comparison$Sentiment)/nrow(comparison))

}

thres <- seq(0,10,1)
accuracy <- sapply(thres, gimme_accuracy,
                   prediction = train_sentiment,
                   actual = train)

#Plot values of p and accuracy
qplot(thres, accuracy)

tt <- thres[which.max(accuracy)]

max(accuracy)

thres <- 3

#Validation

validation_sentiment <- validation_nlp %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(UserName) %>%
  summarise(score = sum(value))

final_accuracy <- gimme_accuracy(thres,validation_sentiment,validation)

final_accuracy