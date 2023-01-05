if(!require(vader)) install.packages("vader", repos = "http://cran.us.r-project.org")
library(vader)

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

train_nlp <- train
validation_nlp <- validation


i <- seq(1,1000)
vader_score <- vader_df(train_nlp$OriginalTweet[i])["compound"]
train_n <- train_nlp[i,]


gimme_accuracy <- function(thres,prediction,actual){
  prediction <- prediction %>%
    mutate(sentiment_class = ifelse(compound == 0,"Neutral",
                                    ifelse(compound > 0,
                                           ifelse(compound <= thres, "Positive",
                                                  "Extremely Positive"),
                                           ifelse(compound >= -thres, "Negative",
                                                  "Extremely Negative"))))
  
  prediction$sentiment_class <- factor(prediction$sentiment_class, 
                                       levels = c("Neutral",
                                                  "Positive","Negative",
                                                  "Extremely Positive",
                                                  "Extremely Negative"))
  
  comparison <- cbind(prediction, actual)
  return(sum(comparison$sentiment_class == comparison$Sentiment)/nrow(comparison))
  
}

thres <- seq(0,1,0.01)
accuracy <- sapply(thres, gimme_accuracy,
                   prediction = vader_score,
                   actual = train_n)

#Plot values of p and accuracy
qplot(thres, accuracy)

tt <- thres[which.max(accuracy)]

max(accuracy)

thres <- tt

#Validation

val_vader_score <- vader_df(validation_nlp$OriginalTweet[i])["compound"]
validation_n <- validation_nlp[i,]

final_accuracy <- gimme_accuracy(thres,val_vader_score,validation_n)

final_accuracy