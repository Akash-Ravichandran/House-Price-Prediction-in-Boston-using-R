library(neuralnet)
library(caret)
library(tidyverse)
library(MASS)

data('Boston')
df <- Boston
str(df)
?Boston
hist(df$age, col='green')
summary(df)


nrow(df)
rows <- sample(1: nrow(df), nrow(df)*.8, replace =F)
train_base <- df[rows,]
test_base <- df[-rows,]
dim(train_base)
dim(test_base)

rows2 <- createDataPartition(df$age, times = 1, p =.8, list =F)
training <- df[rows2,]
testing <- df[-rows2,]

dim(training)
dim(testing)

model_lm <- train(age~.,
                  data = training,
                  method = 'lm',
                  trControl = 
                    trainControl(method = 'repeatedcv', number =2, repeats = 2))
model_lm


model_rf <- train(age~.,
                  data = training,
                  method = 'ranger',
                  trControl = 
                    trainControl(method = 'repeatedcv', number =2, repeats = 2))
model_rf

model_gbm <- train(age~.,
                   data = training,
                   method = 'gbm',
                   trControl = 
                     trainControl(method = 'repeatedcv', number =2, repeats = 2))
model_gbm


sample <- resamples(list(Linear = model_lm, Forest = model_rf, GBM = model_gbm))

bwplot(sample)
dotplot(sample)
summary(sample)
