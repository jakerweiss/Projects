install.packages("randomForest")
install.packages("pROC")

library(pROC)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(e1071)
library(rattle)
library(plyr)
library(dplyr)
library(tidyverse)
library(randomForest)

# 3
setwd("~/Desktop/INFO 3237") # For Mac
getwd()
data <- read.csv("election_campaign_data-1.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
view(data)

# 4
data$cand_id <- NULL
data$last_name <- NULL
data$first_name <- NULL
data$twitterbirth <- NULL
data$facebookdate <- NULL
data$facebookjan <- NULL
data$youtubebirth <- NULL
View(data)

# 5
factorData <- data %>% mutate_at(c("twitter", "facebook", "youtube", "cand_ici", 
                                   "cand_pty_affiliation", "gender", "gen_election"), factor)
str(factorData)

# 7
dropData <- factorData %>% drop_na()
View(dropData)
n = nrow(dropData)
trainIndex <- sample(1:n, 
                     size = round(0.7*n),
                     replace = FALSE)
train_data <- dropData[trainIndex, ]  
test_data <- dropData[-trainIndex,]

# 8.1
rf5 <- train(gen_election~., data = train_data, method = "rf", tuneGrid = expand.grid
              (.mtry = 2), ntree = 5)
rf5

# 8.2
rf5_2 <- train(gen_election~., data = train_data, method = "rf", tuneGrid = expand.grid
             (.mtry = 7), ntree = 5)
rf5_2

# 8.3
rf10 <- train(gen_election~., data = train_data, method = "rf", tuneGrid = expand.grid
               (.mtry = 7), ntree = 10)
rf10

# 8.4
rf50 <- train(gen_election~., data = train_data, method = "rf", tuneGrid = expand.grid
              (.mtry = 7), ntree = 50)
rf50

#8.5 
pred <- predict(rf50, test_data)
predprob <- predict(rf50, type = "prob", test_data)
cutoff <- 0.5
confusionMatrix(pred, test_data$gen_election)

#8.6
roc <- roc(predictor = predprob[,2],
           response = test_data$gen_election,
           levels = levels(test_data$gen_election))
plot(roc)


#8.7
varImp(rf50)

# 9.1
str(dropData)
dropData2 <- dropData
dropData2$twitter <- NULL
dropData2$facebook <- NULL
dropData2$youtube <- NULL
dropData2$cand_ici < - NULL
dropData2$cand_pty_affiliation <- NULL
dropData2$gender <- NULL

n2 = nrow(dropData2)
trainIndex2 <- sample(1:n2, 
                     size = round(0.7*n2),
                     replace = FALSE)
train_data2 <- dropData2[trainIndex2, ]  
test_data2 <- dropData2[-trainIndex2,]

knn <- train(gen_election~., data=train_data2, method = "knn", tuneLength = 20)
knn

# 9.2
pred2 <- predict(knn, test_data2)
predprob2 <- predict(knn, type = "prob", test_data2)
cutoff <- 0.5
confusionMatrix(pred2, test_data2$gen_election)

# 9.3
roc2 <- roc(predictor = predprob2[,2],
           response = test_data2$gen_election,
           levels = levels(test_data2$gen_election))
plot(roc)

# 10
