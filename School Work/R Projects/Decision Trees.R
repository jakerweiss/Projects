library(rpart)
library(rpart.plot)
library(ROCR)
library(caret)
library(e1071)
library(rattle)
library(plyr)
library(dplyr)
library(tidyverse)

# For Windows setwd("c:/Users/Jake/Downloads")
setwd("~/Desktop/INFO 3237") # For Mac
getwd()
data <- read.csv("election_campaign_data.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
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
data$twitter <- as.factor(data$twitter)
data$facebook <- as.factor(data$facebook)
data$youtube <- as.factor(data$youtube)
data$cand_ici <- as.factor(data$cand_ici)
data$gen_eletion <- as.factor(data$gen_election)

# 7
dataComp <- data[complete.cases(data),]
View(dataComp)
# 8.1
length(dataComp)
# 8.2

nrow(dataComp)

# 8.3
genvars <- length(unique(dataComp$gen_election))
genvars

# 8.4
twitterCount <- ldply(dataComp$twitter, function(c) sum(c=="1"))
count(twitterCount)

# 8.5
facebookCount <- ldply(dataComp$facebook, function(c) sum(c=="1"))
count(facebookCount)

# data partition
n = nrow(dataComp)
trainIndex <- sample(1:n, 
                     size = round(0.7*n),
                     replace = FALSE)
train_data <- dataComp[trainIndex, ]  
test_data <- dataComp[-trainIndex,]

# 9.1
tree1 <- rpart(gen_election ~ twitter + facebook + youtube, data = train_data) 
fancyRpartPlot(tree1)

# 9.2
count((dataComp$facebook == 1) & (dataComp$gen_election == "W"))

# 9.3
summary(tree1)
# 9.4
predicted_values <- predict(tree1, test_data)
head(predicted_values)

test_data$gen_election <- ifelse(test_data$gen_election == "L", 0, 1)
test_data$gen_election <- as.factor(test_data$gen_election)

pred <- factor(ifelse(predicted_values[,2] > 0.5, 1,0))
confM <- confusionMatrix(pred, test_data$gen_election, positive = levels(test_data$gen_election)[2])
confM
# 9.5
# ????


# 10.1
tree2 <- rpart(gen_election ~ twitter + facebook + youtube + ttl_receipts + cand_ici, data = train_data) 
fancyRpartPlot(tree2)
# 10.2
summary(tree2)

# 11.1
acc <- confusionMatrix(pred, test_data$gen_election, positive = levels(test_data$gen_election)[2])$overall["Accuracy"]
acc

# 11.3
predicted_values2 <- predict(tree2, test_data)
head(predicted_values2)

pred2 <- factor(ifelse(predicted_values2[,2] > 0.5, 1,0))
confM2 <- confusionMatrix(pred2, test_data$gen_election, positive = levels(test_data$gen_election)[2])
confM2
s
acc2 <- confusionMatrix(pred2, test_data$gen_election, positive = levels(test_data$gen_election)[2])$overall["Accuracy"]
acc2
