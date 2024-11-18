library(tidyverse)
library(cluster)
library(clustertend)
library(dbscan)
library(tidytext)

setwd("C:/Users/Jake/Desktop/School/INFO3237")
data <- read.csv("college_data.csv", sep=",", header=T, strip.white = T, na.strings = 
                  c("NA","NaN","","?"))
View(data)
colnames(data)

# 1a
data$INSTNM <- as.character(data$INSTNM)
tidy_text <- data %>%
  unnest_tokens(word, INSTNM)
head(tidy_text)
tidy_text %>% count(word, sort = TRUE)


data("stop_words")
stop_words
tidy_text <- tidy_text %>% anti_join(stop_words, by ="word")
tidy_text %>% count(word, sort = TRUE)

# 1b
ncol(data)
data2 <- data %>% 
  select(1,5:240)
data.sd <- scale(data2)
str(data.sd)
summary(data.sd)
# K means ----
withinssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
withinssplot(data.sd, nc=10) 

k.means.fit <- kmeans(data.sd, 4, nstart = 25)
attributes(k.means.fit)
k.means.fit$cluster
k.means.fit$centers
k.means.fit$size
clusplot(data.sd, k.means.fit$cluster, main = "2D repersentation of the Cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)
###
# Hierarchical ----
d <- dist(data.sd, method = "euclidean")
H.single <- hclust(d, method="single")
H.complete <- hclust(d, method="complete")
H.average <- hclust(d, method="average")
H.ward <- hclust(d, method="ward.D2")
par(mfrow=c(2,2))
plot(H.single)
plot(H.complete)
plot(H.average)
plot(H.ward)
par(mfrow=c(1,1))
plot(H.ward)
rect.hclust(H.ward, k=4, border="red") 
groups <- cutree(H.ward, k=4)

# DBSCAN ----
kNNdistplot(data.sd, k =13)
abline(h=2.2, col="red")
db <- dbscan(data.sd, eps=2.2, minPts=13)
db$cluster
summary(db)
db
clusplot(data.sd, db$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

# Part 2 ----
plot(silhouette(k.means.fit$cluster, d)) #0.13
plot(silhouette(groups, d)) #0.11
plot(silhouette(db$cluster, d)) #-0.08

data$cluster <- k.means.fit$cluster
View(data)

# 2a ----

data[,c("INSTNM","cluster")] %>% filter(INSTNM == "Georgia State University")  
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "University of North Carolina at Charlotte")
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "University of Arizona")  

# 2b ----

data[,c("INSTNM","cluster")] %>% filter(INSTNM == "University of North Carolina at Chapel Hill")  
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "North Carolina State University at Raleigh")
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "Arizona State University-Tempe")  

# 2c ----

data[,c("INSTNM","cluster")] %>% filter(INSTNM == "University of Georgia")  
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "Georgia State University")


# 2d ----

data[,c("INSTNM","cluster")] %>% filter(INSTNM == "Georgia Institute of Technology-Main Campus")  
data[,c("INSTNM","cluster")] %>% filter(INSTNM == "Emory University")