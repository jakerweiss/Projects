# Assignment 5
library(cluster)
library(stringr)
library(dplyr)
setwd("C:/Users/Jake/Desktop/School/INFO3237")
data <- read.csv("hospital_ortho.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
View(data)
colnames(data)
summary(data)

# 4 ----
data$type <- rownames(data)
data.nc <- data %>%
  filter(str_detect(state, 'NC'))
data.sc <- data %>%
  filter(str_detect(state, 'SC'))
data.va <- data %>%
  filter(str_detect(state, 'VA'))
data.ga <- data %>%
  filter(str_detect(state, 'GA'))
data.tn <- data %>%
  filter(str_detect(state, 'TN'))
nc_data <- rbind(data.nc, data.sc, data.va, data.ga, data.tn)
View(nc_data)

# 4.2
str(nc_data)
nc_dataDrop <- nc_data[-c(2:4,20)]
df <- scale(nc_dataDrop[, c(1:16)])
str(nc_dataDrop)
summary(df)
View(df)

# 5 ----
k.means.fit <- kmeans(df, 3, nstart = 25)
attributes(k.means.fit)
k.means.fit$cluster
k.means.fit$centers
k.means.fit$size

# 5.1
withinssplot <- function(data, nc = 30, seed = 1234) {
  wss <- (nrow(data)-1) * sum(apply(data, 2, var)) 
  for (i in 2:nc) { 
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")
}

# 5.2
withinssplot(df, nc = 15)

# 5.3
k.means.fit <- kmeans(df, 4, nstart = 25)
k.means.fit$cluster
k.means.fit$size

# 5.4
clusplot(df, k.means.fit$cluster, main = "2D repersentation of the Cluster solution", 
         color = TRUE, shade = TRUE, labels = 2, lines = 0)

# 6 ----
# 6.1
d <- dist(df, method = "euclidean")

# Single 
H.single <- hclust(d, method = "single")
plot(H.single) # display dendogram 

# Complete 
H.complete <- hclust(d, method = "complete")
plot(H.complete) # display dendogram 

# Average 
H.average <- hclust(d, method = "average")
plot(H.average) # display dendogram 

# Ward 
H.ward <- hclust(d, method = "ward.D2")
plot(H.ward) # display dendogram 

# 6.4
group <- cutree(H.ward, k = 4)
plot(H.ward)
rect.hclust(H.ward, k = 4, border = "red")


# 7 ----
library(dbscan)

kNNdistplot(df, k = 4)
abline(h = 3, col = "red")

db <- dbscan(df, eps = 3, minPts = 17)
db$cluster


# 7.3
db

# 7.4

# 7.5
# plot 
clusplot(df, db$cluster, main = "2D representation of the Cluster solution", color = TRUE, shade = TRUE, labels = 2, lines = 0)




# 8 ----
plot(silhouette(k.means.fit$cluster, d))
plot(silhouette(group, d))
plot(silhouette(db$cluster, d))
