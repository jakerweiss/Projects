# Assignment 1
adult <- adult.data
# 5
# 5.1
length(adult)

# 5.2
nrow(adult)

# 5.3
range(adult$education_num)
range(adult$capital_loss)
range(adult$hours_per_week)

# 5.4 ------------------------------ help
getwd()
setwd("c:/Users/Jake/Downloads")

library(dplyr)
adultClean <- read.csv("adult.data.csv", strip.white = T, na.string = "?")
adultClean %>%
  select_if(function(x) any(is.na(x)))

# 6
# 6.1
typeof(adult$age)
typeof(adult$workclass)
typeof(adult$education)
typeof(adult$class)

# 6.2
mean(adult$capital_gain)
sd(adult$capital_gain)

# 6.3
unique(adult$occupation)

# 7
adult <- adult %>%
  mutate(log_age = log(age))
p1 <- ggplot(data = adult)
# 7.1
p1 + geom_histogram(mapping = aes(x = age))

# 7.2
p1 + geom_histogram(mapping = aes(x = log_age))

# 7.3
p1 <- ggplot(data = adult)
p1 + geom_point(mapping = aes(x = education_num, y = capital_gain))

# 7.4
p1 + geom_boxplot(mapping = aes(x = class, y = education_num))

