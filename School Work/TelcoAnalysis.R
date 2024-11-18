# setting up the doc
setwd("C:/Users/Jake/Desktop/INFO 3237 Project")
getwd()
data <- read.csv("Telco.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?"))
View(data)

# number of records
nrow(data)

# number of missing records
sum(is.na(data$TotalCharges))
summary(data$TotalCharges)

# Summary Statistics
length(data$SeniorCitizen)
length(data$tenure)
length(data$MonthlyCharges)
length(data$TotalCharges)

mean(data$SeniorCitizen)
mean(data$tenure)
mean(data$MonthlyCharges)
mean(data$TotalCharges, na.rm = 1)

sd(data$SeniorCitizen)
sd(data$tenure)
sd(data$MonthlyCharges)
sd(data$TotalCharges, na.rm = 1)

min(data$SeniorCitizen)
min(data$tenure)
min(data$MonthlyCharges)
min(data$TotalCharges, na.rm = 1)

max(data$SeniorCitizen)
max(data$tenure)
max(data$MonthlyCharges)
max(data$TotalCharges, na.rm = 1)