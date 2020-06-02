setwd("/Users/manasavegesna/Documents/BetterMe/DataScienceR")

#Loading the Iris Dataset

library(datasets)
data(iris)
iris <- datasets::iris
View(iris)

#using RCurl 
#library(RCurl)
# iris <- read.csv(text = getURL("someURL"))

########################
#Summary Statistics 
########################

head(iris)
tail(iris,5)

summary(iris)
summary(iris$Sepal.Width)

#checking for missing data
sum(is.na(iris)) #summation of missing data

#getting a larger set of statistics for the dataset
library(skimr) 
skim(iris)

#Panel plots
plot(iris)
plot(iris, col='blue')

#Scatter plot
plot(iris$Sepal.Width, iris$Sepal.Length, col='red', xlab= 'Sepal width', ylab = 'Sepal Lenght')

#Histogram
hist(iris$Sepal.Length, col = 'blue', xlab = 'Sepal Lenght')
hist(iris$Sepal.Width, col = 'pink', xlab = 'Sepal Width')

#Feature Plots
#gives box plots
library(caret)
featurePlot(x = iris[,1:4], y = iris$Species, plot = 'box',
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation='free'),
                          y = list(relation='free')))
