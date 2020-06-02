
#Importing required libraries
library(caret)
library(datasets)

#Loading the Iris Dataset
data(dhfr)
View(dhfr)

########################
#Summary Statistics
########################

head(dhfr)
tail(dhfr,5)
summary(dhfr)
summary(dhfr$Y) #checking the imbalance in the dataset

#checking for missing data
sum(is.na(dhfr)) #summation of missing data

#getting a larger set of statistics for the dataset
library(skimr)
skim(dhfr)

#Grouping data by the biological actvity
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()

#Scatter plot
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, xlab = 'moe2D_zagreb', ylab='moe2D_weinerPol')
plot(dhfr$moeGao_Abra_basicity, dhfr$moeGao_Abra_acidity, col='red', xlab= 'Basicity', ylab = 'Acidity')

#Histogram
hist(dhfr$moe2D_zagreb, col = 'blue', xlab = 'moe2D_zagreb')
hist(dhfr$moe2D_weinerPol, col = 'pink', xlab = 'moe2D_weinerPol')

#Feature Plots
#gives box plots and a look at the first 4 variables 
#need to start from the second column because the first one represents the biological activity and can't be used to plot
featurePlot(x = dhfr[,2:21], y = dhfr$Y, plot = 'box',
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation='free'),
                          y = list(relation='free')))
