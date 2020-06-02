# Importing libraries
library(datasets) # Contains the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Iris data set
data(iris)

# Checking for missing data
sum(is.na(iris))

# To achieve reproducible model; setting the random seed number
set.seed(100)

# Data Splitting
#Data is split into 2: training set (which is used to create the training model) and this is applied to predict the class label in the testing set
# training set (80%) and testing set(20%)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet <- iris[TrainingIndex,] # Training Set
TestingSet <- iris[-TrainingIndex,] # Test Set

# Compare scatter plot of the 80 and 20 data subsets
# Scatterplot for Training dataset
plot(TrainingSet$Sepal.Width, TrainingSet$Sepal.Length, col='blue', xlab= 'Sepal width', ylab = 'Sepal Length')
plot(TrainingSet$Petal.Width, TrainingSet$Petal.Length, col='blue', xlab='Petal Width', ylab='Sepal Length')
#Scatterplot for Testing dataset
plot(TestingSet$Sepal.Width, TestingSet$Sepal.Length, col='red', xlab= 'Sepal width', ylab = 'Sepal Length')
plot(TestingSet$Petal.Width, TestingSet$Petal.Length, col='red', xlab='Petal Width', ylab='Sepal Length')

###############################
# Building a classification model using a Support Vector Machine Model
#SVM model (polynomial kernel)

library(e1071)
# Build Training model
Model <- train(Species ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV (Cross Validation) model
Model.cv <- train(Species ~ ., data = TrainingSet,
                  method = "svmPoly",
                  na.action = na.omit,
                  preProcess=c("scale","center"),
                  trControl= trainControl(method="cv", number=10),
                  tuneGrid = data.frame(degree=1,scale=1,C=1)
)


# Apply model for prediction
Model.training <-predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing <-predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv <-predict(Model.cv, TrainingSet) # Perform cross-validation

# Evaluating Model Prediction Performance (Displays confusion matrix and statistics)
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Species)

#Looking at the model performance metrics
print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance (tells us which feature was most important for the class prediction)
Importance <- varImp(Model)
plot(Importance)
plot(Importance, col = "red")
