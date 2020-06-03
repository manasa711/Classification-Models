# Importing libraries
library(datasets) # Contains the Iris data set
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression Training

# Importing the Iris data set
data("dhfr")

# Checking for missing data
sum(is.na(dhfr))

# To achieve reproducible model; setting the random seed number
set.seed(100)

# Data Splitting
#Data is split into 2: training set (which is used to create the training model) and this is applied to predict the class label in the testing set
# training set (80%) and testing set(20%)

# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet <- dhfr[TrainingIndex,] # Training Set
TestingSet <- dhfr[-TrainingIndex,] # Test Set

# Building a classification model using a Support Vector Machine Model
#SVM model (polynomial kernel)

library(e1071)
# Build Training model
Model <- train(Y ~ ., data = TrainingSet,
               method = "svmPoly",
               na.action = na.omit,
               preProcess=c("scale","center"),
               trControl= trainControl(method="none"),
               tuneGrid = data.frame(degree=1,scale=1,C=1)
)

# Build CV (Cross Validation) model
Model.cv <- train(Y ~ ., data = TrainingSet,
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
Model.training.confusion <-confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion <-confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion <-confusionMatrix(Model.cv, TrainingSet$Y)

#Looking at the model performance metrics
print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance (tells us which feature was most important for the class prediction)
Importance <- varImp(Model)
plot(Importance)
plot(Importance, top=25) #Visualizing top 25 factors which influence the prediction
