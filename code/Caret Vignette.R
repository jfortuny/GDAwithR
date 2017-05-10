library(caret)
library(mlbench)
data(Sonar)

# Splitting the data into training and testing sets
# createDataPartition function
set.seed(107)
inTrain <- createDataPartition(y = Sonar$Class,  ## the outcome data are needed
                               p = 0.75,         ## The percentage of data in the training set
                               list = FALSE)     ## The format of the results
str(inTrain)
training <- Sonar[ inTrain,]
testing <- Sonar[-inTrain,]
dim(training)
dim(testing)

# Train the model
# train function
ctrl <- trainControl(method = "repeatedcv",   ## Use K-fold-cross-validation resampling vd boot(strap default)  
                     repeats = 3,             ## Use 3 repetitions vs 10 (default)
                     summaryFunction = twoClassSummary, ## Method to measure performance
                     classProbs = TRUE)       ## Needed by summaryFunction
plsFit <- train(Class ~ .,
                data = training,
                method = "pls",
                tuneLength = 15,   ## Tune 15 values for the tuning parameters vs 3 (default)
                trControl = ctrl,
                metric = "ROC",
                preProc = c("center", "scale")
)
# review the model results and its recommendations (last line of output)
plsFit
# Visuealize model performance
plot(plsFit)
# Predict
# predict function
plsClasses <- predict(plsFit, newdata = testing)  ## Predict the binary outcome
head(plsClasses)
plsProbs <- predict(plsFit, newdata = testing, type = "prob") ## Predict the probability of outcome
head(plsProbs)
# Verify accuracy
# confusionMatrix function
confusionMatrix(data = plsClasses, testing$Class)

# Try now a Regularized Discriminant model
rdaGrid <- data.frame(gamma = (0:4)/4, lambda = 3/4)
rdaGrid
set.seed(123)
rdaFit <- train(Class ~ .,
                data = training,
                method = "rda",
                tuneGrid = rdaGrid,   ## Use rdaGrid values for the tuning parameters
                trControl = ctrl,
                metric = "ROC"
)
rdaFit
plot(rdaFit)
# Verify accuracy
# confusionMatrix function
rdaClasses <- predict(rdaFit, newdata = testing)
confusionMatrix(rdaClasses, testing$Class)

# Compare model performance
# function resamples
resamps <- resamples(list(pls = plsFit, rda = rdaFit))
summary(resamps)
# visualize the results
xyplot(resamps, what = "BlandAltman")
# Is the difference significant?
diffs <- diff(resamps)
summary(diffs)
