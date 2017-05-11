# Amunategui
source('~/R Work/GDAwithR/code/StartLibraries.R')

# PCA
# docs 01 and 02 -----------------------------------------------------------
require(graphics)
data("USArrests")
head(USArrests)
str(USArrests)

# Using prcomp
prc <- prcomp(USArrests, scale. = TRUE)
summary(prc)
screeplot(prc)

# eigenvalue of each component (only values > 1 are significant)
prc$sdev^2
# first PCA is significant bur second is close; plot them together
biplot(prc)

# look at the data
USArrests[order(USArrests$UrbanPop, decreasing = TRUE),]

# using princomp
prc <- princomp(USArrests, cor = TRUE, scale = TRUE)
summary(prc)
plot(prc)

biplot(prc)

# docs 03 ------------------------------------------------------------------
# # Gisette dataset
# library(RCurl)
# # data file
# urlfile <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.data'
# x <- getURL(urlfile, ssl.verifypeer = FALSE)
# gisetteRaw <- read.table(textConnection(x), 
#                          sep = '', 
#                          header = FALSE, 
#                          stringsAsFactors = FALSE
#                          )
# # labels/response file
# urlfile <- "https://archive.ics.uci.edu/ml/machine-learning-databases/gisette/GISETTE/gisette_train.labels"
# x <- getURL(urlfile, ssl.verifypeer = FALSE)
# g_labels <- read.table(textConnection(x), 
#                        sep = '', 
#                        header = FALSE, 
#                        stringsAsFactors = FALSE)
# dim(gisetteRaw)
# 
# # Save the data frame and the labels so we don't have to download the data again
# save(gisetteRaw, file = "./data/gisetteRaw.RData")
# save(g_labels, file = "./data/g_labels.RData")

load("./data/gisetteRaw.RData")
load("./data/g_labels.RData")

# Near-Zero Variance variable removal
library(caret)
nzv <- nearZeroVar(gisetteRaw, saveMetrics = TRUE)
paste("Range:", range(nzv$percentUnique))
head(nzv)
# now for the removal of features with less than 0.1% variance
paste("Column count before removal:", ncol(gisetteRaw))
dim(nzv[nzv$percentUnique > 0.1,])
gisette_nzv <- gisetteRaw[c(rownames(nzv[nzv$percentUnique>0.1,]))]
paste("Column count after removal:", ncol(gisette_nzv))

# Combine cleaned data with outcomes (labels)
gisette_df <- cbind(as.data.frame(sapply(gisette_nzv, as.numeric)),
                    cluster=g_labels$V1)

# GBM model with the full data for comparison
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5*nrow(gisette_df)))
traindf <- gisette_df[split,]
testdf <- gisette_df[-split,]

# prepare for caret::train
traindf$cluster <- as.factor(traindf$cluster)
fitControl <- trainControl(method = "none")
model <- train(cluster~., data = traindf,
               tuneGrid = expand.grid(n.trees=50, 
                                      interaction.depth=3, 
                                      shrinkage=0.1, 
                                      n.minobsinnode=10),
               trControl = fitControl,
               method = "gbm",
               metric = "roc")
# predict against test set
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = model,
                       testdf[,setdiff(names(testdf), "cluster")],
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
#Accuracy     Kappa 
#0.9516667 0.9032827 
# 95% Accuracy with the original data

# modeling with PCA (reduced number of variables) rather than the original data (4639 variables)
pmatrix <- scale(gisette_nzv)
princ <- prcomp(pmatrix)

# with only the first PC
n.comp <- 1
dfComponents <- predict(princ, newdata = pmatrix)[,1:n.comp]
gisette_df <- cbind(as.data.frame(dfComponents), cluster=g_labels$V1)
head(gisette_df)

# split the dataset
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5*nrow(gisette_df)))
traindf <- gisette_df[split,]
testdf <- gisette_df[-split,]

traindf$cluster <- as.factor(traindf$cluster)
fitControl <- trainControl(method = "none")
model <- train(cluster~., data = traindf,
               tuneGrid = expand.grid(n.trees=50, 
                                      interaction.depth=3, 
                                      shrinkage=0.1, 
                                      n.minobsinnode=10),
               trControl = fitControl,
               method = "gbm",
               metric = "roc")
# predict against test set
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = model,
                       newdata = data.frame("dfComponents" = testdf[,setdiff(names(testdf), "cluster")]),
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
#Accuracy     Kappa 
#0.7126667 0.4219162

# with the first 2 PCs
n.comp <- 2
dfComponents <- predict(princ, newdata = pmatrix)[,1:n.comp]
gisette_df <- cbind(as.data.frame(dfComponents), cluster=g_labels$V1)
head(gisette_df)

# split the dataset
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5*nrow(gisette_df)))
traindf <- gisette_df[split,]
testdf <- gisette_df[-split,]

traindf$cluster <- as.factor(traindf$cluster)
fitControl <- trainControl(method = "none")
model <- train(cluster~., data = traindf,
               tuneGrid = expand.grid(n.trees=50, 
                                      interaction.depth=3, 
                                      shrinkage=0.1, 
                                      n.minobsinnode=10),
               trControl = fitControl,
               method = "gbm",
               metric = "roc")
# predict against test set
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = model,
                       newdata = testdf[,setdiff(names(testdf), "cluster")],
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
# Accuracy     Kappa 
# 0.7193333 0.4354248 

# with the first 10 PCs
n.comp <- 10
dfComponents <- predict(princ, newdata = pmatrix)[,1:n.comp]
gisette_df <- cbind(as.data.frame(dfComponents), cluster=g_labels$V1)
head(gisette_df)

# split the dataset
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5*nrow(gisette_df)))
traindf <- gisette_df[split,]
testdf <- gisette_df[-split,]

traindf$cluster <- as.factor(traindf$cluster)
fitControl <- trainControl(method = "none")
model <- train(cluster~., data = traindf,
               tuneGrid = expand.grid(n.trees=50, 
                                      interaction.depth=3, 
                                      shrinkage=0.1, 
                                      n.minobsinnode=10),
               trControl = fitControl,
               method = "gbm",
               metric = "roc")
# predict against test set
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = model,
                       newdata = testdf[,setdiff(names(testdf), "cluster")],
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
# Much better accuracy with 10 PCs
# Accuracy     Kappa 
# 0.9300000 0.8599243

# with the first 20 PCs
n.comp <- 20
dfComponents <- predict(princ, newdata = pmatrix)[,1:n.comp]
gisette_df <- cbind(as.data.frame(dfComponents), cluster=g_labels$V1)
head(gisette_df)

# split the dataset
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5*nrow(gisette_df)))
traindf <- gisette_df[split,]
testdf <- gisette_df[-split,]

traindf$cluster <- as.factor(traindf$cluster)
fitControl <- trainControl(method = "none")
model <- train(cluster~., data = traindf,
               tuneGrid = expand.grid(n.trees=50, 
                                      interaction.depth=3, 
                                      shrinkage=0.1, 
                                      n.minobsinnode=10),
               trControl = fitControl,
               method = "gbm",
               metric = "roc")
# predict against test set
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = model,
                       newdata = testdf[,setdiff(names(testdf), "cluster")],
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
# Small improvement from the previous (10 PCs) case
#Accuracy     Kappa 
# 0.9303333 0.8605418 

# docs 04 ------------------------------------------------------------------
load("./data/gisetteRaw.RData")
load("./data/g_labels.RData")
gisette_df <- cbind(as.data.frame(sapply(gisetteRaw, as.numeric)), cluster=g_labels$V1)

# divide the data into TraintAndTest and Validate
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5 * nrow(gisette_df)))
gisette_df_train_test <- gisette_df[split,]
gisette_df_validate <- gisette_df[-split,]

# divide TraintAndTest into train and test
set.seed(1234)
split <- sample(nrow(gisette_df_train_test), floor(0.5 * nrow(gisette_df_train_test)))
traindf <- gisette_df_train_test[split,]
testdf <- gisette_df_train_test[-split,]

# find suitable variables with GBM
traindf$cluster <- ifelse(traindf$cluster == 1, "yes", "no")
traindf$cluster <- as.factor(traindf$cluster)

fitControl <- trainControl(method = "cv",
                           number = 3,
                           returnResamp = "none",
                           verboseIter = FALSE,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE
)
gbm_model <- train(cluster~.,
                   data = traindf,
                   trControl = fitControl,
                   method = "gbm",
                   metric = "roc")
gbm_model
# The final values used for the model were n.trees = 150, interaction.depth = 3, 
# shrinkage = 0.1 and n.minobsinnode = 10.

testdf$cluster <- ifelse(testdf$cluster == 1, "yes", "no")
testdf$cluster <- as.factor(testdf$cluster)
predictions <- predict(object = gbm_model,
                       testdf[,setdiff(names(testdf), "cluster")], 
                       type = "raw")
head(predictions)
postResample(pred = predictions, obs = testdf$cluster)
# Accuracy     Kappa 
# 0.9586667 0.9172863 

varImp(gbm_model, scale = FALSE)
plot(varImp(gbm_model, scale = FALSE), top=20)

# we can save the varImp results in a data frame so we can use them
vimp <- varImp(gbm_model, scale = FALSE)
results <- data.frame(row.names(vimp$importance), vimp$importance$Overall)
head(results)
results$VariableName <- row.names(vimp)
colnames(results) <- c("VariableName", "Weight")
results <- results[order(results$Weight),]
head(results)
str(results)
results$VariableName <- as.character(results$VariableName)

# let's display the best 20 features
results_temp <- tail(results,20)
par(mar=c(5,5,4,2))
xx <- barplot(results_temp$Weight, 
              width = 0.85,
              main = paste("Variable Importance -",'cluster'), 
              horiz = T,
              xlab = "< (-) importance > < neutral > < importance (+) >", 
              axes = FALSE,
              col = ifelse((results_temp$Weight > 0), 'blue', 'red'))
axis(2, 
     at=xx, 
     labels=results_temp$VariableName, 
     tick=FALSE, 
     las=2, 
     line=-0.3, 
     cex.axis=0.6)   

# We can now try a model with the best features and see how many 
# we need to achieve similar accuracy (although now we'll test accuracy
# against the validation dataset)
tail(results, 100)
# Let's use the results with a Weight > 2
traindf_truncated <- traindf[, c(results$VariableName[results$Weight > 2], 'cluster')]
dim(traindf_truncated)
# Fit the model
fitControl <- trainControl(method="none")
gbm_model <- train(cluster~., 
                   data=traindf_truncated,
                   tuneGrid = expand.grid(n.trees = 150, 
                                          interaction.depth = 3, 
                                          shrinkage = 0.1, 
                                          n.minobsinnode=10),
                   trControl=fitControl, 
                   method="gbm", 
                   metric='roc'
                   )
# now predict
predictions <- predict(object=gbm_model, 
                       gisette_df_validate[,setdiff(names(traindf_truncated), 'cluster')], 
                       type='raw')
# Verify accuracy
head(predictions)
head(gisette_df_validate$cluster)
# We need to convert gisette_df_validate$cluster to "yes" and "no"
postResample(pred=predictions, obs=as.factor(ifelse(gisette_df_validate$cluster == 1,"yes", "no")))
# Not too bad
# Accuracy     Kappa 
# 0.9536667 0.9072691 


# docs 05 ------------------------------------------------------------------
load("./data/gisetteRaw.RData")
load("./data/g_labels.RData")
gisette_df <- cbind(as.data.frame(sapply(gisetteRaw, as.numeric)), cluster=g_labels$V1)

# divide the data into TraintAndTest and Validate
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5 * nrow(gisette_df)))
gisette_df_train_test <- gisette_df[split,]
gisette_df_validate <- gisette_df[-split,]

# divide TraintAndTest into train and test
set.seed(1234)
split <- sample(nrow(gisette_df_train_test), floor(0.5 * nrow(gisette_df_train_test)))
traindf <- gisette_df_train_test[split,]
testdf <- gisette_df_train_test[-split,]

# set the outcome and predictors' names
outcome_name <- 'cluster'
predictors_names <- setdiff(names(traindf), outcome_name)
# caret requires a factor of non-numeric value
traindf$cluster <- ifelse(traindf$cluster == 1, "yes", "no")
traindf$cluster <- as.factor(traindf$cluster )
# prepare for the model
fitControl <- trainControl(method = "cv",
                           number = 3,
                           returnResamp = "none",
                           verboseIter = FALSE,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE
)
glmnet_model <- train(
          x=traindf[,predictors_names], y=traindf[,outcome_name],
          data = traindf,
          trControl = fitControl,
          method = "glmnet",
          metric = "ROC"
          # ,maxit = 10^6
)
# Couldn't fit the model glmnet_model; errored out:
# Error in glmnet(x = c(0, 0, 805, 0, 0, 0, 0, 836, 0, 0, 0, 0, 0, 878,  : 
#                         unused argument (data = list(V1 = c(0, 0, 805, 0, 0, 0, 0, 836, 0, 0, 0, 0, 0, 878, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 666, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 987, 0, 0, 983, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 991, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 976, 0, 0, 0, 0, 0, 0, 0, 0, 980, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
#                                                             0, 0, 0, 0, 0, 0, 0, 805, 0, 614, 0, 0, 0, 0, 0, 0, 0, 601, 0, 0, 0, 0, 0, 0, 537, 0, 0, 0, 0, 0, 284, 0, 0, 944, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 722, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 501, 0, 0, 0, 0, 0, 0, 0, 539, 0, 0, 0, 0, 0, 991, 711, 0, 0, 403, 0, 0, 0, 983, 0, 0, 0, 620, 0, 0, 983, 0, 0, 0, 987, 0, 0, 983, 0, 0, 0, 0, 0, 0, 499, 0, 0, 444, 0, 0, 0, 0, 432, 0, 0, 0, 0, 0, 797, 0, 0, 0, 0, 0, 0, 0, 705, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 991, 0, 0, 0, 0, 0, 0, 0, 0, 0
#                                                             In addition: There were 12 warnings (use warnings() to see them)
#                                                             Timing stopped at: 171.25 0 171.25 


# docs 06 ------------------------------------------------------------------
load("./data/gisetteRaw.RData")
load("./data/g_labels.RData")
gisette_df <- cbind(as.data.frame(sapply(gisetteRaw, as.numeric)), cluster=g_labels$V1)
dim(gisette_df)

# de-duplicate columns
gisette_df <- gisette_df[!duplicated(lapply(gisette_df, summary))]
dim(gisette_df)
# re-format outcome to 0,1 from -1,1
gisette_df$cluster <- ifelse(gisette_df$cluster==-1,0,1)

# divide the data into TraintAndTest and Validate
set.seed(1234)
split <- sample(nrow(gisette_df), floor(0.5 * nrow(gisette_df)))
gisette_df_train_test <- gisette_df[split,]
gisette_df_validate <- gisette_df[-split,]

# divide TraintAndTest into train and test
set.seed(1234)
split <- sample(nrow(gisette_df_train_test), floor(0.5 * nrow(gisette_df_train_test)))
traindf <- gisette_df_train_test[split,]
testdf <- gisette_df_train_test[-split,]

# call the mRMRe package
library(mRMRe)
mRMR_data <- mRMR.data(data = traindf)
#mRMR_data
feats <- mRMR.classic(data = mRMR_data, 
                      target_indices = c(ncol(traindf)), # outcome variable
                      feature_count = 20)
bestVars <-data.frame('features'=names(traindf)[solutions(feats)[[1]]], 'scores'= scores(feats)[[1]])
bestVars

# fit with model glmnet (using caret)
# subset the variables in the training datadrame to the best 20 found in bestVars
# and append the outcome
traindf_temp <- traindf[c(as.character(bestVars$features), 'cluster')]
names(traindf_temp)
# convert the outcome to non-numeric factor (caret requires it)
traindf_temp$cluster <- ifelse(traindf_temp$cluster == 1, "yes", "no")
traindf_temp$cluster <- as.factor(traindf_temp$cluster )
# prepare for the model
objControl <- trainControl(method = "cv",
                           number = 3,
                           returnResamp = "none",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE
)
glmnet_model <- train(
  cluster~.,
  data = traindf_temp,
  trControl = objControl,
  method = "glmnet",
  metric = "ROC"
)
glmnet_model
# predict
glmnet_predictions <- predict(object=glmnet_model, 
                              newdata= gisette_df_validate[,as.character(bestVars$features)], 
                              type='raw')
# Verify accuracy
head(glmnet_predictions)
head(gisette_df_validate$cluster)
# We need to convert gisette_df_validate$cluster to "yes" and "no"
postResample(pred=glmnet_predictions, 
             obs=as.factor(ifelse(gisette_df_validate$cluster == 1,"yes", "no")))
# Not too bad
# Accuracy     Kappa 
# 0.9173333 0.8346890

# now fit with model knn
knn_model <- train(       # name the model differently than glmnet
  cluster~.,
  data = traindf_temp,
  trControl = objControl, # this has not changed from the glmnet run
  method = "knn",         # this is all that changed
  metric = "ROC"
)
knn_model
# predict
knn_predictions <- predict(object=knn_model, 
                           newdata= gisette_df_validate[,as.character(bestVars$features)], 
                           type='raw')
# Verify accuracy
head(knn_predictions)
head(gisette_df_validate$cluster)
# We need to convert gisette_df_validate$cluster to "yes" and "no"
postResample(pred=knn_predictions, 
             obs=as.factor(ifelse(gisette_df_validate$cluster == 1,"yes", "no")))
# Even better than glmnet
# Accuracy     Kappa 
# 0.9306667 0.8613708 

# use the ensemble method of the mRMRe library
# ensemble example
feats <- mRMR.ensemble(data = mRMR_data, 
                       target_indices = c(ncol(traindf)),
                       solution_count = 5,
                       feature_count = 10)
bestVars <-data.frame('features'=names(traindf)[solutions(feats)[[1]]], 
                      'scores'= scores(feats)[[1]])
bestVars
