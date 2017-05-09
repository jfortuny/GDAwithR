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
varImp(gbm_model, scale = FALSE)
plot(varImp(gbm_model, scale = FALSE), top=50)
