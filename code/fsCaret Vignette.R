# fscaret
library(fscaret)

# Regression
data("dataset.train")
data("dataset.test")

trainDF <- dataset.train
testDF <- dataset.test

myFS<-fscaret(trainDF, 
              testDF, 
              myTimeLimit = 5, 
              preprocessData = TRUE,
              Used.funcRegPred=c("pcr","pls"), 
              with.labels = TRUE,
              supress.output = TRUE, 
              no.cores=1)
myRES_tab <- myFS$VarImp$matrixVarImp.MSE[1:10,]
myRES_tab <- subset(myRES_tab, select=c("pcr","pls","SUM%","ImpGrad","Input_no"))
# Print out the Variable importance results for MSE scaling
myRES_tab

myRES_rawMSE <- myFS$VarImp$rawMSE
# Print out the generalization error for models
myRES_rawMSE

myRES_PPlabels <- myFS$PPlabels
# Print out the reduced number of inputs after preprocessing
myRES_PPlabels

# Classification
library(MASS)
data(Pima.te)
Pima.te[,8] <- as.numeric(Pima.te[,8])-1
myDF <- Pima.te
myFS.class<-fscaret(myDF, 
                    myDF, 
                    myTimeLimit = 5, 
                    preprocessData = FALSE,
                    with.labels = TRUE, 
                    classPred = TRUE,
                    regPred = FALSE,
                    Used.funcClassPred = c("knn","rpart"), 
                    supress.output = TRUE, 
                    no.cores = 1)
myRES.class_tab <- myFS.class$VarImp$matrixVarImp.MeasureError
myRES.class_tab <- subset(myRES.class_tab, select=c("knn","rpart","SUM%","ImpGrad","Input_no"))
# Print out the Variable importance results for F-measure scaling
myRES.class_tab

myRES.class_rawError <- myFS.class$VarImp$rawMeasureError
# Print out the generalization error for models
myRES.class_rawError
