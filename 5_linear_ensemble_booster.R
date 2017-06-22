library(xgboost)

#--------------------------------
# Setting the working directory.
#--------------------------------

setwd("/home/benedek/Documents/societe/")

#---------------------------------------------
# Reading the datasets used for the stacking.
#---------------------------------------------

train <- read.csv("./clean_dataset/train_mixing.csv", stringsAsFactors = FALSE, sep = ";")
test <- read.csv("./clean_dataset/test_mixing.csv", stringsAsFactors = FALSE, sep = ";")

#------------------
# Setting the IDs.
#------------------

test_ID <- test[,1]
train_ID <- train[,1]

#--------------------
# Reading the target.
#--------------------

target <- read.csv("./raw_dataset/target.csv", sep = ";", stringsAsFactors = FALSE)
target <- target$Target

#-------------------------------------------
# Transforming the train and test dataset.
#-------------------------------------------

train <- as.matrix(train[,2:ncol(train)])
test <- as.matrix(test[,2:ncol(test)])

#----------------------------------------------------------------------------------
# Performing cross-validation with a linear booster -- it is a nice mixing method.
#----------------------------------------------------------------------------------

bst <- xgb.cv(data = train,
              nfold = 5,
              early.stop.round = 3,
              label = target,
              eta = 0.1,
              subsample = 1,
              colsample_bytree = 0.1,
              nrounds = 2000,
              alpha = 0.5,
              lambda = 0.5,
              objective = "binary:logistic",
              booster = "gblinear",
              eval_metric = "auc",
              maximize = TRUE)

#---------------------
# Fitting the mixer.
#---------------------

bst <- xgboost(data = train,
               label = target,
               eta = 0.1,
               subsample = 1,
               colsample_bytree = 0.1,
               nrounds = 200,
               alpha = 0.5,
               lambda = 0.5,
               objective = "binary:logistic",
               booster = "gblinear",
               eval_metric = "auc",
               maximize = TRUE)

#-----------------------------------------------------
# Doing the predictions and dumping results to disk.
#-----------------------------------------------------

yhat <- predict(bst, test)
out <- cbind(test_ID, yhat)
colnames(out) <- c("ID", "TARGET")
write.table(out, "predictions.csv", row.names = FALSE, sep = ";")