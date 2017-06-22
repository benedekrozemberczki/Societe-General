library(xgboost)

#---------------------------
# Set the working directory.
#---------------------------

setwd("/home/benedek/Documents/societe/")

#--------------------------
# Reading the datasets.
#--------------------------

train <- read.csv("./clean_dataset/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./clean_dataset/test.csv", stringsAsFactors = FALSE)

#-----------------
# Reading the IDs.
#-----------------

test_ID <- read.csv("./raw_dataset/test.csv", sep = ";", stringsAsFactors = FALSE)
train_ID <- read.csv("./raw_dataset/train.csv", sep = ";", stringsAsFactors = FALSE)

#-----------------------------
# Getting the respective IDs.
#-----------------------------

test_ID <- test_ID$ID
train_ID <- train_ID$ID

#-----------------------------
# Reading the target dataset.
#-----------------------------

target <- read.csv("./raw_dataset/target.csv", sep = ";", stringsAsFactors = FALSE)
target <- target$Target

#-------------------------------------------------
# The training and test datasets are transformed.
#-------------------------------------------------

train[is.na(train)] <- -5000
test[is.na(test)] <- -5000

train <- as.matrix(train)
test <- as.matrix(test)

#------------------------------------
# I perform 5-fold cross-validation.
#------------------------------------

bst <- xgb.cv(data = train,
              label = target,
              eta = 0.05,
              nfold = 5,
              early.stop.round = 3,
              max_depth = 4,
              subsample = 0.9,
              colsample_bytree = 0.1,
              nrounds = 3000,
              min_child_weight = 3,
              objective = "binary:logistic",
              eval_metric = "auc",
              maximize = TRUE)

#------------------------------------------------------
# The predictions are binded to tables with the IDs.
#------------------------------------------------------

test_out <- data.frame(test_ID)
train_out <- data.frame(train_ID)

#--------------------
# I fit 2000 models.
#--------------------

control <- 2000

for (i in 1:control){
  
    bst <- xgboost(data = train,
                   label = target,
                   eta = 0.05,
                   max_depth = 4,
                   subsample = 0.9,
                   colsample_bytree = 0.1,
                   nrounds = 185,
                   min_child_weight = 3,
                   objective = "binary:logistic",
                   eval_metric = "auc",
                   maximize = TRUE)

    test_out <- cbind(test_out, predict(bst, test))
    train_out <- cbind(train_out, predict(bst, train))
}

#-------------------------------------
# The predictions are dumped to disk.
#-------------------------------------

write.table(test_out, "./clean_dataset/test_mixing.csv", sep = ";", row.names = FALSE)
write.table(train_out, "./clean_dataset/train_mixing.csv", sep = ";", row.names = FALSE)
