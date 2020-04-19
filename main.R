library(corrplot)
require('caret')
library(rpart)
library(rpart.plot)
library('ggplot2')
library('ggthemes') 
library('scales')
library('dplyr') 
library('mice')
library('randomForest') 
library('data.table')
library('gridExtra')
library('corrplot') 
library('GGally')
library('e1071')

train <- read.csv2('./input/train.csv', sep = ',', stringsAsFactors = FALSE)
test <- read.csv2('./input/test.csv', sep = ',', stringsAsFactors = FALSE)

get_mode <- function(value){
  return(names(sort(table(value), decreasing = T, na.last = T)[1]))
}

################### TRAIN ####################
train$MasVnrType <- replace(train$MasVnrType, which(is.na(train$MasVnrType)), get_mode(train$MasVnrType) )
train$Electrical <- replace(train$Electrical, which(is.na(train$Electrical)), get_mode(train$Electrical) )

train$BsmtExposure[which(is.na(train$BsmtExposure))] = 'No basement'
train$BsmtQual[which(is.na(train$BsmtQual))] = 'No basement'
train$BsmtCond[which(is.na(train$BsmtCond))] = 'No basement'
train$BsmtFinType1[which(is.na(train$BsmtFinType1))] = 'No basement'
train$BsmtFinType2[which(is.na(train$BsmtFinType2))] = 'No basement'
train$FireplaceQu[which(is.na(train$FireplaceQu))] = 'No fireplace'
train$GarageType[which(is.na(train$GarageType))] = 'No garage'
train$GarageFinish[which(is.na(train$GarageFinish))] = 'No garage'
train$GarageQual[which(is.na(train$GarageQual))] = 'No garage'
train$GarageCond[which(is.na(train$GarageCond))] = 'No garage'
train$PoolQC[which(is.na(train$PoolQC))] = 'No pool'
train$Fence[which(is.na(train$Fence))] = 'No Fence'
train$MiscFeature[which(is.na(train$MiscFeature))] = 'None'
train$Alley[which(is.na(train$Alley))] = 'No alley access'

colnames(train)[ apply(train, 2, anyNA) ]
#colnames(test)[ apply(test, 2, anyNA) ]

#filling train$LotFrontage missing values
hist(train$LotFrontage)
train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage, na.rm = TRUE)
hist(train$LotFrontage)
#filling train$MasVnrArea missing values
hist(train$MasVnrArea)
train$MasVnrArea[is.na(train$MasVnrArea)] = median(train$MasVnrArea, na.rm = TRUE)
hist(train$MasVnrArea)
#filling train$GarageYrBlt missing values
hist(train$GarageYrBlt)
train$GarageYrBlt[is.na(train$GarageYrBlt)] = median(train$GarageYrBlt, na.rm = TRUE)
hist(train$GarageYrBlt)

################### TEST ####################
test$MasVnrType <- replace(test$MasVnrType, which(is.na(test$MasVnrType)), get_mode(test$MasVnrType) )
test$Electrical <- replace(test$Electrical, which(is.na(test$Electrical)), get_mode(test$Electrical) )

test$BsmtExposure[which(is.na(test$BsmtExposure))] = 'No basement'
test$BsmtQual[which(is.na(test$BsmtQual))] = 'No basement'
test$BsmtCond[which(is.na(test$BsmtCond))] = 'No basement'
test$BsmtFinType1[which(is.na(test$BsmtFinType1))] = 'No basement'
test$BsmtFinType2[which(is.na(test$BsmtFinType2))] = 'No basement'
test$FireplaceQu[which(is.na(test$FireplaceQu))] = 'No fireplace'
test$GarageType[which(is.na(test$GarageType))] = 'No garage'
test$GarageFinish[which(is.na(test$GarageFinish))] = 'No garage'
test$GarageQual[which(is.na(test$GarageQual))] = 'No garage'
test$GarageCond[which(is.na(test$GarageCond))] = 'No garage'
test$PoolQC[which(is.na(test$PoolQC))] = 'No pool'
test$Fence[which(is.na(test$Fence))] = 'No Fence'
test$MiscFeature[which(is.na(test$MiscFeature))] = 'None'
test$Alley[which(is.na(test$Alley))] = 'No alley access'
#filling test$LotFrontage missing values
hist(test$LotFrontage)
test$LotFrontage[is.na(test$LotFrontage)] = mean(test$LotFrontage, na.rm = TRUE)
hist(test$LotFrontage)
#filling test$MasVnrArea missing values
hist(test$MasVnrArea)
test$MasVnrArea[is.na(test$MasVnrArea)] = median(test$MasVnrArea, na.rm = TRUE)
hist(test$MasVnrArea)
#filling test$GarageYrBlt missing values
hist(test$GarageYrBlt)
test$GarageYrBlt[is.na(test$GarageYrBlt)] = median(test$GarageYrBlt, na.rm = TRUE)
hist(test$GarageYrBlt)
# for bsm we set the value to 0 if 'no basement' else we use the median
test$BsmtFinSF2[is.na(test$BsmtFinSF2)&test$BsmtCond == 'No basement'] = 0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)&test$BsmtCond != 'No basement'] = median(test$BsmtFinSF2)
test$TotalBsmtSF[is.na(test$TotalBsmtSF)&test$BsmtCond == 'No basement'] = 0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)&test$BsmtCond != 'No basement'] = median(test$TotalBsmtSF)
test$BsmtUnfSF[is.na(test$BsmtUnfSF)&test$BsmtCond == 'No basement'] = 0
test$BsmtUnfSF[is.na(test$BsmtUnfSF)&test$BsmtCond != 'No basement'] = median(test$BsmtUnfSF)
test$BsmtFinSF1[is.na(test$BsmtFinSF1)&test$BsmtCond == 'No basement'] = 0
test$BsmtFinSF1[is.na(test$BsmtFinSF1)&test$BsmtCond != 'No basement'] = median(test$BsmtFinSF1)
test$BsmtFullBath[is.na(test$BsmtFullBath)&test$BsmtCond == 'No basement'] = 0
test$BsmtFullBath[is.na(test$BsmtFullBath)&test$BsmtCond != 'No basement'] = median(test$BsmtFullBath)
test$BsmtHalfBath[is.na(test$BsmtHalfBath)&test$BsmtCond == 'No basement'] = 0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)&test$BsmtCond != 'No basement'] = median(test$BsmtHalfBath)

test$GarageArea[is.na(test$GarageArea)&test$GarageCond == 'No garage'] = 0
test$GarageArea[is.na(test$GarageArea)&test$GarageCond != 'No garage'] = median(test$GarageArea)
test$GarageCars[is.na(test$GarageCars)&test$GarageCond == 'No garage'] = 0
test$GarageCars[is.na(test$GarageCars)&test$GarageCond != 'No garage'] = median(test$GarageCars)
#replace missing values in MSZoning column in test set with RL (most frequently occuring)
test$MSZoning[is.na(test$MSZoning)] = get_mode(test$MSZoning)
#replace missing values in Utilities column in test set with AllPub (most frequently occuring)
test$Utilities[is.na(test$Utilities)] = get_mode(test$Utilities)
#replace missing values in Exterior1st column in test set with VinylSd (most frequently occuring)
test$Exterior1st[is.na(test$Exterior1st)] = get_mode(test$Exterior1st)
#replace missing values in Exterior2nd column in test set with VinylSd (most frequently occuring)
test$Exterior2nd[is.na(test$Exterior2nd)] = get_mode(test$Exterior2nd)
#replace missing value in KitchenQual column in test set with TA (most common)
test$KitchenQual[is.na(test$KitchenQual)] = get_mode(test$KitchenQual)
#replace missing values in Functional column in test set with Min2 (most common)
test$Functional[is.na(test$Functional)] = get_mode(test$Functional)
#replace missing value in SaleType column in test set with WD (most common)
test$SaleType[is.na(test$SaleType)] = get_mode(test$SaleType)

######################### Data Ready ####################

cat_var <- names(train)[which(sapply(train, is.character))]













# set.seed(42)
# # First random forest
# myControl = trainControl(method = "cv", number = 5, verboseIter = FALSE)
# model_rf = train(SalePrice ~ .,
#                  data = train,
#                  tuneLength = 1,
#                  method = "ranger",
#                  importance = 'impurity',
#                  trControl = myControl)
# model_rf
# # Second random forest
# model_rf2 = train(SalePrice ~ .,
#                  data = train,
#                  tuneLength = 2,
#                  method = "ranger",
#                  importance = 'impurity',
#                  trControl = myControl)
# model_rf2
# 
# plot(model_rf2)
# 
# # Linear Regression
# model_lm = train(SalePrice ~ .,
#                  data = train,
#                  method = "lm",
#                  trControl = myControl)
# 
# model_lm
# 
# plot(model_lm)
# 
# model_list <- list(lm = model_lm, rf = model_rf)
# resamples = resamples(model_list)
# summary(resamples)
# 
# bwplot(resamples, metric = "RMSE")
# rm(resamples, model_list)
