trainEng <- function(train) {
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
  #hist(train$LotFrontage)
  train$LotFrontage[is.na(train$LotFrontage)] = mean(train$LotFrontage, na.rm = TRUE)
  #hist(train$LotFrontage)
  #filling train$MasVnrArea missing values
  #hist(train$MasVnrArea)
  train$MasVnrArea[is.na(train$MasVnrArea)] = median(train$MasVnrArea, na.rm = TRUE)
  #hist(train$MasVnrArea)
  #filling train$GarageYrBlt missing values
  #hist(train$GarageYrBlt)
  train$GarageYrBlt[is.na(train$GarageYrBlt)] = median(train$GarageYrBlt, na.rm = TRUE)
  #hist(train$GarageYrBlt)
  
  return(train)
}