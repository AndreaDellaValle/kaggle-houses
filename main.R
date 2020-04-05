train <- read.csv2('./input/train.csv', sep = ',', stringsAsFactors = FALSE)
test <- read.csv2('./input/test.csv', sep = ',')

get_mode <- function(x){
  return(names(sort(table(x), decreasing = T, na.last = T)[1]))
}

train$MasVnrType <- replace(train$MasVnrType, which(is.na(train$MasVnrType)),get_mode(train$MasVnrType) )
train$Electrical <- replace(train$Electrical, which(is.na(train$Electrical)),get_mode(train$Electrical) )

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

# cor(train[which(!is.na(train$LotShape))],train[which(!is.na(train$LotFrontage))])

library(corrplot)

corrplot(cor(data.matrix(train)), col = train$LotFrontage)
