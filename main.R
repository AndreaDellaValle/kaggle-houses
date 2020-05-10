library(rpart)
library("plyr")
library('mice')
library(foreach)
library('caret')
library('dplyr')
library('e1071')
library('GGally')
library('onehot')
library(xgboost)
library('scales')
library('ggplot2')
library(rpart.plot)
library('corrplot')
library('ggthemes')
library('gridExtra')
library('data.table')
library('randomForest')

source('./utils/getMode.R')
source('./utils/solveSkewness.R')
source('./utils/removeNumericalCorr.R')
source('./utils/scaleNumericalData.R')
source('./utils/trainFeatureEng.R')
source('./utils/testFeatureEng.R')
source('./utils/greatestCor.R')
source('./utils/plots.R')

train <- read.csv2('./input/train.csv', sep = ',', stringsAsFactors = FALSE)
test <- read.csv2('./input/test.csv', sep = ',', stringsAsFactors = FALSE)

train <- trainEng(train)
test <- testEng(test)

train <- train %>% select(-Id)
test <- test %>% select(-Id)

######################### Data Ready ####################

# Split data between numerical and categorical
cat_var <- names(train)[which(sapply(train, is.character))]
numeric_var <- names(train)[which(sapply(train, is.numeric))]

train1_cat <- train[cat_var]
train1_num <- train[numeric_var]

salePriceNotSkewed <- log(train1_num$SalePrice)

hist(train1_num$SalePrice)
hist(salePriceNotSkewed)

train1_num_no_saleprice <- train1_num %>% select(-SalePrice)
# Find correlations between numerical data (greates or 5 greater)
numCor <- cor(train1_num_no_saleprice, train1_num$SalePrice)

# See if there are outliers in the plot
plot(train1_num$GarageArea, train1_num$SalePrice)

plot(train1_num$GarageArea, train1_num$SalePrice)

# Remove the outliers
train1_num_step1 <- train1_num[train1_num$GrLivArea<4500,]


# One hot encoding for categorical variables
dmy <- dummyVars(" ~ .", data = train1_cat, fullRank=TRUE)
encoded_cat <- data.frame(predict(dmy, newdata = train1_cat))

# Bind the new one-hot encoded data with the other numerical data
wholedata <- cbind(train1_num, encoded_cat)



# Data exploration

firstCor <- cor(wholedata[c(38, 2:30)], use="everything")
secondCor <- cor(wholedata[c(38, 31:60)], use="everything")
thirdCor <- cor(wholedata[c(38, 61:90)], use="everything")
fourthCor <- cor(wholedata[c(38, 91:120)], use="everything")
fifthCor <- cor(wholedata[c(38, 121:150)], use="everything")
sixthCor <- cor(wholedata[c(38, 151:180)], use="everything")
seventhCor <- cor(wholedata[c(38, 181:210)], use="everything")
eightCor <- cor(wholedata[c(38, 211:240)], use="everything")
ninehtCor <- cor(wholedata[c(38, 241:261)], use="everything")

corrplot(firstCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(secondCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(thirdCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(fourthCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(fifthCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(sixthCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(seventhCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(eightCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")
corrplot(ninehtCor, method="number", type="lower",  sig.level = 0.01, insig = "blank")


ggplot(train, aes(x = Neighborhood, y = SalePrice)) +
  geom_boxplot() +
  geom_hline(aes(yintercept=80), 
             colour='red', linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format()) +
  theme_few()


doPlots(train1_num, fun = plotDen, ii = 7:9, ncol = 2)



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
