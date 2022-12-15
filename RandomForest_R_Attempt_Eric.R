
library(tidyverse)
library(caret)
library(leaps)
library(MASS)
print("NEWLINE")

initialData = read.csv('/Users/ericsimon/Downloads/playsADJUSTED_personnelFIXED.csv', stringsAsFactors=FALSE);
#Obviously depends on your file path ^^^

df = initialData;

fit <- lm(df$playResult~as.matrix(df[123:134]))

#install.packages("caret") 
library(caret)
set.seed(333) 
inTrain <- as.vector( caret::createDataPartition( df[,1], p = 0.8, list = FALSE, times = 1 ) ) 
dTrain <- df[inTrain,]; dim(dTrain)

dTest <- df[-inTrain,]; dim(dTest)

yTrain <- dTrain[,120] 
xTrain <- as.matrix(dTrain[[123:134]]) 
yTest <- dTest[,120] 
xTest <- as.matrix(dTest[[123:134]]) 


#install.packages("glmnet")
library(glmnet)

#ridge
par(mfrow  = c(2, 2))
fit.ridge <- glmnet(xTrain, yTrain, alpha=0, standardize=FALSE) 
plot(fit.ridge, label=TRUE, xvar="lambda") 

cv.lasso <- cv.glmnet(xTrain, yTrain, alpha=0, standardize=FALSE, nfolds=10) 
plot(cv.ridge) 

#Lasso
fit.lasso <- glmnet(xTrain, yTrain, alpha=1, standardize=FALSE) 
#plot(fit.lasso, label=TRUE, xvar="lambda") 

cv.lasso <- cv.glmnet(xTrain, yTrain, alpha=1, standardize=FALSE, nfolds=10) 
#plot(cv.lasso) 


#ridge

fit.ridge <- glmnet(xTrain, yTrain, alpha=0.05, standardize=FALSE) 
plot(fit.ridge, label=TRUE, xvar="lambda") 
#set.seed(798) 
cv.ridge <- cv.glmnet(xTrain, yTrain, alpha=0.05, standardize=FALSE, nfolds=10) 
plot(cv.ridge) 

cv.ridge$lambda.min

cv.ridge$lambda.1se

ridge.coef <- coef(cv.ridge, s=cv.ridge$lambda.min) 
ridge.coef 

#NOTE -- some stuff (like below) were for an earlier regression method; ignore
#check the model
sqrt(sum(ridge.coef[-1]^2)) #L2 norm, i.e., Euclidean length

sum(abs(ridge.coef[-1])) #L1 norm

ridge.yTrain <- predict(cv.ridge, newx=xTrain, s=cv.ridge$lambda.min) 
sum((yTrain - ridge.yTrain)^2) #SSE on train se

#log(cv.ridge$lambda.min)

#fit <- lm(df$playResult~as.matrix(df[88:113]))
#step.model <- stepAIC(cv.ridge, direction = "both", trace = FALSE); #This selects the best variables
#summary(step.model)

lasso.coef <- coef(cv.lasso, s=cv.lasso$lambda.min) 
lasso.coef 

step.model <- stepAIC(lasso, direction = "both", trace = FALSE) #This selects the best variables
summary(step.model)


# on a vector
# create an empty vector of zeros
STEP_COEF = vector("numeric",length(coefficients(cv.lasso)))
#same names
names(STEP_COEF) = names(coefficients(cv.lasso))
#fill in the ones found in step
STEP_COEF[names(coefficients(step.model))] = as.numeric(coefficients(step.model))



############# Random Forest:
initialData = read.csv('/Users/ericsimon/Downloads/playsADJUSTED_personnelFIXED.csv', stringsAsFactors=FALSE);
#Obviously depends on your file path ^^^

df = initialData;

# Installing package
install.packages("caTools")       # For sampling the dataset
install.packages("randomForest")  # For implementing random forest algorithm

# Loading package
library(caTools)
library(randomForest)

# Splitting data in train and test data
split <- sample.split(df, SplitRatio = 0.7)
split

train <- subset(df, split == "TRUE")
test <- subset(df, split == "FALSE")

# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[123:134],
                             y = train$playResult,
                             ntree = 5000)

classifier_RF

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[123:134])

# Confusion Matrix
#confusion_mtx = table(test[123:134], y_pred)
#confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)

