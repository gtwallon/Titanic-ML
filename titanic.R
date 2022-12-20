#### Titanic ####

library(knitr)
library(xgboost)
library(readr)
library(stringr)
library(randomForest)
library(dplyr)
setwd("/Users/gabrielwallon/Desktop/R")

trainData <- read_csv("titanic_train.csv")
testData <- read_csv("titanic_test.csv")
head(trainData)

--------------------------------------------------------------------

#### EDA #### 

## Exclude from model: PassengerId, Name, Ticket, Cabin, Embarked

head(trainData)

sum(trainData$Survived) / dim(trainData)[1]     # total survival rate

## Identifying NA values in "Cabin" 
length(trainData$Cabin[is.na(trainData$Cabin)])
trainData$Cabin[!is.na(trainData$Cabin)]
trainData$Cabin %>% unique()


## Checking woman vs man survival rates
totalWomen = sum(trainData$Sex == "female")
survivedWomen = sum(trainData[which(trainData$Sex == "female"), "Survived"])
womanSurvivalRate = survivedWomen/totalWomen

totalMen = sum(trainData$Sex == "male")
survivedMen = sum(trainData[which(trainData$Sex == "male"), "Survived"])
manSurvivalRate = survivedMen/totalMen


## Check percent from each tclass that perished
totalClass3 = sum(trainData$Pclass == 3)
totalClass2 = sum(trainData$Pclass == 2)
totalClass1 = sum(trainData$Pclass == 1)

# class 3
class3_survivors = trainData %>% filter(
    trainData$Pclass == 3,
    trainData$Survived == 1)
survivedClass3 = dim(class3_survivors)[1]  # number of survivors from class 3
rateClass3 = survivedClass3 / totalClass3
rateClass3
# class 2
class2_survivors = trainData %>% filter(
  trainData$Pclass == 2,
  trainData$Survived == 1)
survivedClass2 = dim(class2_survivors)[1]  # number of survivors from class 2
rateClass2 = survivedClass2 / totalClass2
rateClass2
# class 1
class1_survivors = trainData %>% filter(
  trainData$Pclass == 1,
  trainData$Survived == 1)
survivedClass1 = dim(class1_survivors)[1]  # number of survivors from class 1
rateClass1 = survivedClass1 / totalClass1
rateClass1


# explore relation between PClass and Fare
## Can see there is not a very strong correlation, many 2nd class passengers paid less than
### third class for their tickets, therefore the Fare variable holds non-redundant,
#### potentially useful information
plot(
  trainData$Pclass,
  trainData$Fare
)


# finding variable correlations
selected = trainData %>% mutate(
  sexMale = as.numeric(Sex == "male"),
  Age = impute_na(Age)
)%>% select(
  -PassengerId,
  -Name,
  -Ticket,
  -Cabin,
  -Embarked,
  -Sex,
  -Pclass
) 

oheTrainPclass = ohe_column_to_mat(trainData$Pclass, "Pclass")

trainDesMat = cbind.data.frame(
  selected,
  oheTrainPclass
)

cor(trainDesMat)[,1] %>% as_tibble() %>% colnames("Survived")

tibble(
  "Variables" = colnames(trainDesMat),
  "Survived" = cor(trainDesMat)[,1]
)

--------------------------------------------------------------------

#### Linear and Logistic Models ####
# check for NAs
apply(is.na(newTrainDf), MARGIN = 2, any)
apply(is.na(newTestDf), MARGIN = 2, any)

##change variables for the train and test data sets
# train
selectTrainDf = trainData %>% mutate(
    sexMale = as.numeric(Sex == "male"),
    Age = impute_na(Age)
)%>% select(
    -PassengerId,
    -Name,
    -Ticket,
    -Cabin,
    -Embarked,
    -Sex,
    -Pclass
) 

oheTrainPclassMat = ohe_column_to_mat(trainData$Pclass, "Pclass", TRUE)

newTrainDf = cbind.data.frame(
    selectTrainDf,
    oheTrainPclassMat
)
head(newTrainDf)

#test
selectTestDf = testData %>% mutate(
  sexMale = as.numeric(Sex == "male"),
  Age = impute_na(Age),
  Fare = impute_na(Fare)
)%>% select(
  -PassengerId,
  -Name,
  -Ticket,
  -Cabin,
  -Embarked,
  -Sex,
  -Pclass
) 

oheTestPclassMat = ohe_column_to_mat(testData$Pclass, "Pclass", TRUE)

head(oheTestPclassMat)

newTestDf = cbind.data.frame(
  selectTestDf,
  oheTestPclassMat
)
head(newTestDf)


# check again for NAs
apply(is.na(newTrainDf), MARGIN = 2, any)
apply(is.na(newTestDf), MARGIN = 2, any)


# making models
linModel = lm(newTrainDf$Survived ~ ., data = newTrainDf)
summary(linModel)
# linear model is not the best thing to use in this case because we are trying to predict
## a discrete categories, not numbers


logit_1 = glm(newTrainDf$Survived ~ ., data = newTrainDf, family = "binomial")
summary(logit_1)
preds_1 = predict(logit_1, newTestDf)    # can be transformed into probabilities
rounded_preds_1 = round(logit(preds_1)) # assume survival if predicted val >= .5

# make Kaggle submission
logit_1_preds = cbind(
  PassengerId = testData$PassengerId,
  Survived = rounded_preds_1
) %>% as_tibble()
# write_csv(logit_1_preds, "titanic_answer1.csv")
dim(logit_1_preds)
# logit_1_preds has a .76 rate of accuracy


logit_2 = glm(newTrainDf$Survived ~ sexMale + Age + Pclass_1 + Pclass_2, data = newTrainDf, family = "binomial")
summary(logit_2)
preds_2 = predict(logit_2, newTestDf)
rounded_preds_2 = round(logit(preds_2))

# make Kaggle submission
logit_2_preds = cbind(
  PassengerId = testData$PassengerId,
  Survived = rounded_preds_2
) %>% as_tibble()
# write_csv(logit_2_preds, "titanic_answer2.csv")
dim(logit_2_preds)
# logit_3_preds has a .77 rate of accuracy


logit_3 = glm(newTrainDf$Survived ~ sexMale + Pclass_1 + Pclass_2, data = newTrainDf, family = "binomial")
summary(logit_3)
preds_3 = predict(logit_3, newTestDf)
rounded_preds_3 = round(logit(preds_3))

# make Kaggle submission
logit_3_preds = cbind(
  PassengerId = testData$PassengerId,
  Survived = rounded_preds_3
) %>% as_tibble()
# write_csv(logit_3_preds, "titanic_answer4.csv")
dim(logit_3_preds)
# logit_2_preds has a .76 rate of accuracy


logit_4 = glm(newTrainDf$Survived ~ sexMale + Pclass_1 + Pclass_2 + 0, data = newTrainDf, family = "binomial")
summary(logit_4)
preds_4 = predict(logit_4, newTestDf)
rounded_preds_4 = round(logit(preds_4))

# make Kaggle submission
logit_4_preds = cbind(
  PassengerId = testData$PassengerId,
  Survived = rounded_preds_4
) %>% as_tibble()
# write_csv(logit_4_preds, "titanic_answer5.csv")
dim(logit_4_preds)
# logit_4_preds has a .78 rate of accuracy

--------------------------------------------------------------------

#### Random Forest ####
head(newTrainDf)
rfModel_1 = randomForest(as.factor(Survived) ~ .,
                       data = newTrainDf,
                       proximity=TRUE,
                       ntree = 10000)
rf_preds_1 = predict(rfModel_1, newTestDf)
rf_submission_1 = tibble(
  PassengerId = testData$PassengerId,
  Survived = rf_preds) 
# write_csv(rf_submission, "titanic_answer3.csv")
# rfModel_1 had an accuracy of .78


rfModel_2 = randomForest(as.factor(Survived) ~ .,
                         data = newTrainDf,
                         proximity=TRUE,
                         ntree = 5000)
rf_preds_2 = predict(rfModel_2, newTestDf)
rf_submission_2 = tibble(
  PassengerId = testData$PassengerId,
  Survived = rf_preds) 
# write_csv(rf_submission, "titanic_answer6.csv")
# rfModel_2 had an accuracy of .78


#### trying to build RF with parallel computing 

# install.packages("foreach")
# install.packages("iterators")
# install.packages("parallel")
# install.packages("doParallel")
library(foreach)
library(iterators)
library(parallel)
library(doParallel)
library(abind)

nCores = detectCores() - 1
myCluster <- makeCluster(spec = nCores, type = "PSOCK")
registerDoParallel(cl = myCluster)
{
  start_time = Sys.time()
      rfModel = randomForest(as.factor(Survived) ~ .,
                         data = newTrainDf,
                         proximity=TRUE,
                         ntree = 10000)
  stop_time = Sys.time()
  stop_time - start_time
}
stopCluster(cl = myCluster)

--------------------------------------------------------------------

#### XGBoost Model Attempt ####
xgbDesMat = xgboost::xgb.DMatrix(newTrainDf)

training_x = model.matrix(Survived ~ ., 
                          data = newTrainDf)

xgb <- xgboost::xgboost(data = data.matrix(training_x), 
                        label = newTrainDf$Survived,
                        eta = 0.1,
                        max_depth = 10,
                        nrounds = 25,
                        obj = logit
)
xgb_preds = predict(xgb, newTestDf)
summary(xgb)


#### Functions ####

# lbind
lbind <- function(x, y){
  abind(x, y, along = 3)
}

# logistic model transformation function
logit <- function(x){1/(1 + exp(-x))}

# function to change column NA values
impute_na <- function(col, method = "median", specified = NULL){
  if(method == "mean"){
      value = mean(col, na.rm = TRUE)
  } else if(method == "median"){
      value = median(col, na.rm = TRUE)
  } else if(method == "specified"){
      value = specified
  }
  col[is.na(col)] = value
  col
}

# function to create create a row, one-hot-encoding a variable
ohe_single_entry <- function(entry, values){
  as.numeric(entry == values)
}

# now function to apply function to a column 
ohe_column_to_mat <- function(col, colName, dropLast = FALSE){
  colValues = col %>% unique() %>% sort()
  oheMat = sapply(col, ohe_single_entry, colValues) %>% t()
  colnames(oheMat) = paste0(colName, sep = "_", colValues)
  if(dropLast){
    oheMat = oheMat[, -ncol(oheMat)]
  }
  return(oheMat)
}

