#### Titanic ####

library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(leaps)
library(bestglm)
library(randomForest)
library(xgboost)

setwd("/Users/gabrielwallon/Desktop/R/")

trainData <- read_csv("train.csv")
testData <- read_csv("test.csv")

# sort the testData so that we can easily compare models with survival answer key
testData = testData[order(testData$Name),]

#--------------------------------------------------------------------

#### EDA #### 

## Exclude from model: PassengerId, Name, Ticket, Cabin, Embarked

head(trainData)

sum(trainData$Survived) / dim(trainData)[1]     # total survival rate

## Identifying NA values in "Cabin" 
length(trainData$Cabin[is.na(trainData$Cabin)])
trainData$Cabin[!is.na(trainData$Cabin)]
trainData$Cabin %>% unique()
percent_na = length(trainData$Cabin[is.na(trainData$Cabin)]) / dim(trainData)[1]
print(paste("Percent Cabin Column NA:", percent_na))
# can see that there are a lot of NA values in the cabin column. I also suspect 
# there to be a strong correlation between this cabin number and a passengers 
# "Pclass", or passenger class, which indicates their ticket class. This would 
# imply that adding the cabin number feature to any model would likely not 
# provide any additional value to the model.


## Checking woman vs man survival rates
totalWomen = sum(trainData$Sex == "female")
survivedWomen = sum(trainData[which(trainData$Sex == "female"), "Survived"])
womanSurvivalRate = survivedWomen/totalWomen

totalMen = sum(trainData$Sex == "male")
survivedMen = sum(trainData[which(trainData$Sex == "male"), "Survived"])
manSurvivalRate = survivedMen/totalMen
print(paste("Women:", womanSurvivalRate))
print(paste("Men:", manSurvivalRate))
# Here we can see that men were much less likely to survive the crash then 
# women. This makes sense when you think of how chivalry is admired in many cultures.


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
# Here, we can clearly see that a passenger with a higher ticket class, had a 
# higher chance of survival. This also follows our intuition.


# Explore relation between PClass and Fare
## Can see there is not a very strong correlation, many 2nd class passengers paid less than
### third class for their tickets, therefore the Fare variable holds non-redundant,
#### potentially useful information
ggplot(data = trainData,
  aes(x = as.character(Pclass),
      y = Fare)) + 
  geom_boxplot()


## Finding variable correlations
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

# View the correlations between variables 
cor(trainDesMat)

tibble(
  "Variables" = colnames(trainDesMat),
  "Survived" = cor(trainDesMat)[,1]
)

#--------------------------------------------------------------------

#### Logistic Models ####

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
) %>% select(
  -PassengerId,
  -Name,
  -Ticket,
  -Cabin,
  -Embarked,
  -Sex,
  -Pclass
) 

oheTestPclassMat = ohe_column_to_mat(testData$Pclass, "Pclass", TRUE)

newTestDf = cbind.data.frame(
  selectTestDf,
  oheTestPclassMat
)
head(newTestDf)

# check for NAs
apply(is.na(newTrainDf), MARGIN = 2, any)
apply(is.na(newTestDf), MARGIN = 2, any)


## LOGISTIC REGRESSION
logit_1 = glm(newTrainDf$Survived ~ ., 
              data = newTrainDf, 
              family = "binomial")
summary(logit_1)
preds_1 = predict(logit_1, newTestDf)    # can be transformed into probabilities using 
## `logit` function
rounded_preds_1 = round(logit(preds_1)) # assume survival if predicted val > .5

# make Kaggle submission
logit_1_preds = tibble(
  PassengerId = testData$PassengerId,
  Probabilities = logit(preds_1),
  Survived = rounded_preds_1
) 
# write_csv(logit_1_preds, "titanic_answer1.csv")
# logit_1_preds has a .76 rate of accuracy


logit_2 = glm(newTrainDf$Survived ~ sexMale + Age + Pclass_1 + Pclass_2, 
              data = newTrainDf, 
              family = "binomial")
summary(logit_2)
preds_2 = predict(logit_2, newTestDf)
rounded_preds_2 = round(logit(preds_2))

# make Kaggle submission
logit_2_preds = tibble(
  PassengerId = testData$PassengerId,
  Probabilities = logit(preds_2),
  Survived = rounded_preds_2
)
# write_csv(logit_2_preds, "titanic_answer2.csv")
# logit_3_preds has a .77 rate of accuracy


logit_3 = glm(newTrainDf$Survived ~ sexMale + Pclass_1 + Pclass_2, 
              data = newTrainDf, 
              family = "binomial")
summary(logit_3)
preds_3 = predict(logit_3, newTestDf)
rounded_preds_3 = round(logit(preds_3))

# make Kaggle submission
logit_3_preds = tibble(
  PassengerId = testData$PassengerId,
  Probabilities = logit(preds_3),
  Survived = rounded_preds_3
)
# write_csv(logit_3_preds, "titanic_answer4.csv")
# logit_2_preds has a .76 rate of accuracy


logit_4 = glm(newTrainDf$Survived ~ sexMale + Pclass_1 + Pclass_2 + 0, 
              data = newTrainDf, 
              family = "binomial")
summary(logit_4)
preds_4 = predict(logit_4, newTestDf)
rounded_preds_4 = round(logit(preds_4))

# make Kaggle submission
logit_4_preds = tibble(
  PassengerId = testData$PassengerId,
  Probabilities = logit(preds_4),
  Survived = rounded_preds_4
)
# write_csv(logit_4_preds, "titanic_answer5.csv")
# logit_4_preds has a .78 rate of accuracy


## bestglm() method 
# requires binary response variable in the last column
train_reordered <- newTrainDf %>% 
  transmute(Age, SibSp, Parch, Fare, sexMale, Pclass_1, Pclass_2, Survived)

best_subset <- bestglm(Xy=train_reordered,family=binomial,method='exhaustive',IC='AIC')
aic_best_model = best_subset$BestModel
summary(aic_best_model)
# same as logit_1 !

#-----------------------------------------------------------------------------

#### Comparing Logit Models with Log Loss ####

# AIC
aic_comp_table <- tibble(
  model1 = logit_1$aic,
  model2 = logit_2$aic,
  model3 = logit_3$aic,
  model4 = logit_4$aic
)

# here I use the `answerKey` created in the titanic2.R file 
# horizontally combine the logistic model predictions with the answer key
llDf1 = cbind(logit_1_preds, answerKey)
llDf2 = cbind(logit_2_preds, answerKey)
llDf3 = cbind(logit_3_preds, answerKey)
llDf4 = cbind(logit_4_preds, answerKey)

# below returns the same accuracy rate given by Kaggle's evaluation of the model
kaggleScore1 = sum(llDf1$Survived == llDf1$realSurvival) / 418
kaggleScore2 = sum(llDf2$Survived == llDf2$realSurvival) / 418
kaggleScore3 = sum(llDf3$Survived == llDf3$realSurvival) / 418
kaggleScore4 = sum(llDf4$Survived == llDf4$realSurvival) / 418

# using log_loss function `log_loss(yTrue, yPred)`
logit1_LL = log_loss(llDf1$realSurvival, llDf1$Probabilities)
logit2_LL = log_loss(llDf2$realSurvival, llDf2$Probabilities)
logit3_LL = log_loss(llDf3$realSurvival, llDf3$Probabilities)
logit4_LL = log_loss(llDf4$realSurvival, llDf4$Probabilities)

compTable = tibble(
  Model = c("Logit_1", "Logit_2", "Logit_3", "Logit_4"),
  LogLoss = c(logit1_LL, logit2_LL, logit3_LL, logit4_LL),
  KaggleScore = c(kaggleScore1, kaggleScore2, kaggleScore3, kaggleScore4)
) %>% column_to_rownames(var = "Model")

# We can see from `compTable` above that a higher Log-Loss corresponds to a 
## more accurate model, this goes against what I would expect from what I know 
## about Log-Loss method of model evaluation, shown below. 

highs = runif(10, min = .5, max = 1)
lows = runif(10, 0, .5)
highValues = c(1,1,1,1,1,1,1,1,1,1)
lowValues = c(0,0,0,0,0,0,0,0,0,0)

log_loss(highValues, lows)
log_loss(highValues, highs)
log_loss(lowValues, lows)
log_loss(lowValues, highs)

## A higher Log-Loss score indicates that the predicted values had a greater 
## divergence from the true values.

#--------------------------------------------------------------------

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

#--------------------------------------------------------------------

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

#--------------------------------------------------------------------------------
  

#### Functions ####
# Log_loss
log_loss <- function(yTrue, yPred){
  -mean(yTrue * log(yPred) + (1-yTrue)*log(1-yPred))
}

# lbind
lbind <- function(x, y){
  abind(x, y, along = 3)
}

# logistic model transformation function
logit <- function(x){1/(1 + exp(-x))}

# function to impute column NA values
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

# I did NOT author the two functions below
# function to create create a row, one-hot-encoding a variable
ohe_single_entry <- function(entry, values){
  as.numeric(entry == values)
}

# now function to apply OHE function to a column 
ohe_column_to_mat <- function(col, colName, dropLast = FALSE){
  colValues = col %>% unique() %>% sort()
  oheMat = sapply(col, ohe_single_entry, colValues) %>% t()
  colnames(oheMat) = paste0(colName, sep = "_", colValues)
  if(dropLast){
    oheMat = oheMat[, -ncol(oheMat)]
  }
  return(oheMat)
}

