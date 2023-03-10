#### Same Titanic data which includes survival values for Kaggle test data####
# loading a different version of the data set so that I can more easily 
## evaluate the accuracy of my models by checking the survival values for 
### the Kaggle `testData` names

library(tidyverse)
library(tibble)


# Kaggle data
setwd("/Users/gabrielwallon/Desktop/R")
trainData <- read_csv("train.csv")
testData <- read_csv("test.csv")

# loading 'titanic' data set 
set.seed(17)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read_csv(path)


head(titanic)
head(trainData)
testPassNames = testData$Name         
trainPassNames = trainData$Name
kaggleTitanicNames = c(testPassNames, trainPassNames)
# below implies the names in the data sets are different
all(titanic$name %in% kaggleTitanicNames)

# check for matching names in the different data sets
kagSpecNames = kaggleTitanicNames[!(kaggleTitanicNames %in% titanic$name)]
kagRegNames = kaggleTitanicNames[kaggleTitanicNames %in% titanic$name]
# 75 Kaggle names in the Kaggle DS not in the titanic and vice-versa

# names in the titanic DS which are not in the kaggle DS
titSpecNames = titanic$name[!(titanic$name %in% kaggleTitanicNames)]
titRegNames = titanic$name[titanic$name %in% kaggleTitanicNames]

# sanity check
all(kagRegNames %in% titRegNames)
matchingNames = kagRegNames

# check if the two sets of non-matching names look the same
head(sort(kagSpecNames))
head(sort(titSpecNames))

#--------------------------------------------------------------------------------


#### Do the data sets have the same passenger survival values? ####
# first check the survival records in the matching names data to ensure they are equal
kagNewDf = trainData[trainData$Name %in% matchingNames,]
kagNewDf = kagNewDf[order(kagNewDf$Name),]
head(kagNewDf)

trainDataMatchingNames = kagNewDf$Name
titNewDf = titanic[titanic$name %in% trainDataMatchingNames,]
titNewDf = titNewDf[order(titNewDf$name),]
head(titNewDf)

#can see that these data sets have different number of rows 
dim(kagNewDf)
dim(titNewDf)

#where do they start to disagree and why?
kagNewDf$Survived == titNewDf$survived
kagNewDf[165:176,]
titNewDf[165:176,]

# can see there were two "Miss. Kate Connolly"s on the boat and only one in the traindata
## but two in the titanic data, thus will remove this extra entry

titNewDf[169:170,]
titNewDf = titNewDf[-170,]
kagNewDf[168:173,]
titNewDf[168:173,]

#now start again , and notice there are two "Mr.James Kelly"'s aboard and only one in trainData
kagNewDf$Survived == titNewDf$survived
kagNewDf[408:415,]
titNewDf[408:415,]

#remove the right james kelly, the one of age 34.5
titNewDf = titNewDf[-412,]
dim(titNewDf)

# now we can see that all the outcomes match in these two data sets
all(kagNewDf$Survived == titNewDf$survived)

#--------------------------------------------------------------------------------
  

#### Finding the survival values for test data ####
# make a joint Kaggle data set
head(trainData)
head(testData)

# add arbitrary survived column to the testData tibble so we can combine the DFs
testData2 = add_column(testData, Survived = 0) 
testData2 = testData %>% relocate(Survived, .after = PassengerId)
head(testData2)

# vertically stack the Kaggle train DF and test DF 
kaggleDf = bind_rows(trainData, testData2)
tail(kaggleDf)
dim(kaggleDf)
dim(titanic)


#### sort both make the kaggleDf and the titanic DF and then add the survival 
##### survival column from the titanic DF to kaggleDf 
# need to show that the order of the 
kaggleDfSortedNames = kaggleDf[order(kaggleDf$Name),]$Name
titanicSortedNames = titanic[order(titanic$name),]$name

kaggleDfSortedNames[kaggleDfSortedNames == titanicSortedNames]
length(kaggleDfSortedNames[kaggleDfSortedNames != titanicSortedNames])

# if the order of the names of the passengers are the same in both dataframes, 
## then we don't need to change any of the names we just need to change the survived column
### of the kaggleDf to equal that of the titanic DF
kaggleDfSortedNames[kaggleDfSortedNames != titanicSortedNames] == sort(kagSpecNames)
titanicSortedNames[kaggleDfSortedNames != titanicSortedNames] == sort(titSpecNames)

# all the names in the two data sets that do not match appear to be in the same order
kaggleDfSortedNames[kaggleDfSortedNames != titanicSortedNames]
titanicSortedNames[kaggleDfSortedNames != titanicSortedNames]

# therefore if we sort the two data frames and then copy the titanic survival column into the 
## kaggleDf column they should be the correct values 
sortedKaggleDf = kaggleDf[order(kaggleDf$Name),]
sortedTitanicDf = titanic[order(titanic$name),]

sortedKaggleDf = add_column(sortedKaggleDf, survived = sortedTitanicDf$survived)
head(sortedKaggleDf)
head(sortedTitanicDf)

# now subset `sortedKaggleDf` to just the names that are in the testData 
# `survived` column of this DF should be the answers to the competition
answerDf = sortedKaggleDf[sortedKaggleDf$Name %in% testData2$Name,]

# dimension of this DF is not correct because there are some passengers in the train data 
## with the same names as some passengers in the test data 
dim(answerDf)

# where do the answerDf and the testData DF start to differ?
sortedTest = testData2[order(testData2$Name),]
sortedTest$Name == answerDf$Name

# around the 85th row they start to differ
answerDf[84:88,]$Name
sortedTest[84:88,]$Name

# answerDf has an extra "Connolly, Miss. Kate" which need be removed
answerDf[84:88,]
sortedTest[84:88,]

# remove the "Connolly, Miss. Kate" who is 22 years old 
answerDf[85,]
answerDf = answerDf[-85,]
answerDf[84:88,]
sortedTest[84:88,]

# repeat this process for the second name repetition
sortedTest$Name == answerDf$Name

# around the 210th row they start to differ
answerDf[209:212,]$Name
sortedTest[209:212,]$Name

# answerDf has an extra "Kelly, Mr. James" which need be removed
answerDf[209:212,]
sortedTest[209:212,]

# remove the "Kelly, Mr. James" who is 44 years old 
answerDf[210,]
answerDf = answerDf[-210,]
answerDf[209:212,]
sortedTest[209:212,]

# now these names should all match
all(sortedTest$Name == answerDf$Name)


####  NOW the `survived` column of this DF should be the answers to the competition
answerKey = tibble(
  Name = answerDf$Name,
  PassengerId = answerDf$PassengerId,
  realSurvival = as.numeric(answerDf$survived)
)
# write_csv(answerKey, "titanic_answerKey2.csv")

typeof(answerKey$realSurvival)

View(answerKey)

#### Conclusion ####
# upon checking the accuracy of these predictions, `answerKey`,  on Kaggle we 
## can confirm a 100% survival prediction rate, so I did this correctly
