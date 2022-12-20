#### Titanic with new split ####
# loading a different version of the dataset so that I can more easily 
## evaluate the accuracy of my models by checking the survival values for 
### the kaggle testData names




# loading new dataset 
set.seed(17)
path <- 'https://raw.githubusercontent.com/guru99-edu/R-Programming/master/titanic_data.csv'
titanic <-read_csv(path)

# below implies the names in the datasets are different
head(titanic)
head(trainData)
all(titanic$name %in% kaggleTitanicNames)



testPassNames = testData$Name         
trainPassNames = trainData$Name
kaggleTitanicNames = c(testPassNames, trainPassNames)


# check for matching names in the different data sets
kagSpecNames = kaggleTitanicNames[!(kaggleTitanicNames %in% titanic$name)]
kagRegNames = kaggleTitanicNames[kaggleTitanicNames %in% titanic$name]
# 75 kaggle names in the kaggle DS not in the titanic and vice versa

kagSpecNames = sort(kagSpecNames)

# uniquely rename the non-matching names 
head(kagSpecNames)
newKagSpecs = c()
for (i in 1:length(kagSpecNames)){
    newKagSpecs = c(newKagSpecs,
                    paste0(
                      paste0(str_split(kagSpecNames[i], ',')[[1]][1], "_"),
                      i)
                    )
}


# titSpecNames = sort(titSpecNames)
# newTitSpecs = c()
# for (i in 1:length(titSpecNames)){
#   newTitSpecs = c(newTitSpecs,
#                   paste0(
#                     paste0(str_split(titSpecNames[i], ',')[[1]][1], "_"),
#                     i)
#   )
# }

##
all(newKagSpecs == newTitSpecs)

# want to be able to do titanic[titanic$names %in% kaggletestnames] to see the
## survival of these passengers
# need to replace the non-matching names in the testData with the modified names
# need to replace the non_matching names in the titanicData with the modified names

head(testPassNames)

kaggleTestNames = c()
for (name in testPassNames){
  if (name %in% )
}

# names in the titanic DS which are not in the kaggle DS
titSpecNames = titanic$name[!(titanic$name %in% kaggleTitanicNames)]
titRegNames = titanic$name[titanic$name %in% kaggleTitanicNames]

# sanity check
all(kagRegNames %in% titRegNames)
matchingNames = kagRegNames

# check if the two sets of non-matching names look the same
head(sort(kagSpecNames))
head(sort(titSpecNames))

typeof(titSpecNames)



#### Do the data sets have the same outcomes? ####
# first check the survived records in the matching names data to ensure they are equal
kagNewDf = trainData[trainData$Name %in% matchingNames,]
trainDataMatchingNames = kagNewDf$Name

kagNewDf = kagNewDf[order(kagNewDf$Name),]

titNewDf = titanic[titanic$name %in% trainDataMatchingNames,]
titNewDf = titNewDf[order(titNewDf$name),]

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



# make sorted data sets and rename the rows if the names are in the kag/tit spec list
head(titanic)
sortedTitanic = titanic[order(titanic$name),]
sortedTitanic %>% head()

head(trainData)
sortedKaggle = trainData[order(trainData$Name),]
sortedKaggle %>% head()




# add a `Survived` column to the testData dataset, with values equal to the `survived`
## values of the same people in the titanic dataset




# go through every name in the kaggle testDf and change to first 

# remake the columns of both data sets so that the non-matching names will be the same 
## in both
typeof(str_split(titSpecNames[1], ','))
typeof(str_split(titSpecNames[1], ',')[[1]])

str_split(titSpecNames[1], ',')[[1]][1]

titSpecNames = str_split(titSpecNames, ',')[[1]][1]

titanicDataNames = c()

titanic$name[1]

namList =  titanic$name[1:5]

namList[1] = "g"
namList

for (i in 1:length(titanic$name)){
  if (!(titanic$name[i] %in% matchingNames)) {
    titanic$name[i] = titanic$name[i]
  }
}
  
  
  




# seeing what the kaggle names that are not in the titanic DS look like 
testPassNames[!(testPassNames %in% titanic$name)]
# can see that the kaggle names which are not in the titanic DS
## have a couple unnecessary backslashes in them

# seeing what the original names that are in the titanic DS look like 
testPassNames[(testPassNames %in% titanic$name)]

tail(sort(testPassNames), 50)

tail(titanic$name, 50)



length(kaggleTitanicNames) == length(titanic$name)




titanic[titanic$name]




testPassNames %in% titanic$name













## Shuffling and cleaning the titanic data set
# notice this data is not shuffled, thus the training data will not have an equal distribution
## of passengers from different classes
titanic$pclass

shuffle_index <- sample(1:nrow(titanic))
head(shuffle_index)

titanic <- titanic[shuffle_index, ]
head(titanic)

titanic <- titanic %>% mutate(
  fare = impute_na(Fare),
) %>% select(
  -c(home.dest, cabin, name, embarked, ticket)
)

apply(is.na(titanic), MARGIN = 2, any)