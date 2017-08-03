# Nick Kim, Gabriel Rushin, Cody Stancil
# Machine Learning Project - Police Shootings Data from Washington Post
# Prof. Abby Flower - DSI 6016 Machine Learning
# 3/3/2017

#######################################
#######                       #########
#######     Trees (c5.0)      #########
#######                       #########
#######################################

setwd("~/Box Sync/DSI/Spring Semester 2017/Machine Learning/Project1")
library(C50)
library(gmodels)
library(plyr)
police.data = read.csv("fatal-police-shootings-data.csv")
lapply(police.data, levels)

# Replace  blanks with "missing"
levels(police.data$armed)[1] = "missing"
levels(police.data$gender)[1] = "missing"
levels(police.data$race)[1] = "missing"

# Replace  blanks with NA
police.data$flee[police.data$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64
hist(police.data$age)
police.data$age[is.na(police.data$age) ]="Unknown"
police.data[["AGE_BINS"]] = ifelse(police.data$age < 18, "Less_than_18", ifelse(police.data$age <= 44, "18_to_44", ifelse(police.data$age== "Unknown", "Unknown", ifelse(police.data$age<=64, "45_to_64", "OLD"))))
police.data$AGE_BINS= as.factor(police.data$AGE_BINS)
plot(police.data$AGE_BINS)
#Grouping flee by "Flee", "Notfleeing", "Other"
police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot"|
                            police.data$flee == "Other", 'flee',"notfleeing")
police.data$flee= as.factor(police.data$flee)

#Distribution of Weapons
police.data[["Armed_Bins"]] <- ifelse(police.data$armed== "gun", "gun",ifelse(police.data$armed== "knife", "knife", 
                                                                              ifelse(police.data$armed== "undetermined","undetermined", ifelse(police.data$armed=="vehicle", "vehicle", ifelse(police.data$armed=="unarmed", "unarmed", 
                                                                                                                                                                                               ifelse(police.data$armed== "toy weapon", "toyweapon","other"))))))

set.seed(1234)
# get the data from somewhere and specify number of folds
nrFolds <- 10

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(police.data))

# actual cross validation
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- police.data[-fold,]
  data.test <- police.data[fold,]
  police.model <- C5.0(data.train[-c(1,2,13,6,3,9,5)], as.factor(data.train$flee))
  police.predict <- predict(police.model, newdata=data.test)
  print(summary(police.predict))
  x= CrossTable(data.test$flee, police.predict,
                prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                dnn = c('actual flee', 'predicted flee'))
  print(x$t)
  print(postResample(predict(police.model, data.test), data.test$flee))
}



police.data.dem= read.csv("police_data_demographics_merged.csv")

# Replace  blanks with "missing"
levels(police.data.dem$armed)[1] = "missing"
levels(police.data.dem$gender)[1] = "missing"
levels(police.data.dem$race)[1] = "missing"

# Replace  blanks with NA
police.data.dem$flee[police.data.dem$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64
hist(police.data.dem$age)
police.data.dem$age[is.na(police.data.dem$age) ]="Unknown"
police.data.dem[["AGE_BINS"]] = ifelse(police.data.dem$age < 18, "Less_than_18", ifelse(police.data.dem$age <= 44, "18_to_44", ifelse(police.data.dem$age== "Unknown", "Unknown", ifelse(police.data.dem$age<=64, "45_to_64", "OLD"))))
police.data.dem$AGE_BINS= as.factor(police.data.dem$AGE_BINS)

#Grouping flee by "Flee", "Notfleeing", "Other"
police.data.dem$flee = ifelse(police.data.dem$flee == "Car" | police.data.dem$flee == "Foot"|
                                police.data.dem$flee == "Other", 'flee',"notfleeing")
police.data.dem$flee= as.factor(police.data.dem$flee)

#Distribution of Weapons
police.data.dem[["Armed_Bins"]] <- ifelse(police.data.dem$armed== "gun", "gun",ifelse(police.data.dem$armed== "knife", "knife", 
                                                                                      ifelse(police.data.dem$armed== "undetermined","undetermined", ifelse(police.data.dem$armed=="vehicle", "vehicle", ifelse(police.data.dem$armed=="unarmed", "unarmed", 
                                                                                                                                                                                                               ifelse(police.data.dem$armed== "toy weapon", "toyweapon","other"))))))
police.data.dem[,c(55)] <- as.numeric(police.data.dem[,c(55)] )
police.data.dem[,c(56)] <- as.numeric(police.data.dem[,c(56)] )
police.data.dem[,c(57)] <- as.numeric(police.data.dem[,c(57)] )
police.data.dem$Median_Household_Income_2015 = gsub("\\$", "", police.data.dem$Median_Household_Income_2015)
police.data.dem$Median_Household_Income_2015 = as.numeric(gsub("\\,", "", police.data.dem$Median_Household_Income_2015))

set.seed(1234)
# get the data from somewhere and specify number of folds
nrFolds <- 10

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(police.data.dem))

# actual cross validation

accuracy= as.data.frame(matrix())
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- police.data.dem[-fold,]
  data.test <- police.data.dem[fold,]
  police.model <- C5.0(data.train[,-c(1,2,3,4,15,8,5,11,12,7,18,17,19,20, 23:54)], as.factor(data.train$flee))
  police.predict <- predict(police.model, newdata=data.test)
  x= CrossTable(data.test$flee, police.predict,
                prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                dnn = c('actual flee', 'predicted flee'))
  print(x)
  print(postResample(predict(police.model, data.test), data.test$flee))
  ac= postResample(predict(police.model, data.test), data.test$flee)
  accuracy= rbind(accuracy,ac )
}
mean(accuracy[2:11,1]) #0.729305

#######################################
#######                       #########
#######     Naive Bayes       #########
#######                       #########
#######################################

setwd("~/Desktop/data-police-shootings-master")
library(C50)
library(gmodels)
library("e1071")
library("caret")
##########Orginial police data###############
police.data = read.csv("fatal-police-shootings-data.csv")
lapply(police.data, levels)

# Replace  blanks with "missing"
levels(police.data$armed)[1] = "missing"
levels(police.data$gender)[1] = "missing"
levels(police.data$race)[1] = "missing"

# Replace  blanks with NA
police.data$flee[police.data$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64
police.data$age[is.na(police.data$age) ]="Unknown"
police.data[["AGE_BINS"]] = ifelse(police.data$age < 18, "Less_than_18", ifelse(police.data$age <= 44, "18_to_44", ifelse(police.data$age== "Unknown", "Unknown", ifelse(police.data$age<=64, "45_to_64", "OLD"))))
police.data$AGE_BINS= as.factor(police.data$AGE_BINS)

#Grouping weapons
police.data[["Armed_Bins"]] <- ifelse(police.data$armed== "gun", "gun",ifelse(police.data$armed== "knife", "knife", 
                                                                              ifelse(police.data$armed== "undetermined","undetermined", ifelse(police.data$armed=="vehicle", "vehicle", ifelse(police.data$armed=="unarmed", "unarmed", 
                                                                                                                                                                                               ifelse(police.data$armed== "toy weapon", "toyweapon","other"))))))
lapply(police.data, levels)
#Grouping flee by "Flee", "Notfleeing", "Other"
police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot", 'flee', ifelse(police.data$flee == "Other", "Other", "notfleeing"))
police.data$flee= as.factor(police.data$flee)
str(police.data$flee)
unique(factor(police.data$flee))

as.factor(police.data$armed)

##########################################################
########## Naive Bayes #################
##########################################################

#Cross validation
# get the data from somewhere and specify number of folds
nrFolds <- 10

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(police.data))

set.seed(1234)
# actual cross validation
accuracy= as.data.frame(matrix())
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- police.data[-fold,]
  data.test <- police.data[fold,]
  police.model<-naiveBayes(data.train$flee~., data=data.train[,-c(1,2,13,6,3,9,5)])
  police.predict <- predict(police.model, newdata=data.test)
  print(summary(police.predict))
  x= CrossTable(data.test$flee, police.predict,
                prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                dnn = c('actual flee', 'predicted flee'))
  print(x$t)
  print(postResample(predict(police.model, data.test), data.test$flee))
  ac= postResample(predict(police.model, data.test), data.test$flee)
  accuracy= rbind(accuracy,ac )
}
mean(accuracy[2:11,1])
#0.6778087



######Police data + added demographics###########
police.data.dem= read.csv("police_data_demographics_merged.csv")

# Replace  blanks with "missing"
levels(police.data.dem$armed)[1] = "missing"
levels(police.data.dem$gender)[1] = "missing"
levels(police.data.dem$race)[1] = "missing"

# Replace  blanks with NA
police.data.dem$flee[police.data.dem$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64
hist(police.data.dem$age)
police.data.dem$age[is.na(police.data.dem$age) ]="Unknown"
police.data.dem[["AGE_BINS"]] = ifelse(police.data.dem$age < 18, "Less_than_18", ifelse(police.data.dem$age <= 44, "18_to_44", ifelse(police.data.dem$age== "Unknown", "Unknown", ifelse(police.data.dem$age<=64, "45_to_64", "OLD"))))
police.data.dem$AGE_BINS= as.factor(police.data.dem$AGE_BINS)

#Grouping flee by "Flee", "Notfleeing", "Other"
police.data.dem$flee = ifelse(police.data.dem$flee == "Car" | police.data.dem$flee == "Foot"|
                                police.data.dem$flee == "Other", 'flee',"notfleeing")
police.data.dem$flee= as.factor(police.data.dem$flee)

#Distribution of Weapons
police.data.dem[["Armed_Bins"]] <- ifelse(police.data.dem$armed== "gun", "gun",ifelse(police.data.dem$armed== "knife", "knife", 
                                                                                      ifelse(police.data.dem$armed== "undetermined","undetermined", ifelse(police.data.dem$armed=="vehicle", "vehicle", ifelse(police.data.dem$armed=="unarmed", "unarmed", 
                                                                                                                                                                                                               ifelse(police.data.dem$armed== "toy weapon", "toyweapon","other"))))))
police.data.dem[,c(55)] <- as.numeric(police.data.dem[,c(55)] )
police.data.dem[,c(56)] <- as.numeric(police.data.dem[,c(56)] )
police.data.dem[,c(57)] <- as.numeric(police.data.dem[,c(57)] )
police.data.dem$Median_Household_Income_2015 = gsub("\\$", "", police.data.dem$Median_Household_Income_2015)
police.data.dem$Median_Household_Income_2015 = as.numeric(gsub("\\,", "", police.data.dem$Median_Household_Income_2015))

set.seed(1234)
# get the data from somewhere and specify number of folds
nrFolds <- 10

# generate array containing fold-number for each sample (row)
folds <- rep_len(1:nrFolds, nrow(police.data.dem))

# actual cross validation

accuracy= as.data.frame(matrix())
for(k in 1:nrFolds) {
  # actual split of the data
  fold <- which(folds == k)
  data.train <- police.data.dem[-fold,]
  data.test <- police.data.dem[fold,]
  police.model<-naiveBayes(data.train$flee~., data=data.train[,-c(1,2,3,4,15,8,5,11,12,7,18,17,19,20, 23:54)])
  police.predict <- predict(police.model, newdata=data.test)
  x= CrossTable(data.test$flee, police.predict,
                prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
                dnn = c('actual flee', 'predicted flee'))
  print(x)
  print(postResample(predict(police.model, data.test), data.test$flee))
  ac= postResample(predict(police.model, data.test), data.test$flee)
  accuracy= rbind(accuracy,ac )
}
mean(accuracy[2:11,1]) #0.6750825


#######################################
#######                       #########
####### LOGISTIC REGRESSION   #########
#######                       #########
#######################################

# DATA CLEANING

police.data = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/fatal-police-shootings-data.csv",header=TRUE)
#police.data = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/police_data_demographics_merged.csv",header=TRUE)
str(police.data)
library(C50)
library(gmodels)

lapply(police.data, levels)

# Replace  blanks with "missing"
levels(police.data$armed)[1] = "missing"
levels(police.data$gender)[1] = "missing"
levels(police.data$race)[1] = "missing"

# Replace  blanks with NA
police.data$flee[police.data$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64, 65+
hist(police.data$age)
police.data$age[is.na(police.data$age) ]="Unknown"
police.data[["AGE_BINS"]] = ifelse(police.data$age < 18, "Less_than_18", ifelse(police.data$age <= 44, "18_to_44", ifelse(police.data$age== "Unknown", "Unknown", ifelse(police.data$age<=64, "45_to_64", "65+"))))
police.data$AGE_BINS= as.factor(police.data$AGE_BINS)

#Group the weapons categories
police.data[["Armed_Bins"]] <- ifelse(police.data$armed== "gun", "gun",ifelse(police.data$armed== "knife", "knife", 
                                                                              ifelse(police.data$armed== "undetermined","undetermined", ifelse(police.data$armed=="vehicle", "vehicle", ifelse(police.data$armed=="unarmed", "unarmed", 
                                                                                                                                                                                               ifelse(police.data$armed== "toy weapon", "toyweapon","other"))))))
#Grouping flee by "Flee", "Notfleeing", "Other"
#police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot", 'flee', ifelse(police.data$flee == "Other", "Other", "notfleeing"))
police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot"| police.data$flee == "Other", 'flee',"notfleeing")

#police.data$flee[police.data$flee== 'Other' ] = 'notfleeing'
police.data$flee= as.factor(police.data$flee)
str(police.data$flee)
unique(factor(police.data$flee))


######### County Demographic DATA MERGING

#Read in all demographic data

# Demographic data acquired from Economic Research Searvice "Unemployment and median household income for the U.S., States, and counties, 2007-15"
# https://www.ers.usda.gov/data-products/county-level-data-sets/county-level-data-sets-download-data.aspx
# The dataset of US Cities with their corresponding counties was found at this linke
# https://github.com/grammakov/USA-cities-and-states

data1 = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/us_cities_states_counties.csv",header=TRUE)
data2 = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/fips_demographic_police.csv",header=TRUE)
police.data = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/fatal-police-shootings-data.csv",header=TRUE)

#Create new column for the city name in the format we need - "Charlottesville, VA"
police.data$full_city_name = paste(police.data$city,", ",police.data$state,sep="")

#Grab the unique city and county pairs 
subset = data1[,c("full_city_name","full_county_name")]
subset = unique(subset)

#Merge onto the police data the name of the county
m = merge(police.data,subset,by="full_city_name",all.x=TRUE)
m = subset(m, !duplicated(name))

#fill in washington,dc as the county name for unread labels 

for(i in 1:nrow(m)){
  if(m$full_city_name[i] == "Washington, DC"){
    m$full_county_name[i] = "Washington, DC"
  }
}

#Merge onto the previous dataset the demographic data by county
m2 = merge(m,data2,by="full_county_name",all.x=TRUE)

#Export final dataset as CSV

#write.csv(m2,"C:/Users/Nick/Desktop/DSI Spring 2017/police_data_demographics_merged.csv",row.names = FALSE)

############################################
# Logistic Regression Model

police.data = police.data[-c(1,2,5,6,3,9)]
police.data = police.data[!is.na(police.data$flee),]


# Split the data into training and testing data sets
set.seed(1230984)
training.indices = sample(1:nrow(police.data), as.integer(nrow(police.data) * 0.75))
training.set = police.data[training.indices,]
testing.set = police.data[-training.indices,]


model = glm(training.set$flee~.,family=binomial(link="logit"),data = training.set)
summary(model)

#Model Diagnostics - Checking Deviance using chisquare anova in R
anova(model,test='Chisq')

#The chisquare test shows the significant p-values in order of lowest to highest correspond to
# (1) signs_of_mental_illness
# (2) Armed_Bins (grouped armed variable)
# (3) AGE_Bins (categorical age variable)
# (4) manner_of_death
# (5) race
# (6) state
# with state almost being borderline close to the 0.05 level of significance 

backwards = step(model) 
summary(backwards)

# The Backwards stepwise logistic regression model using the Akeike Information Criterion reduction process
# found the formula to be flee~armed_bins + signs_of_mental_illness + AGE_BINS
anova(backwards,test='Chisq')

#Elminating the factors that are new in the test set - the need for this was eliminated by grouping the armed variable in the data cleaning section
#unique_armed = unique(training.set$armed)
#testing.set$armed[!(testing.set$armed %in% unique_armed)] = NA


fitted.results <- predict(backwards,newdata=testing.set,type='response')
#fitted.results <- ifelse(fitted.results > 0.5,1,0)
fitted.results <- ifelse(fitted.results > 0.5,'notfleeing','fleeing')

misClasificError <- mean(fitted.results != testing.set$flee,na.rm=TRUE)
print(paste('Accuracy',1-misClasificError))

############ 10 Fold Cross Validation
require("caret")

#Create 10 folds of the observations
folds <- createFolds(police.data$flee,k=10)

# Instantiate list for final models in backwards step regression and the accuracy of that model
models = list()
coef = list()
accuracy_results = list()

for(i in 1:length(folds)){
  testing.indices = unlist(folds[i])
  testing.set = police.data[testing.indices,]
  training.set = police.data[-testing.indices,]
  
  model = glm(training.set$flee~.,family=binomial(link="logit"),data = training.set)
  #Backwards Elimination Stepwise Regression
  backwards = step(model,trace=0) 
  
  models[i] = formula(backwards)[3]
  name = paste("coef",i,sep="")
  assign(name,as.list(backwards$coefficients))
  
  #use the model to make predictions and compare to the held out test set
  fitted.results <- predict(backwards,newdata=testing.set,type='response')
  #fitted.results <- ifelse(fitted.results > 0.5,1,0)
  fitted.results <- ifelse(fitted.results > 0.5,'notfleeing','fleeing')
  
  misClasificError <- mean(fitted.results != testing.set$flee,na.rm=TRUE)
  print(paste('Accuracy',1-misClasificError))
  accuracy_results[i] = 1-misClasificError  
}

#10-fold cross validated accuracy  - 66.108%


#Model 9 has the highest accuracy rate - final model: flee ~ 
#signs_of_mental_illness + AGE_BINS + Armed_Bins
# $`(Intercept)`
# [1] 0.3094783
# 
# $signs_of_mental_illnessTrue
# [1] 1.109657
# 
# $AGE_BINS45_to_64
# [1] 1.04197
# 
# $`AGE_BINS65+`
# [1] 1.580386
# 
# $AGE_BINSLess_than_18
# [1] -0.4869924
# 
# $AGE_BINSUnknown
# [1] 0.2139129
# 
# $Armed_Binsknife
# [1] 1.055623
# 
# $Armed_Binsother
# [1] 0.9589315
# 
# $Armed_Binstoyweapon
# [1] 0.2360412
# 
# $Armed_Binsunarmed
# [1] -0.1245752
# 
# $Armed_Binsundetermined
# [1] -0.02003587
# 
# $Armed_Binsvehicle
# [1] -1.787303

###################################### USING DEMOGRAPHIC DATA

police.data = read.csv("C:/Users/Nick/Desktop/DSI Spring 2017/police_data_demographics_merged.csv",header=TRUE)

str(police.data)
library(C50)
library(gmodels)

lapply(police.data, levels)

# Replace  blanks with "missing"
levels(police.data$armed)[1] = "missing"
levels(police.data$gender)[1] = "missing"
levels(police.data$race)[1] = "missing"

# Replace  blanks with NA
police.data$flee[police.data$flee== "" ] = NA

#Bins based on 
#  < 18, 18 to 44, ages 45 to 64, 65+
hist(police.data$age)
police.data$age[is.na(police.data$age) ]="Unknown"
police.data[["AGE_BINS"]] = ifelse(police.data$age < 18, "Less_than_18", ifelse(police.data$age <= 44, "18_to_44", ifelse(police.data$age== "Unknown", "Unknown", ifelse(police.data$age<=64, "45_to_64", "65+"))))
police.data$AGE_BINS= as.factor(police.data$AGE_BINS)

#Group the weapons categories
police.data[["Armed_Bins"]] <- ifelse(police.data$armed== "gun", "gun",ifelse(police.data$armed== "knife", "knife", 
                                                                              ifelse(police.data$armed== "undetermined","undetermined", ifelse(police.data$armed=="vehicle", "vehicle", ifelse(police.data$armed=="unarmed", "unarmed", 
                                                                                                                                                                                               ifelse(police.data$armed== "toy weapon", "toyweapon","other"))))))
#Grouping flee by "Flee", "Notfleeing", "Other"
#police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot", 'flee', ifelse(police.data$flee == "Other", "Other", "notfleeing"))
police.data$flee = ifelse(police.data$flee == "Car" | police.data$flee == "Foot"| police.data$flee == "Other", 'flee',"notfleeing")

#police.data$flee[police.data$flee== 'Other' ] = 'notfleeing'
police.data$flee= as.factor(police.data$flee)
str(police.data$flee)
unique(factor(police.data$flee))

#Repeat the same procedure with this new dataset


police.data = police.data[c(6,9,10,12,13,14,15,16,21,60,58)]
police.data = police.data[!is.na(police.data$flee),]

police.data = police.data[complete.cases(police.data),]
############ 10 Fold Cross Validation
require("caret")

#Create 10 folds of the observations
folds <- createFolds(police.data$flee,k=10)

# Instantiate list for final models in backwards step regression and the accuracy of that model
models = list()
coef = list()
accuracy_results = list()

for(i in 1:length(folds)){
  testing.indices = unlist(folds[i])
  testing.set = police.data[testing.indices,]
  training.set = police.data[-testing.indices,]
  
  model = glm(training.set$flee~.,family=binomial(link="logit"),data = training.set)
  #Backwards Elimination Stepwise Regression
  backwards = step(model,trace=0) 
  
  models[i] = formula(backwards)[3]
  name = paste("coef",i,sep="")
  assign(name,as.list(backwards$coefficients))
  
  #use the model to make predictions and compare to the held out test set
  fitted.results <- predict(backwards,newdata=testing.set,type='response')
  #fitted.results <- ifelse(fitted.results > 0.5,1,0)
  fitted.results <- ifelse(fitted.results > 0.5,'notfleeing','fleeing')
  
  misClasificError <- mean(fitted.results != testing.set$flee,na.rm=TRUE)
  print(paste('Accuracy',1-misClasificError))
  accuracy_results[i] = 1-misClasificError  
}
#Best model contains race + signs_of_mental_illness + body_camera
models[9]
coef9
sum = 0
for(i in 1:10){
  sum = sum + accuracy_results[[i]]
}
accuracy_results[[i]]
sum/10 # 68.247%
