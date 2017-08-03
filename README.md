# police-shootings-project
A Machine Learning based approach to investigating the Police Shootings Data from The Washington Post

## Background
In an effort to further investigate this topic of discussion, The Washington Post released a dataset containing every fatal shooting in the United States by a police officer in the line of duty since 2015. There are many attributes associated with each observation including race of the deceased, circumstances of the shooting, and additional information about the deceased including whether or not he/she was experiencing signs of mental health issues, their age, and gender [3]. An initial research question was formed around being able to use the information provided - **is it feasible to apply machine learning methods in classifying/predicting whether or not a person will flee given the circumstances of the situation and the information about the individual?**

In training these classifiers, variable importance and subset selection was performed in an attempt to identify which of the attributes come together to provide the best predictive power. After using this dataset to train and evaluate the classifiers, additional geographic and demographic location information by county was added in an attempt to aid in the performance of the classification methods.

## Methodology
Three different methods were used to attempt to make this classification with the highest accuracy - logistic regression, Naive Bayes, and a decision tree using the C5.0 algorithm. The process began with a dataset provided from the Washington Post database that includes the variables name, date, manner of death, armed, age, gender, race, city+state, signs of mental illness, threat level, presence of a body camera from the officer, and whether or not the deceased tried to flee. From here, each of the three methods builds a classifier and the best was chosen from k-fold cross validation with k set to 10 and the percent accuracy was reported.

### Data Exploration 
The police shootings data set contains several categorical variables that have many different levels, such as the type of weapon used variable. To avoid overfitting, these variables were re-categorized. For the type of weapon used the following categories were used: Gun, Knife, Toy Weapon, Unarmed, Vehicle, Undetermined, and Other. This took the initial number of levels from 65 to 7 categories; the following visualization helped to guide the selection of the new categories. 

Age groups were also re-categorized based on how the US census groups ages; the new categories were 0-17 years, 18-44, 45- 64, 65+ and unknown. Lastly, the “flee” variable was re-categorized. This variable originally indicated how and if the suspect fled with the following categories: “Car”, “Foot”, Not Fleeing”, “Other”, and “Missing”. Since the main research question is attempting to predict if a suspect will or will not flee, the variable was regrouped to “Flee” and “Not Flee” and missing values were removed from the data set.

To deal with other missing values in the data set, several variables received an additional category labeled “missing.” The missing data was limited to five variables (Gender, Age Groups, Race, Weapon Type, and Flee). Each of these categories were refactored to include the new “missing” level except for the Flee variable (the rows with missing data were removed). The distribution of the newly reclassified predictor is the following:  

![Alt Text](https://rawgithub.com/nickkimer/police-shootings-project/gh-pages/fleeing.PNG)


### Data Manipulation
In efforts to try to boost model performance and increase the size of the feature space, census data was merged onto the original Data.

```
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

```


## Results

The C5.0 algorithm, without the additional merged demographic data, had a 10-fold cross-validated raw accuracy of 71.9%. With the additional data the 10-fold cross-validated raw accuracy increased 1% to 72.9%. The model with the highest testing accuracy from the original data is the following: 

![Alt Text](https://rawgithub.com/nickkimer/police-shootings-project/gh-pages/treediagram.PNG)

Three variables (sign of mental illness, the type of weapon, and the age groups) were the variables of the greatest importance in the decision tree. This model had an accuracy of 75%. 
The best model from the demographic data had an accuracy of 79% and had a tree size of 16. 

The second model with the additional census data is slightly better than the first model; however, the additional percent in accuracy probably does not warrant adding the additional variables. The model is more susceptible overfitting. Therefore, the simpler model is probably the better model in this case. 

The next model is Naïve Bayes. Naïve Bayes is a conditional probability model that is made up of simple probabilistic classifiers. Naïve Bayes, in theory, seems to be a useful aspect in predicting whether or not a suspect will flee. The r function NaiveBayes in the package e1071. Using the original data set of police shooting, an accuracy of 67.7 % was achieved. Adding the additional demographics to the data set, gave an accuracy of 67.5%. The accuracy of these two data sets are identical. This could be due to the small sample of data but a large number of predictors. Additionally, Naïve Bayes assumes all predictors are independent of each other. However, some variables such as threat level and gender could be related. These aspects make it difficult to support the continued use of the Naive Bayes classifier. 

The logistic regression model builds in efforts to classify the observations utilized both datasets and followed a similar process. The models were trained and tested using 10-fold cross validation and backward stepwise regression to find the best model with the highest classification accuracy. In doing so with the original dataset without any county demographic data, the final model with the highest accuracy rate was flee ~ signs of mental illness + age + armed. The final equation itself was 0.3095 + 1.1097*signs of mental illness(True) + 1.0419*age(45-64) + 1.5804*age(65+) - 0.4869*age(Less than 18) + 0.2139*age(unknown) + 1.0556*armed(knife) + 0.9589*armed(other) + 0.2360*armed(toyweapon) - 0.1246*armed(unarmed) - 0.0200*armed(undetermined) - 1.7863*armed(vehicle). From this it is evident that the largest effect in terms of magnitude in both directions of effect for log odds are the age of 65+ and being armed with a vehicle. The 10-fold cross-validation classification averaged accuracy rate was 66.108%.

The same process was repeated after adding in the demographic data and the model with the highest cross validated accuracy was a model containing race + signs of mental illness + body camera. The final modeled equation came out to -0.0832*raceA -1.0279*raceB - 0.8814*raceH - 1.0363*raceN - 0.8163*raceO - 0.7807*raceW + 1.313*signs of mental illness(True), with an average cross validated accuracy rate of 68.247%. Again this model highlights the importance of signs of mental illness in determining whether or not an observation is classified as flee or not fleeing in the police encounter. In the grand scheme of things, the added variables only came out to a marginal boost in classification accuracy.

