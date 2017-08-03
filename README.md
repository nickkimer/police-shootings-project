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

[Fleeing: 637; Non-Fleeing: 1428; NA: 35]

