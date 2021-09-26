#########################################################
## Created By : Aniket Maheshwari
## Date: 09/07/2021
## Aim is to do some Exploratory Data Analysis on Cereal data set and prepare a clean data for further regression. 
#########################################################


##Clear the environment 
rm(list = ls())


## First we will set the directory of the R script 
setwd("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 1")

#### Now we will install all the packages required 

## install.packages("lattice")
## install.packages("ggplot2")
## install.packages("MASS")
library(lattice)
library(ggplot2)
library(MASS)


#Importing the Data set 

Data1 = read.csv("C:/Users/anike/Desktop/Sem 1/EAS 506 Statistical Data Mining/Homework/Homework 1/cereal.csv" , fill = FALSE , header =  TRUE)


####### Now we'll find some basic information on the data set 

dim(Data1) 
## We have 77 rows and 16 columns in the data set 

str(Data1)
#### Out of 16 Columns : 3 are characters fields, 8 are Integer fields and 5 are numeric fields 


summary(Data1)
## Basic summary of the Data set


## Now we will check if there are any missing value in any columns.
cbind(
  lapply(
    lapply(Data1, is.na)
    , sum)
)
## So no field had a Na value i.e Missing value in it. 






#####################################
## Now let's start with EDA but before that we need to find a little more about every feature

#install.packages("Hmisc")
library(Hmisc)
describe(Data1)
#### So the information i gathered on the features are as follows:
## 1. mfr : categorical feature, has 7 categories. One thing to note about this feature is that 'A' has only one value. This may be a possible outlier.
## 2. type : categorical feature, has 2 categories but 'H' has only 3 records, we might as well remove these records because they won't be helpful. 
## 3. calories : integer feature. will plot histogram and box plot to look for potential outliers. 
## 4. protein : Discrete variable. Most proportion of values are at protein level below 4. '5' and '6' protein level has only 3 points. 
## 5. Fat : Discrete variable.
## 6. sodium : continuous integer variable. 
## 7. fiber :  Discrete variable. 
## 8. Carbo : continuous numeric variable. 
## 9. sugars : Discrete variable. One interesting thing to note in this is one data point has sugar value '-1'
## 10. potass : continuous integer variable.
## 11. vitamins :  Discrete variable. has three output '0' , '25' and '100'. 
## 12 - 14 : shelf , weight , cups : i don't really think these feature impact our target feature ('rating') much but i will plot a correlation plot first before removing these features. 
## 15 : rating : our target feature. will plot a histogram to see if there is any potential outlier in this feature. 



######################################
## Basic plotting

## Univariate - Analysis 
######################################

## 1. mfr - categorical feature.  
summary(Data1$mfr)
table(Data1$mfr)
barchart(Data1$mfr, main = "Barchart of 'mfr' feature" , ylab = "Frequency" , xlab = "mfr" , horizontal = FALSE )
# So 'A' has only 1 value to it. For now, I'll just save the index of that data point. 

mfr_index <- which(Data1$mfr == 'A')
mfr_index
## 44 index has mfr value 'A' 


## 2. type - categorical variable : 'C' and 'H' 
summary(Data1$type)
table(Data1$type)
barchart(Data1$type, main = "Barchart of 'type' feature" , ylab = "Frequency" , xlab = "Type of Cereal" , horizontal = FALSE)
## 'H' is significantly less than 'C'. We will remove 'H'
## First I'll copy my original dataset to a new dataset so that i don't make changes to my original dataset 
Data2 <- Data1
head(Data2)
dim(Data1)

type_index <- which(Data1$type == 'H')
type_index ## indexes 21,44,58 
## 44 was also the index in mfr where 'A' had it's single value. 
Data2 <- Data2[-c(21,44,58),]
dim(Data2)

## 3. calories-  int , barplot & boxplot  
summary(Data2$calories)
table(Data2$calories)

hist(Data2$calories , main = "Histogram of Feature : Calories" , xlab = "Calories" , breaks = 30)
boxplot(Data2$calories,ylab = "Calories" , main = "Boxplot of Feature : Calories" , outline = TRUE)

## box plot of calories shows some outliers but i won't remove them as of yet because we don't have much data. Instead, I'll again save those points indexes.
calories_indexs = which(Data2$calories > 120 | Data2$calories <85 )
calories_indexs

# indexes are : 1  3  4  8 39 43 44 45 48 50 53 54 61 68


## 4. protein : discrete variable  
summary(Data2$protein)
table(Data2$protein)
barplot(Data2$protein, main = "Barchart of 'protein' feature" , ylab = "Frequency" , xlab = "Protein in Cereal", col = 'purple' )
# we can see from the bar plot that out of 74 records only 2 records has protein value '6' 


## 5. Fat : Discrete variable.
summary(Data2$fat)
table(Data2$fat)
hist(Data2$fat , main = "Histogram of Feature : Fat" , xlab = "Fat" , breaks = 20)


## 6. Sodium : continuous integer variable.
summary(Data2$sodium)
hist(Data2$sodium, main = "Histogram of Feature : Sodium" , xlab = "Sodium" , breaks = 50 , probability =  FALSE )
boxplot(Data2$sodium,ylab = "sodium")

## so some point has sodium value as 0, that must be a mistake. But i won't remove those points. 

## 7. fiber :  Discrete variable. 
summary(Data2$fiber)
table(Data2$fiber)
hist(Data2$fiber , main = "Histogram of Feature : Fiber" , xlab = "Fiber" , breaks = 20)
# One data point here is way to far from other data points. Again I'll find it's index. 

fiber_index = which(Data2$fiber >10)
fiber_index
# index '4'.

## 8. Carbo : continuous numeric variable
summary(Data2$carbo)
hist(Data2$carbo, main = "Histogram of Feature : Carbohydrate" , xlab = "Carbohydrate" , breaks = 20)
boxplot(Data2$carbo,ylab = "Carbohydrate")


## 9. sugars : Discrete variable
summary(Data2$sugars)
table(Data2$sugars)
hist(Data2$sugars, main = "Histogram of Feature : Sugar" , xlab = "Sugar" , breaks = 20)

## 10. potass : continuous integer variable.
summary(Data2$potass)
hist(Data2$potass , main = "Histogram of Feature : Potassium" , xlab = "Potassium" , breaks = 30)
## there are some points that are far away.  
boxplot(Data2$potass,ylab = "potassium")
## there are possible 4 outliers. Let's find there indexes. 
potass_indexs = which(Data2$potass > 250)
potass_indexs
## indexes are 1,3,4 and  51


## 11. vitamins:
table(Data2$vitamins)

## 12. Rating : 
hist(Data2$rating , main = "Histogram of Feature  : Rating" , xlab = "Rating", breaks = 20)
#There seems to be 1 point far away from the distribution. 
boxplot(Data2$rating , ylab = "Rating" , main= "Boxplot of feature: Rating")
# Boxplot also shows one outlier 
rating_index = which(Data2$rating > 80)
rating_index
## Index is '4'


## So , Index number '4' has been a outlier in Rating, potass , fiber and calories. So i'll remove datapoint from that index.
Data3 <- Data2
dim(Data3)
Data3 <- Data3[-c(4), ]
dim(Data3)
# Successfully, removed that index.



####################################################################
##### Let's find relationship of features with respect to rating. 

####################################################################
######## Scatter - plots #############

## install.packages("car")
library(car)
scatterplotMatrix(~rating+sugars+potass+calories, data=Data3, main="Scatterplot Matrix with Features : Rating ,Sugar, Potassium & Calories")


## From the above scatter plots i can tell that, Sugar and calories has -ve impact on the Rating Feature. 
## Potassium feature has a slight positive curve that mean it is slightly +vely correlated to rating. 




scatterplotMatrix(~rating+fat+sodium+protein, data=Data3, main="Scatterplot Matrix with Features : Rating , Fat, Sodium & Protein")

## From the above scatter plots i can tell that, fat and sodium has -ve impact on the Rating Feature. 
## protein feature is +vely correlated to rating.

scatterplotMatrix(~rating+fiber+carbo+vitamins, data=Data3, main="Scatterplot Matrix with Features : Rating , Fiber, Carbo & Vitamins")
## Fiber feature is positively correlated to our target feature. 
## Vitamins feature is slightly correlated to our target feature. 

scatterplotMatrix(~rating+shelf+weight+cups,  data=Data3, main="Scatterplot Matrix with Features : Rating , Shelf, Weight & Cups")
## Features shelf , weight and cups has either a slight line or slightly -ve curve. This features are not important to our target feature. 
## I will plot a individual scatter-plots and co-relation plot and if they are not correlated then I'll remove them.


scatter.smooth(x = Data1$rating , y = Data1$weight , main = "Scatter - Smooth Plot : Rating ~ Weight" )


scatter.smooth(x = Data1$rating , y = Data1$cups , main = "Scatter - Smooth Plot : Rating ~ Cups" )


scatter.smooth(x = Data1$rating , y = Data1$shelf , main = "Scatter - Smooth Plot : Rating ~ Shelf" )


#######################################################
########## Correlation matrix .
########## Correlation plot only takes numeric fields as argument so we need to find numeric fields from the data frame : Data3
#######################################################


num_columns <- unlist(lapply(Data3, is.numeric)) ## returns a Boolean Vector telling which of our indexes.
data_numeric <- subset(Data3[,num_columns])
corrcereal <- cor(data_numeric)
corrcereal


## install.packages("corrplot")
library(corrplot)
corrplot(corrcereal , method = 'number' )

#### So cups and shelf have no correlation with our target variable 'Rating' and with any other feature. so I'll remove them 
### weight feature don't have much co-relation with 'Rating' but it is highly correlated to 'calories' feature so I'll keep it for now.

###########################################
## dropping 'cups' and 'shelf' feature.

Data4 <- Data3
dim(Data4)
head(Data4)
Data4 <- subset(Data4, select = -c(13,15))  ## Removing a feature. 
head(Data4)
dim(Data4)

######################################################################################################
## initially we had 77 rows and 16 columns in the data set, Now we have 73 rows and 14 columns. ##
## I'll save this clean data in R file ##


# save(Data4,file="cleaned_cereal_data.RData")









