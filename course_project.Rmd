---
title: "Practical Machine Learning Course Project"
author: "Tjeerd Luykx"
date: "Friday, August 21, 2015"
output: html_document
---

## Introduction

### 1.1. Executive Summary

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, the goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

### 1.2. Project Goal

The goal of the project is to predict the manner in which they did the exercise. This is the "classe" variable in the training set. Any of the other variables can be used to predict with. A report is created describing how the model is bued, how cross validation is used, what the expected out of sample error is, and why specific choices are made.
 
## 2. Method

### 2.1 Library Load

For this project specific libraries are loaded. They are listed below. Additionaly, the seed is set at a random number. 

```{r, warning=FALSE, message=FALSE}
library(caret)
library(e1071)
library(randomForest)

# Setting seed.
set.seed(123456789)
```

### 2.2 Data Download

The training and the test set are downloaded from a url. The urls are listed below. Both datasets are defined as data frame objects for analysis purposes.


```{r}
# Training set URL.
trainUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

# Test set URL.
testUrl <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

# Define training data frame.
training <- read.csv(url(trainUrl), na.strings=c("NA","#DIV/0!",""))

# Define test data frame.
testing <- read.csv(url(testUrl), na.strings=c("NA","#DIV/0!",""))
```

### 2.3 Data Partioning

For data preperation and analysis the training dataset is partioned. 60% for training, 40% for testing. 

```{r}
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
Training <- training[inTrain, ]
Testing <- training[-inTrain, ]
dim(Training); dim(Testing)
```

### 2.3 Data Cleaning

The first step in data cleaning, is to remove the variables with near zero variance. An object is defined to display the near zero variables. 

```{r}
DataNZV <- nearZeroVar(Training, saveMetrics=TRUE)
```


```{r}
NZVvars <- names(Training) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
"kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
"max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
"var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
"stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
"kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
"max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
"kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
"skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
"amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
"skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
"max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
"amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
"avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
"stddev_yaw_forearm", "var_yaw_forearm")
Training <- Training[!NZVvars]
dim(Training)
```

Next, the first column is removed from the dataset due to insignificance for further analysis. 

```{r}
Training <- Training[c(-1)]
```

NAs are removed with a for loop and an if statement. 

```{r}
# creating data subet by looping over columns in dataset and erasing column if more than 60% of obseravtions are NAs and if columns are equal.

trainingLoop <- Training 
for(i in 1:length(Training)) { 
        if(sum(is.na(Training[,i]))/nrow(Training)>=.6) { 
        for(j in 1:length(trainingLoop)) {
            if(length(grep(names(Training[i]), names(trainingLoop)[j]))==1)  { 
                trainingLoop <- trainingLoop[,-j] 
            }   
        } 
    }
}

# Defining TrainingLoop to Training data frame.
Training <- trainingLoop
rm(trainingLoop)
```

All data cleaning procedures are performed on the testing dataset. 

```{r}
clean1 <- colnames(Training)
clean2 <- colnames(Training[, -58])
Testing <- Testing[clean1]
testing <- testing[clean2]
```

### 2.4 Data Coercing

In order to perform an adequate analysis with Random Forests, the data is coerced in to the same type. 

```{r}
for (i in 1:length(testing)) {
        for(j in 1:length(Training)) {
        if( length(grep(names(Training[i]), names(testing)[j])) ==1)  {
            class(testing[j]) <- class(Training[i])
        }      
    }      
}


testing <- rbind(Training[2, -58], testing)
```

## 3. Practical Machine Learning Algorithm: Random Forests 

In comparison to a Decision Tree prediction, a Random Forest Algorithm was choses due it's generally superior in-sample error prediction. 

```{r}
modFitB1 <- randomForest(classe ~., data = Training)

# Prediction of in-sample error.
predictionsB1 <- predict(modFitB1,Testing, type = "class")

confusionMatrix(predictionsB1,Testing$classe)
```

## 4. File Creation for Project Submission

```{r}
predictionsB2 <- predict(modFitB1,testing, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)
```






