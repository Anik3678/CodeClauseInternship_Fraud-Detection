#CodeClause Internship Task-1

#Aim 
#Detect fraudulent transactions in a credit card dataset.

#Description
#Apply anomaly detection or classification algorithms to identify potentially
#fraudulent activities.

# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(pROC)

#Load the data
credit<-read.csv("C:\\Users\\User\\Documents\\Data Science\\Internship\\CodeClause Internship\\Task-1\\credit_risk_dataset.csv")

#View the data
View(credit)

#Column names of data
colnames(credit)

#Observing no. of rows & columns of the dataset
dim(credit)

#Checking for missing value
any(is.na(credit))

#Total no. of missing value
sum(is.na(credit))

#Remove the missing value
new_credit<-na.omit(credit)

#Remove the unnecessary columns
new_credit<-new_credit[,-c(12)]

#Observing first 20 rows of the dataset
new_credit_data<-head(new_credit,150)

#Update with actual column names
colnames(new_credit_data) <- c("Age", "Income", "Housing status", "Loan ID", "Loan type", 
                    "Category", "Transaction Amount", "Interest Rate", 
                    "Fraud Flag","Prob of fraud","Verified Transaction") 

#View the data
View(new_credit_data)

#Check again for any missing value
any(is.na(new_credit_data))

#Data Preprocessing
new_credit_data$`Fraud Flag` <- as.factor(new_credit_data$`Fraud Flag`)  # Convert Fraud_Flag to factor
new_credit_data$`Housing status` <- as.factor(new_credit_data$`Housing status`)
new_credit_data$`Loan type` <- as.factor(new_credit_data$`Loan type`)
new_credit_data$Category <- as.factor(new_credit_data$Category)
new_credit_data$`Verified Transaction` <- as.factor(new_credit_data$`Verified Transaction`)

#Makes all names syntactically valid
colnames(new_credit_data) <- make.names(colnames(new_credit_data))  

#Split data into train and test sets
set.seed(123)
trainIndex <- createDataPartition(new_credit_data$Fraud.Flag, p = 0.8, list = FALSE)

train_data <- new_credit_data[trainIndex, ]
test_data <- new_credit_data[-trainIndex, ]

#Using Random Forest model
random_forest_model <- randomForest(Fraud.Flag  ~ ., data = train_data, importance = TRUE)
random_forest_model

#Predict and Evaluate
predicted <- predict(random_forest_model, test_data)
predicted

#Original values
observed<-new_credit_data$Fraud.Flag
observed

#Checking accuracy
accuracy<-mean(predicted==observed)
accuracy

#Interpret the Model
varImpPlot(random_forest_model)















