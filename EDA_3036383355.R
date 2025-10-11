# This is a sample R script for assignment 1.

chooseCRANmirror(graphics=FALSE, ind=1) # or use setRepositories()

# install.packages('corrplot')
# install.packages('caret')
# install.packages('tidyverse')
# install.packages('ggplot2')
# install.packages('dplyr')
library(corrplot)
library(caret)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lubridate)


# Load the data
dataset = read.csv("./A1_data.csv")

# Exploratory Data Analysis
# 1. Distinguish Attributes

#Structure of the dataset
print("Structure of the dataset:")
str(dataset)

#Summary of the dataset
print("Summary of the dataset:")
summary(dataset)

#Bar graph too see number of cases per class
ggplot(dataset, aes(x = factor(isFraud), fill = factor(isFraud))) +
  geom_bar(color = "black") +
  labs(title = "Number of Cases per Class", x = "isFraud", y = "Total", fill = "isFraud") +
  theme_minimal()

#Print out the absolute number of cases per class
class_counts <- table(dataset$isFraud)
print("Number of cases per class:")
print(class_counts)

#2 Missing Values and Outliers
# Print the number of missing values
print("Total missing values in the dataset")
sum(is.na(dataset))

#Missing values per column
print("Missing values per column:")
colSums(is.na(dataset))


# Synthetic Distribution Curve Imputation for Missing Values
# Find numeric columns in the dataset
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]

# For each numeric column, impute missing values
for (col in numeric_cols) {
  missing_idx <- which(is.na(dataset[[col]]))
  if (length(missing_idx) > 0) {
    observed <- dataset[[col]][!is.na(dataset[[col]])]
    # Sample with replacement from observed values
    imputed_values <- sample(observed, length(missing_idx), replace = TRUE)
    dataset[[col]][missing_idx] <- imputed_values
  }
}

#Plot box Plot for TxnAmt to see outliers
ggplot(dataset, aes(y = TxnAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Transaction Amount", y = "Transaction Amount") +
  theme_minimal() 

#remove the outliers
Q1 <- quantile(dataset$TxnAmt, 0.25)
Q3 <- quantile(dataset$TxnAmt, 0.75)
IQR <- Q3 - Q1  
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR
dataset <- dataset %>% filter(TxnAmt >= lower_bound & TxnAmt <= upper_bound)
print("Box plot after removing outliers")
ggplot(dataset, aes(y = TxnAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Transaction Amount After Removing Outliers", y = "Transaction
  Amount") +
  theme_minimal()
  