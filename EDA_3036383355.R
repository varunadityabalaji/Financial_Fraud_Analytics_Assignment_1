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
# ------- 1. Distinguish Attributes -------

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

# ------- 2. Missing Values and Outliers -------
# Print the number of missing values
print("Total missing values in the dataset")
sum(is.na(dataset))

#Missing values per column
print("Missing values per column:")
colSums(is.na(dataset))


# Synthetic Distribution Curve Imputation for Missing Values

set.seed(42)

# Find numeric columns in the dataset
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]

# For each numeric column, impute missing values
for (col in numeric_cols) {
  missing_idx <- which(is.na(dataset[[col]]))
  if (length(missing_idx) > 0) {
    observed <- dataset[[col]][!is.na(dataset[[col]])]
    observed <- observed[!is.na(observed)] # Ensure no NA in observed
    if (length(observed) > 0) {
      imputed_values <- sample(observed, length(missing_idx), replace = TRUE)
    } else {
      imputed_values <- rep(NA, length(missing_idx))
    }
    dataset[[col]][missing_idx] <- imputed_values
  }
}

# Find non-numeric columns
non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]

#For Non Numeric columns, replace missing values with mode
# Convert blank strings in non-numeric columns to NA
for (col in non_numeric_cols) {
  if (is.character(dataset[[col]])) {
    dataset[[col]][dataset[[col]] == ""] <- NA
  }
}

# For each non-numeric column, impute missing values by random sampling
for (col in non_numeric_cols) {
  missing_idx <- which(is.na(dataset[[col]]))
  if (length(missing_idx) > 0) {
    observed <- dataset[[col]][!is.na(dataset[[col]])]
    observed <- observed[!is.na(observed)] # Ensure no NA in observed
    if (length(observed) > 0) {
      imputed_values <- sample(observed, length(missing_idx), replace = TRUE)
    } else {
      imputed_values <- rep(NA, length(missing_idx))
    }
    dataset[[col]][missing_idx] <- imputed_values
  }
}


# Print out the NAs in card4 column
cat("Indices of NAs in card4 column:", which(is.na(dataset$card4)), "\n")
cat("Values of NAs in card4 column:", dataset$card4[is.na(dataset$card4)], "\n")


#Plot box Plot for TxnAmt to see outliers
ggplot(dataset, aes(y = TxnAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Transaction Amount", y = "Transaction Amount") +
  theme_minimal() 

#remove the outliers
# Cap outliers for all numeric columns using Median ± 3 x IQR/(2 x 0.6745)
cap_iqr_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  lower_limit <- Q1 - 1.5 * IQR_val
  upper_limit <- Q3 + 1.5 * IQR_val
  x[x < lower_limit] <- lower_limit
  x[x > upper_limit] <- upper_limit
  return(x)
}

# Apply to all numeric columns
for (col in numeric_cols) {
  dataset[[col]] <- cap_iqr_outliers(dataset[[col]])
}

print("Box plot after removing outliers")

ggplot(dataset, aes(y = TxnAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Transaction Amount After Removing Outliers", y = "Transaction Amount") +
  theme_minimal()

# ------- 3. Univariate Analysis -------

# Histogram of TxnAmt
ggplot(dataset, aes(x = TxnAmt)) +
  geom_histogram(binwidth = 50, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Transaction Amount", x = "Transaction Amount", y = "Count") +
  theme_minimal()

ggplot(dataset %>% filter(TxnAmt < 500), aes(x = TxnAmt)) +
  geom_histogram(binwidth = 10, fill = "deepskyblue", color = "black") +
  labs(title = "Histogram of Transaction Amounts Below 500", x = "Transaction Amount (<500)", y = "Count") +
  theme_minimal()

# Mean, Median, Mode for TxnAmt
txn_mean <- mean(dataset$TxnAmt, na.rm = TRUE)
txn_median <- median(dataset$TxnAmt, na.rm = TRUE)
txn_max <- max(dataset$TxnAmt, na.rm = TRUE)
txn_min <- min(dataset$TxnAmt, na.rm = TRUE)
# Mode function for numeric
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
txn_mode <- get_mode(dataset$TxnAmt)
cat("TxnAmt Mean:", txn_mean, "\n")
cat("TxnAmt Median:", txn_median, "\n")
cat("TxnAmt Mode:", txn_mode, "\n")
cat("TxnAmt Max:", txn_max, "\n")
cat("TxnAmt Min:", txn_min, "\n")

# Histogram of card6
ggplot(dataset, aes(x = card6, y = (..count..) / sum(..count..))) +
  geom_bar(fill = "orange", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage Distribution of card6", x = "card6", y = "Percentage") +
  theme_minimal()

# Histogram of card4
ggplot(dataset, aes(x = card4, y = (..count..) / sum(..count..))) +
  geom_bar(fill = "purple", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage Distribution of card4", x = "card4", y = "Percentage") +
  theme_minimal()

# Histogram of P_emaildomain
ggplot(dataset, aes(x = P_emaildomain, y = (..count..) / sum(..count..))) +
  geom_bar(fill = "green", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage Distribution of P_emaildomain", x = "P_emaildomain", y = "Percentage") +
  theme_minimal()

# Histogram of R_emaildomain
ggplot(dataset, aes(x = R_emaildomain, y = (..count..) / sum(..count..))) +
  geom_bar(fill = "red", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Percentage Distribution of R_emaildomain", x = "R_emaildomain", y = "Percentage") +
  theme_minimal()



# ------- 4. Bivariate Analysis -------

