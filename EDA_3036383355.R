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


# ------- 2. Missing Values and Outliers -------

# Print the number of missing values
print("Total missing values in the dataset")
sum(is.na(dataset))

#Missing values per column
print("Missing values per column:")
colSums(is.na(dataset))

# Impute missing values for numeric columns with mean
numeric_cols <- names(dataset)[sapply(dataset, is.numeric)]
for (col in numeric_cols) {
  missing_idx <- which(is.na(dataset[[col]]))
  if (length(missing_idx) > 0) {
    mean_val <- mean(dataset[[col]], na.rm = TRUE)
    dataset[[col]][missing_idx] <- mean_val
  }
}

# Impute missing values for categorical columns with mode
non_numeric_cols <- names(dataset)[sapply(dataset, function(x) !is.numeric(x))]
for (col in non_numeric_cols) {
  # Convert empty strings to NA
  if (is.character(dataset[[col]])) {
    dataset[[col]][dataset[[col]] == ""] <- NA
  }
  missing_idx <- which(is.na(dataset[[col]]))
  if (length(missing_idx) > 0) {
    # Mode function for categorical
    get_mode <- function(v) {
      uniqv <- unique(v[!is.na(v)])
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    mode_val <- get_mode(dataset[[col]])
    dataset[[col]][missing_idx] <- mode_val
  }
}

# ------- 3. Univariate Analysis -------

#Plot box Plot for TxnAmt
ggplot(dataset, aes(y = TxnAmt)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Transaction Amount", y = "Transaction Amount") +
  theme_minimal()

#Bar graph to see number of cases per class
ggplot(dataset, aes(x = factor(isFraud), fill = factor(isFraud))) +
  geom_bar(color = "black") +
  labs(title = "Number of Cases per Class", x = "isFraud", y = "Total", fill = "isFraud") +
  theme_minimal()

#Print out the absolute number of cases per class
class_counts <- table(dataset$isFraud)
print("Number of cases per class:")
print(class_counts)

# Histogram of TxnAmt
ggplot(dataset, aes(x = TxnAmt)) +
  geom_histogram(binwidth = 100, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Transaction Amount", x = "Transaction Amount", y = "Count") +
  theme_minimal()

# Histogram of TxnAmt < 500
ggplot(dataset %>% filter(TxnAmt < 500), aes(x = TxnAmt)) +
  geom_histogram(binwidth = 10, fill = "orange", color = "black") +
  labs(title = "Histogram of Transaction Amounts < 500",
       x = "Transaction Amount",
       y = "Count") +
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

# Stacked Barchart of isFraud as a factor against productCD
ggplot(dataset, aes(x = ProductCD, fill = factor(isFraud))) +
  geom_bar(position = "stack", color = "black") +
  labs(
    title = "Fraud vs Non-Fraud Cases by ProductCD",
    x = "ProductCD",
    y = "Count",
    fill = "isFraud"
  ) +
  theme_minimal()


# Stacked Barchart of isFraud as a factor against card4
ggplot(dataset, aes(x = card4, fill = factor(isFraud))) +
  geom_bar(position = "stack", color = "black") +
  labs(
    title = "Fraud vs Non-Fraud Cases by Card Company",
    x = "card4",
    y = "Count",
    fill = "isFraud"
  ) +
  theme_minimal()

# Stacked Barchart of isFraud as a factor against card6
ggplot(dataset, aes(x = card6, fill = factor(isFraud))) +
  geom_bar(position = "stack", color = "black") +
  labs(
    title = "Fraud vs Non-Fraud Cases by Card Company",
    x = "card6",
    y = "Count",
    fill = "isFraud"
  ) 
  theme_minimal()

# Print mean TxnAmt for each isFraud class
mean_txn_0 <- mean(dataset$TxnAmt[dataset$isFraud == 0], na.rm = TRUE)
mean_txn_1 <- mean(dataset$TxnAmt[dataset$isFraud == 1], na.rm = TRUE)
cat("Mean TxnAmt for isFraud = 0:", mean_txn_0, "\n")
cat("Mean TxnAmt for isFraud = 1:", mean_txn_1, "\n")


# Print number of cases for isFraud 0 and 1 where TxnAmt < 500
count_0_below_500 <- sum(dataset$isFraud == 0 & dataset$TxnAmt < 500, na.rm = TRUE)
count_1_below_500 <- sum(dataset$isFraud == 1 & dataset$TxnAmt < 500, na.rm = TRUE)
cat("Number of cases for isFraud = 0 and TxnAmt < 500:", count_0_below_500, "\n")
cat("Number of cases for isFraud = 1 and TxnAmt < 500:", count_1_below_500, "\n")

# Print number of cases for isFraud 0 and 1 where TxnAmt > 500
count_0_above_500 <- sum(dataset$isFraud == 0 & dataset$TxnAmt > 500, na.rm = TRUE)
count_1_above_500 <- sum(dataset$isFraud == 1 & dataset$TxnAmt > 500, na.rm = TRUE)
cat("Number of cases for isFraud = 0 and TxnAmt > 500:", count_0_above_500, "\n")
cat("Number of cases for isFraud = 1 and TxnAmt > 500:", count_1_above_500, "\n")


# Side-by-side box plot for log-transformed TxnAmt by isFraud
ggplot(dataset, aes(x = factor(isFraud), y = log1p(TxnAmt), fill = factor(isFraud))) +
  geom_boxplot() +
  labs(
    title = "Log(Transaction Amount) by Fraud Status",
    x = "isFraud",
    y = "Log(Transaction Amount)",
    fill = "isFraud"
  ) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
  theme_minimal()

# Create a new feature: TransactionHour extracted from TransactionDT
# Use lubridate for robust date-time handling

date_data <- dataset %>%
  mutate(TransactionDate = as.POSIXct(TxnDT, origin = "2017-12-01", tz = "UTC"),
         TransactionHour = hour(TransactionDate))

# This shows the distribution of transaction amounts by hour and fraud status

ggplot(date_data, aes(x = factor(TransactionHour), y = TxnAmt, fill = factor(isFraud))) +
  geom_violin(alpha = 0.6, trim = FALSE) +
  geom_jitter(aes(color = factor(isFraud)), width = 0.2, height = 0, alpha = 0.3, size = 0.5) +
  labs(x = 'Transaction Hour', y = 'Transaction Amount', fill = 'Is Fraud',
       title = 'Distribution of Transaction Amounts by Hour and Fraud Status') +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "red")) +
  scale_color_manual(values = c("0" = "steelblue", "1" = "red")) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(dataset, aes(x = id30, fill = factor(isFraud))) +
  geom_bar(position = "stack", color = "black") +
  labs(
    title = "Fraud vs Non-Fraud Cases by id30",
    x = "ProductCD",
    y = "Count",
    fill = "isFraud"
  ) +
  theme_minimal()
  
# Correlation matrix for numeric features
numeric_data <- dataset %>% select(where(is.numeric))

# Remove columns with zero variance
zero_var_cols <- sapply(numeric_data, function(x) sd(x, na.rm = TRUE) == 0)
numeric_data_filtered <- numeric_data[, !zero_var_cols]
corr_mat <- cor(numeric_data_filtered, use = "complete.obs")
corrplot(corr_mat, method = "color", tl.cex = 0.7, tl.col = "black", tl.srt = 45, mar = c(2,2,2,2))

















