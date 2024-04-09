##########################################
## Customer Churn in Banking
## 
##
###########################################

library(rpart)
library(rpart.plot)
#install.packages("vip")
library(vip)


# Set up the working directory using setwd()
setwd("/Users/aseemhsalim/Documents/MS IE/Classes/Fall23/Project/BI/")
# getwd() # verify working directory

# Extracting csv data from churn.csv to df0
df0 = read.csv("churn.csv",sep = ",",header = TRUE)
dim(df0) # 10000 rows and 14 variables.

str(df0) # There are 10000 observations with 14 variables.



# Data Pre-processing
# -------------------

# Check number of unique values in each column.
apply(df0, MARGIN = 2, function(x) length(unique(x)))

# Remove RowNumber and CustomerId as those are unique for each rows
df1 = df0
df1$RowNumber = NULL
df1$CustomerId = NULL

# Remove Surname as it's not relevant
df1$Surname = NULL

# Display number of unique values in each column.
apply(df1, MARGIN = 2, function(x) length(unique(x)))

str(df1) # Now there are 10000 observations with 11 variables.


# Check for missing values

columns <- colnames(df1)
columns
# Convert the blank spaces as NA
for (i in columns) { 
  df1[[i]] <- ifelse(trimws(df1[[i]]) == "",  
                     NA, df1[[i]])
}

sum(is.na(df1) == TRUE) # Get total number of missing values
colSums(is.na(df1)) # Find missing values in each column
# There are no missing values.

df2 = df1


# Outlier detection using boxplots for continuous variable
num_cols = c("CreditScore", "Age", "Tenure", "Balance", "EstimatedSalary")
cat_cols = c("Geography","Gender","NumOfProducts","HasCrCard","IsActiveMember")
out_col = c("Exited")

op = par()

par(mfrow = c(2,3))

for (i in num_cols) {
  boxplot(df2[i], ylab = i)
}

mtext("Outlier detection for continuous variables", 
      side = 3, line = - 2, outer = TRUE)

par(op)

# Outliers present in below columns
# CreditScore, Age

# 
par(mfrow = c(1,1))
out <- boxplot.stats(df2$CreditScore)$out
boxplot(df2$CreditScore,
        ylab = "CreditScore"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

# CreditScore Outliers
# 376 376 363 359 350 350 358 351 365 367 350 350 382 373 350

out2 <- boxplot.stats(df2$Age)$out
boxplot(df2$Age,
        ylab = "Age"
)
mtext(paste("Outliers Range: ", paste(range(out2), collapse = " - ")))

# Age Outliers Range from 63 to 92. 359 Outliers present


# Convert chr variables to qualitative variables
# Geography, Gender
df3 = df2
df3$Geography = factor(df3$Geography)
df3$Gender = factor(df3$Gender)

# Convert num columns having less than 5 distinct values
conv <- sapply(df3, function(x) is.numeric(x) && length(unique(x)) < 5)
df3[conv] <- lapply(df3[conv], as.factor)

str(df3)


# Remove outliers
df4 = df3

# CreditScore
# Age
# Tenure
# Balance
# EstimatedSalary


df5 = df4

# Remove outliers in CreditScore
df5$zscore = as.data.frame(
  abs(df5$CreditScore-mean(df5$CreditScore))/sd(df5$CreditScore))

df5 <- subset(df5, df5$zscore < 3)

dim(df5) # 8 outliers removed


# Remove outliers in Age
df5$zscore = as.data.frame(
  abs(df5$Age - mean(df5$Age))/sd(df5$Age))

df5 <- subset(df5, df5$zscore < 3)

dim(df5) # 133 outliers removed

df6 = df5[,-12]
View(df6)



# Get correlation of continuos 
library(corrplot)
cor1 = cor(df6[,num_cols])
corrplot(cor1, type = 'upper') # Plot correlation


corrplot(cor1, method="color", 
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         sig.level = 0.01, insig = "blank", # Combine with significance
         diag=FALSE
)

# Almost no correlation between continuous variables.


# Barplots of Categorical variables including response Exited
barplot(table(df6$Exited), col=c("lightblue","darkred"),
        main="Exited No(0) vs. Yes (1)",
        xlab = "Exited", ylab="Count")
table(df6$Exited)
# 7841 customers didn't exit
# 2018 customer churn in given dataset

barplot(table(df6$Geography), col=c("lightblue","darkred"),
        main="Geography frequency",
        xlab = "Geography", ylab="Count")
table(df6$Geography)
# Data contains details from 3 countries
# France Germany   Spain 
# 4940   2474      2445 

barplot(table(df6$Gender), col=c("lightblue","darkred"),
        main="Gender frequency",
        xlab = "Gender", ylab="Count")
table(df6$Gender)
# Almost equal distribution for male and female customer.
# Female   Male 
# 4472     5387 

barplot(table(df6$NumOfProducts), col=c("lightblue","darkred"),
        main="NumOfProducts frequency",
        xlab = "NumOfProducts", ylab="Count")
table(df6$NumOfProducts)
# Most of the customers have 1 or 2 products.
# Very few customers having 3 or 4 products.

barplot(table(df6$HasCrCard), col=c("lightblue","darkred"),
        main="HasCrCard frequency",
        xlab = "HasCrCard", ylab="Count")
table(df6$HasCrCard)
# Almost 70% of the customers are having credict card.

barplot(table(df6$IsActiveMember), col=c("lightblue","darkred"),
        main="IsActiveMember frequency",
        xlab = "IsActiveMember", ylab="Count")
table(df6$IsActiveMember)
# Almost 50% plus customers are active members.




# Check skewness of continuous variables.
hist(df6$CreditScore, 
     main="CreditScore Distribution",
     xlab = "CreditScore")

hist(df6$Age, 
     main="Age Distribution",
     xlab = "Age")

hist(df6$Tenure, 
     main="Tenure Distribution",
     xlab = "Tenure")

hist(df6$Balance, 
     main="Balance Distribution",
     xlab = "Balance")

hist(df6$EstimatedSalary, 
     main="EstimatedSalary Distribution",
     xlab = "EstimatedSalary")


df7 = df6
# Normalize numerical variables
for (i in num_cols) {
  df7[i] = scale(df7[i],center = TRUE,scale = TRUE)
}

head(df7)



set.seed(123)
sample_size = 6901 # 70% of data for train

# Create a random sample of row indices
sample_indices = sample(nrow(df7), sample_size)

# Subset the dataframe
df_train = df7[sample_indices, ]
df_test = df7[-sample_indices, ]


# Construct a decision tree model using rpart.
decTree = rpart(Exited ~ ., data = df_train, method = "class")
summary(decTree)

# Visualize and interpret decision tree model.
# One aspect of interpretation is understanding important variables from dataset.
rpart.plot(decTree) # Visualize decision tree model


# Variable importance is usually determined by features used for splitting at 
# nodes.
vip(decTree)

# Top 5 important variables are:
# Age, NumOfProducts, IsActiveMember, Balance and Geography.

top5_cols = c("Age", "NumOfProducts", "IsActiveMember", "Balance", "Geography")
df8 = df7[top5_cols]
df8$Exited = df7$Exited
dim(df8)
head(df8)


# Subset the dataframe
df2_train = df8[sample_indices, ]
df2_test = df8[-sample_indices, ]

dim(df2_train)
dim(df2_test)


# Modelling 

# Gradient Boosting

# Install and load required packages

library(data.table)

library(xgboost)
library(caret)
# Convert df2_train and df2_test to data.tables
setDT(df2_train)
setDT(df2_test)

# Function to convert factors to one-hot encoding
one_hot_encode = function(dt) {
  for (col in names(dt)) {
    if (is.factor(dt[[col]])) {
      dt[, (col) := as.numeric(as.factor(dt[[col]]))]
    }
  }
}

# Apply one-hot encoding to training and test datasets
one_hot_encode(df2_train)
one_hot_encode(df2_test)
# Convert labels to binary 0 and 1
df2_train$Exited = as.numeric(as.factor(df2_train$Exited)) - 1
df2_test$Exited = as.numeric(as.factor(df2_test$Exited)) - 1

# Re-check if conversion is successful
unique(df2_train$Exited) # should be 0 and 1
unique(df2_test$Exited)  # should be 0 and 1

# Now, reprepare the matrices for xgboost
train_matrix = as.matrix(df2_train[, -ncol(df2_train), with = FALSE])
train_label = df2_train$Exited
test_matrix = as.matrix(df2_test[, -ncol(df2_test), with = FALSE])
test_label = df2_test$Exited

# Convert to xgboost DMatrix
dtrain = xgb.DMatrix(data = train_matrix, label = train_label)
dtest = xgb.DMatrix(data = test_matrix, label = test_label)

# Proceed with the model training, prediction, and evaluation

# Prepare matrices for xgboost
train_matrix = as.matrix(df2_train[, -ncol(df2_train), with = FALSE])
train_label = df2_train$Exited
test_matrix = as.matrix(df2_test[, -ncol(df2_test), with = FALSE])
test_label = df2_test$Exited

# Convert to xgboost DMatrix
dtrain = xgb.DMatrix(data = train_matrix, label = train_label)
dtest = xgb.DMatrix(data = test_matrix, label = test_label)
# Set parameters
params = list(
  booster = "gbtree",
  objective = "binary:logistic",
  eta = 0.01,
  max_depth = 3
)

# Train the model
xgb_model = xgb.train(params = params, data = dtrain, nrounds = 50)

# Make predictions
predicted_probs = predict(xgb_model, dtest)
predicted_classes = ifelse(predicted_probs > 0.5, 1, 0)

# Evaluate the Model
confusionMatrix(factor(predicted_classes, levels = c(0, 1)), factor(test_label, levels = c(0, 1)))


sum(is.na(df2_train)) # Total number of NA values in df2_train

# KNN

library(class)

# Assuming df2_train and df2_test are already prepared and include 'Exited' column

# Prepare the training and test datasets for KNN
train_data = subset(df2_train, select = -Exited)
test_data = subset(df2_test, select = -Exited)

train_labels = df2_train$Exited
test_labels = df2_test$Exited
# Check the number of rows in train_data and length of train_labels
print(nrow(train_data))
print(length(train_labels))

# If they are different, it's necessary to investigate and align them

# Convert factors to characters to avoid issues in KNN
train_data[] = lapply(train_data, function(x) if(is.factor(x)) as.character(x) else x)
test_data[] = lapply(test_data, function(x) if(is.factor(x)) as.character(x) else x)

# Convert characters to numerics
train_data[] = lapply(train_data, function(x) if(is.character(x)) as.numeric(x) else x)
test_data[] <- lapply(test_data, function(x) if(is.character(x)) as.numeric(x) else x)

# Apply KNN. Let's start with k = 5
set.seed(123) # for reproducibility
knn_pred = knn(train = train_data, test = test_data, cl = train_labels, k = 5)

# Evaluate the model
confusionMatrix = table(Predicted = knn_pred, Actual = test_labels)
print(confusionMatrix)

# Calculate accuracy
accuracy = sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", accuracy))


# Support Vector Machine
dim(df6)
View(df6)
write.csv(df6,"svm_data.csv",row.names=FALSE)
str(df6)
library(e1071)
# 1. Feature Scaling (scale only numerical features)

df6$CreditScore <- scale(df6$CreditScore)
df6$Age <- scale(df6$Age)
df6$Tenure <- scale(df6$Tenure)
df6$Balance <- scale(df6$Balance)
df6$EstimatedSalary <- scale(df6$EstimatedSalary)
# 2. Split the data into training and testing sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(df6), size = 0.8 * nrow(df6))
train_data <- df6[train_indices, ]
test_data <- df6[-train_indices, ]
svm_model <- svm(Exited ~ ., data = train_data, type = 'C-classification', kernel = 'radial')
# 4. Predict and Evaluate the Model
predictions <- predict(svm_model, test_data)
confusionMatrix <- table(predictions, test_data$Exited)
print(confusionMatrix)
# Extracting the elements of the confusion matrix
TP <- confusionMatrix[2, 2] # True Positives
TN <- confusionMatrix[1, 1] # True Negatives


# Calculating Accuracy, Recall, and Precision
accuracy <- (TP + TN) / sum(confusionMatrix)


# Print the results
print(paste("Accuracy:", accuracy))

# Thus, we get an accuracy of 87.22 %

str(df8)


# Running SVM on the important cariables obtained from Decision Tree.

# 2. Split the data into training and testing sets
set.seed(123) # For reproducibility
train_indices <- sample(1:nrow(df8), size = 0.8 * nrow(df8))
train_data <- df8[train_indices, ]
test_data <- df8[-train_indices, ]
svm_model <- svm(Exited ~ ., data = train_data, type = 'C-classification', kernel = 'radial')
# 4. Predict and Evaluate the Model
predictions <- predict(svm_model, test_data)
confusionMatrix <- table(predictions, test_data$Exited)
print(confusionMatrix)
# Extracting the elements of the confusion matrix
TP <- confusionMatrix[2, 2] # True Positives
TN <- confusionMatrix[1, 1] # True Negatives


# Calculating Accuracy
accuracy <- (TP + TN) / sum(confusionMatrix)


# Print the results
print(paste("Accuracy:", accuracy))

# Thus, we get an accuracy of 87.52 % using important variables from decision 
# tree which is a slight imporovement on our basic SVM model.
