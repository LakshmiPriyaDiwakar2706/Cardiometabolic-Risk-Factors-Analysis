# Install and load the 'car' package
install.packages("car")

data <- read.csv("C:/Users/laksh/OneDrive/Desktop/Data Stats/Household_final_dataset.csv")
data
str(data)


data$Gender <- as.factor(data$Gender)
data$alcohol <- as.factor(data$alcohol)
data$Hypertension <- as.factor(data$Hypertension)
data$Obesity <- as.factor(data$Obesity)
data$hv115...current.marital.status. <- as.factor(data$hv115...current.marital.status.)
data$Education.Level <- as.factor(data$Education.Level)
data$hv106..highest.education.level.attained. <- as.factor(data$hv106..highest.education.level.attained.)
data$hv270..wealth.index.combined. <- as.factor(data$hv270..wealth.index.combined.)
data$BMI <- as.numeric(as.character(data$BMI))


##########################LINEAR REGRESSION#################################################################

#Linear Regression - Systolic bp
model_1 <- lm(final_blood_pressure_systolic ~ age +  Height.in..cms.+ Weight..in.Kgs.+ smoking
             + final_blood_pressure_diastolic  + hv115...current.marital.status. + Education.Level +Gender + BMI  + alcohol, data= data )
summary(model_1)


#Checking for multicollinearity
library(car)

# Assuming 'model' is your lm or glm model object
vif_values <- vif(model_1)
print(vif_values)


#Linear Regression - Diastolic bp
model_2 <- lm(final_blood_pressure_diastolic ~ age + Height.in..cms.+ Weight..in.Kgs. + smoking
              + final_blood_pressure_systolic  + Gender + BMI + hv115...current.marital.status. + Education.Level+ alcohol, data= data )
summary(model_2)

#checking for multicollinearity
library(car)

# Assuming 'model' is your lm or glm model object
vif_values <- vif(model_2)
print(vif_values)


#Linear Regression - BMI
model_3 <- lm(BMI ~ age + Weight..in.Kgs. +  Height.in..cms.+ smoking
              + final_blood_pressure_systolic  + Gender + final_blood_pressure_diastolic + + hv115...current.marital.status. + Education.Level+ alcohol, data= data )
summary(model_3)

#Checking for multicollinearity
library(car)

# Assuming 'model' is your lm or glm model object
vif_values <- vif(model_2)
print(vif_values)

########################### LOGISTIC REGRESSION ###########################################################

####logistic regression first part(without adjustment)
install.packages("brglm")

library(brglm)

data$age <- as.numeric(as.character(data$age))
data$BMI <- as.numeric(as.character(data$BMI))

model <- glm(Hypertension ~ age + Weight..in.Kgs. + Height.in..cms. + smoking + 
               Gender + BMI + hv115...current.marital.status. + Education.Level , family = binomial(link = "logit"), data = data)


summary(model)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)


model <- brglm(Obesity ~ age +  Height.in..cms. + smoking + 
               Gender + BMI +  hv115...current.marital.status. + Education.Level, family = binomial(link = "logit"), data = data)

summary(model)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)


###logistic regression demographic vairables
#Logistic Regression Hypertension
model_4 <- glm(Hypertension ~ age + Gender + hv115...current.marital.status. + Education.Level , family = binomial(link = "logit"), data = data)

summary(model_4)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)

#PREDICING AND CREATING CONFUSION MATRIX
# Predict probabilities
predicted_probabilities <- predict(model_4, type = "response")

# Convert probabilities to binary classes based on a 0.5 threshold
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual classes from your dataset
actual_classes <- data$Hypertension


# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)


#CALCULATING ACCURACY, RECALL, PRECISION AND F1 SCORE
# Extracting elements from the confusion matrix
true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]

# Accuracy
accuracy <- (true_positives + true_negatives) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Recall (Sensitivity)
recall <- true_positives / (true_positives + false_negatives)
print(paste("Recall:", recall))

# Precision
precision <- true_positives / (true_positives + false_positives)
print(paste("Precision:", precision))

# F1-Score
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("F1-Score:", f1_score))


# CALCULATING AUC
library(pROC)

# Generate ROC object
roc_result <- roc(actual_classes, predicted_probabilities)

auc_value <- auc(roc_result)
print(paste("AUC:", auc_value))



# Logistic Regression - Obesity
model_5 <- glm(Obesity ~ age + Gender + hv115...current.marital.status. + Education.Level , family = binomial(link = "logit"), data = data)


summary(model_5)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)

#PREDICING AND CREATING CONFUSION MATRIX
# Predict probabilities
predicted_probabilities <- predict(model_5, type = "response")

# Convert probabilities to binary classes based on a 0.5 threshold
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual classes from your dataset
actual_classes <- data$Obesity


# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)


#CALCULATING ACCURACY, RECALL, PRECISION AND F1 SCORE
# Extracting elements from the confusion matrix
true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]

# Accuracy
accuracy <- (true_positives + true_negatives) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Recall (Sensitivity)
recall <- true_positives / (true_positives + false_negatives)
print(paste("Recall:", recall))

# Precision
precision <- true_positives / (true_positives + false_positives)
print(paste("Precision:", precision))

# F1-Score
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("F1-Score:", f1_score))


#PLOTTING ROC CURVE AND CALCULATING AUC
library(pROC)

# Generate ROC object
roc_result <- roc(actual_classes, predicted_probabilities)

auc_value <- auc(roc_result)
print(paste("AUC:", auc_value))




###logistic regression lifestyle
#Hypertension
model_6 <- glm(Hypertension ~  smoking+ + Weight..in.Kgs. + Height.in..cms. + BMI, family = binomial(link = "logit"), data = data)

summary(model_6)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)


#PREDICING AND CREATING CONFUSION MATRIX
# Predict probabilities
predicted_probabilities <- predict(model_6, type = "response")

# Convert probabilities to binary classes based on a 0.5 threshold
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual classes from your dataset
actual_classes <- data$Hypertension


# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)


#CALCULATING ACCURACY, RECALL, PRECISION AND F1 SCORE
# Extracting elements from the confusion matrix
true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]

# Accuracy
accuracy <- (true_positives + true_negatives) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Recall (Sensitivity)
recall <- true_positives / (true_positives + false_negatives)
print(paste("Recall:", recall))

# Precision
precision <- true_positives / (true_positives + false_positives)
print(paste("Precision:", precision))

# F1-Score
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("F1-Score:", f1_score))


# CALCULATING AUC
library(pROC)

# Generate ROC object
roc_result <- roc(actual_classes, predicted_probabilities)

auc_value <- auc(roc_result)
print(paste("AUC:", auc_value))


#Logistic Regression Obesity
model_7 <- brglm(Obesity ~  smoking+ + Weight..in.Kgs. + Height.in..cms. + BMI, family = binomial(link = "logit"), data = data)

summary(model_7)

# Calculate the odds ratios
odds_ratios <- exp(coef(model))

# Calculate the 95% confidence intervals for the odds ratios
ci <- exp(confint(model))

# Display the odds ratios and their 95% confidence intervals
print(odds_ratios)
print(ci)

#PREDICING AND CREATING CONFUSION MATRIX
# Predict probabilities
predicted_probabilities <- predict(model_7, type = "response")

# Convert probabilities to binary classes based on a 0.5 threshold
predicted_classes <- ifelse(predicted_probabilities > 0.5, 1, 0)

# Actual classes from your dataset
actual_classes <- data$Hypertension


# Create a confusion matrix
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)


#CALCULATING ACCURACY, RECALL, PRECISION AND F1 SCORE
# Extracting elements from the confusion matrix
true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[1, 2]
false_negatives <- confusion_matrix[2, 1]

# Accuracy
accuracy <- (true_positives + true_negatives) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))

# Recall (Sensitivity)
recall <- true_positives / (true_positives + false_negatives)
print(paste("Recall:", recall))

# Precision
precision <- true_positives / (true_positives + false_positives)
print(paste("Precision:", precision))

# F1-Score
f1_score <- 2 * ((precision * recall) / (precision + recall))
print(paste("F1-Score:", f1_score))


# CALCULATING AUC
library(pROC)

# Generate ROC object
roc_result <- roc(actual_classes, predicted_probabilities)

auc_value <- auc(roc_result)
print(paste("AUC:", auc_value))