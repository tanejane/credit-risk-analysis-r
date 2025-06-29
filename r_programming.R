# Objective: To assess the influence of loan duration and instalment commitment on the credit risk class of bank customers.

filepath =  "filepath"
df = read.csv(filepath)
View(df)
sum(is.na(df))
# 1. Average Loan Duration and Installment Commitment for Each Credit Risk Class
library(dplyr)
library(ggplot2)
avg = df %>%
  group_by(class) %>%
  summarize(avg_duration = mean(duration),
            avg_installment_commitment = mean(installment_commitment),
            sd_duration = sd(duration),
            sd_installment_commitment = sd(installment_commitment))
summary(avg)
print(avg)

# Plot
ggplot(avg, aes(x = class)) +
  # Bar and error bars for Loan Duration
  geom_bar(aes(y = avg_duration), fill = "powderblue", 
           stat = "identity", position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = avg_duration - sd_duration, 
                    ymax = avg_duration + sd_duration), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  # Add text labels for Loan Duration
  geom_text(aes(y = avg_duration, label = round(avg_duration, 2)), 
            position = position_dodge(width = 0.7), vjust = -0.7, color = "red") +
  
  # Bar and error bars for Installment Commitment
  geom_bar(aes(y = avg_installment_commitment), fill = "plum1", 
           stat = "identity", position = position_dodge(width = 0.7), alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_installment_commitment - sd_installment_commitment, 
                    ymax = avg_installment_commitment + sd_installment_commitment), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  # Add text labels for Installment Commitment
  geom_text(aes(y = avg_installment_commitment, label = round(avg_installment_commitment, 2)), 
            position = position_dodge(width = 0.7), vjust = -1.2, color = "red") +
  
  labs(title = "Average Loan Duration and Installment Commitment\nby Credit Risk Class",
       y = "Average Value", x = "Credit Risk Class") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) # Make the title in the middle


#2. How are the credit risk class distribution across different range of loan duration and installment commitment?
library(dplyr)
library(ggplot2)
library(scales) # For percentage formatting

# Categorize loan duration and installment commitment into ranges
df <- df %>%
  mutate(loan_range = cut(duration, breaks = c(0, 2, 5, 10, Inf), 
                          labels = c("0-2", "3-5", "6-10", "10+")),
         installment_commitment_range = cut(installment_commitment, 
                                            breaks = c(0, 2, 4, 6, Inf), 
                                            labels = c("0-2%", "2-4%", "4-6%", "6%+")))

# Validate and explore class distribution
class_distribution <- df %>%
  group_by(loan_range, installment_commitment_range, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count)) 
# Calculate proportions for each combination

print(class_distribution)

# Filter combinations with low sample sizes for better interpretation
threshold <- 5
low_sample_combinations <- class_distribution %>%
  filter(count < threshold)

print("Combinations with low sample size:")
print(low_sample_combinations)

# Visualize proportions with percentage labels
ggplot(class_distribution, aes(x = loan_range, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(
    aes(label = percent(proportion, accuracy = 0.1)),
    position = position_fill(vjust = 0.5),
    size = 3) +
  facet_wrap(~ installment_commitment_range, ncol = 2) +
  labs(
    title = "Distribution of Credit Risk Class Across \nLoan Duration and Installment Commitment Ranges",
    x = "Loan Duration (Months)", 
    y = "Proportion",
    fill = "Credit Risk Class") +
  scale_fill_manual(values = c("good" = "palegreen", "bad" = "orangered")) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold"))

#3. What are the underlying patterns and factors for loan durations and instalment commitments in credit class classification? 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(caret)

# Descriptive Statistics for Loan Duration and Installment Commitment by Credit Class
summary_by_class <- df %>%
  group_by(class) %>%
  summarise(
    duration_mean = mean(duration),
    duration_median = median(duration),
    duration_sd = sd(duration),
    installment_commitment_mean = mean(installment_commitment),
    installment_commitment_median = median(installment_commitment),
    installment_commitment_sd = sd(installment_commitment))

print(summary_by_class)

# Visualizations
# Density Plot of Loan Duration by Credit Class
ggplot(df, aes(x = duration, fill = class, color = class)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Loan Duration by Credit Class",
       x = "Loan Duration (months)",
       y = "Density") +
  theme_minimal()

# Density Plot of Installment Commitment by Credit Class
ggplot(df, aes(x = installment_commitment, fill = class, color = class)) +
  geom_density(alpha = 0.3) +
  labs(title = "Density Plot of Installment Commitment by Credit Class",
       x = "Installment Commitment",
       y = "Density") +
  theme_minimal()

# Statistical Tests
# T-test for loan duration by credit class
t_test_duration <- t.test(duration ~ class, data = df)
print(t_test_duration)

# T-test for installment commitment by credit class
t_test_installment <- t.test(installment_commitment ~ class, data = df)
print(t_test_installment)


# Correlation Analysis
# Correlation between loan duration and installment commitment by credit class
cor_good <- cor(df %>% filter(class == "good") %>% select(duration, installment_commitment))
cor_bad <- cor(df %>% filter(class == "bad") %>% select(duration, installment_commitment))

print(paste("Correlation for good class: ", cor_good))
print(paste("Correlation for bad class: ", cor_bad))

# Logistic Regression Model
df$class <- factor(df$class, levels = c("good", "bad"), labels = c(0, 1))

model <- glm(class ~ duration + installment_commitment, family = binomial, data = df)
summary(model)

# Predictions and Log-Loss Calculation
# Predict probabilities from the logistic regression model
df$predicted_prob <- predict(model, type = "response")

# Convert 'class' to numeric for log-loss calculation (0 for 'good' and 1 for 'bad')
df$class_numeric <- as.numeric(df$class)  # 'good' = 0, 'bad' = 1

# Calculate log-loss
log_loss <- -mean(df$class_numeric * log(df$predicted_prob) + (1 - df$class_numeric) * log(1 - df$predicted_prob))

# Print the log-loss value
print(paste("Log-Loss: ", log_loss))

# Calculate the proportion of 'good' and 'bad' classes
p_good <- mean(df$class_numeric == 0)  # Proportion of good cases
p_bad <- mean(df$class_numeric == 1)   # Proportion of bad cases

# Baseline probabilities
prob_good <- p_good
prob_bad <- p_bad

# Baseline log-loss calculation
baseline_log_loss <- -mean(df$class_numeric * log(prob_bad) + (1 - df$class_numeric) * log(prob_good))

# Print the baseline log-loss
print(paste("Baseline Log-Loss: ", baseline_log_loss))

#4. Can loan duration and installment commitment be used to predict a customer’s credit risk class?
library(ggplot2)
library(randomForest)
library(caret)
library(pdp)
library(pROC)

# Ensure the 'class' variable is a factor
df$class = as.factor(df$class)
df$class = factor(df$class, levels = c("good", "bad"), labels = c(0, 1))

# Set up cross-validation (10-fold)
train_control <- trainControl(method = "cv", number = 10)

# Perform training with cross-validation
set.seed(123)  # For reproducibility
rf_model_cv <- train(class ~ duration + installment_commitment, 
                     data = df, 
                     method = "rf", 
                     trControl = train_control, 
                     ntree = 500)

# Print cross-validation results
print(rf_model_cv)

# Extract cross-validation results 
print(rf_model_cv$results)

# Variable importance plot from the cross-validated model
importance(rf_model_cv$finalModel)
varImpPlot(rf_model_cv$finalModel)


# Confusion Matrix and Accuracy 
rf_predictions_cv <- predict(rf_model_cv, df)
confusionMatrix(rf_predictions_cv, df$class)

# AUC from the cross-validated model
rf_probabilities_cv <- predict(rf_model_cv, df, type = "prob")
# Use probabilities for the positive class
roc_curve_cv <- roc(df$class, rf_probabilities_cv[, 2])  
plot(roc_curve_cv, main = "ROC Curve for Random Forest (CV)")
auc(roc_curve_cv)

#5. What loan duration and installment commitment values are associated with a higher probability of being classified as bad credit risk, and what thresholds can be set to minimize the number of bad-risk customers?
library(ggplot2)
library(caret)
library(pROC)
library(PRROC)
library(xgboost)

# Ensure the 'class' variable is a factor with 0 for "good" and 1 for "bad"
df$class <- as.factor(df$class)
df$class <- factor(df$class, levels = c("good", "bad"), labels = c(0, 1))

# Prepare data for GBM
dtrain <- xgb.DMatrix(data = as.matrix(df[, c("duration", "installment_commitment")]), 
                      label = as.numeric(as.character(df$class)))

# Fit the GBM Model
gbm_model <- xgboost(data = dtrain, max_depth = 3, eta = 0.1, nrounds = 100, 
                     objective = "binary:logistic", verbose = 0)

# Predict probabilities using GBM
gbm_prob <- predict(gbm_model, as.matrix(df[, c("duration", "installment_commitment")]))

# Plot the Precision-Recall Curve for GBM
pr_curve_gbm <- pr.curve(scores.class0 = gbm_prob, 
                         weights.class0 = as.numeric(as.character(df$class)), 
                         curve = TRUE)

plot(pr_curve_gbm, main = "Precision-Recall Curve for GBM", 
     xlab = "Recall", ylab = "Precision")

# Calculate and print AUC for the Precision-Recall Curve
pr_auc_gbm <- pr_curve_gbm$auc.integral  # Use the integral version of AUC
cat("Precision-Recall AUC for GBM: ", pr_auc_gbm, "\n")

# Threshold Analysis
threshold_range <- seq(0, 1, by = 0.02)
bad_risk_counts <- sapply(threshold_range, function(thresh) {
  predicted_class <- ifelse(gbm_prob > thresh, 1, 0)  # Classify based on the threshold
  sum(predicted_class == 1)  # Count the number of "bad" predictions
})

# Plot the Threshold Analysis
plot(threshold_range, bad_risk_counts, type = "b", xlab = "Threshold", 
     ylab = "Number of Bad Class Customers", main = "Threshold vs Bad Class Customers (GBM)")

# Optimal Threshold
thresholds <- seq(0, 1, by = 0.02)
f1_scores <- sapply(thresholds, function(thresh) {
  predicted_class <- ifelse(gbm_prob > thresh, 1, 0)
  cm <- confusionMatrix(factor(predicted_class), df$class, positive = "1")
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1 <- 2 * ((precision * recall) / (precision + recall))
  return(f1)
})

optimal_threshold <- thresholds[which.max(f1_scores)]
cat("Optimal Threshold (Max F1): ", optimal_threshold, "\n")

# Apply the Optimal Threshold to Classify
df$predicted_class_gbm <- ifelse(gbm_prob > optimal_threshold, 1, 0)

# Confusion Matrix for GBM
conf_matrix <- confusionMatrix(as.factor(df$predicted_class_gbm), 
                               df$class, positive = "1")
print("Confusion Matrix for GBM:")
print(conf_matrix)

# Feature Importance for GBM
importance <- xgb.importance(feature_names = colnames(df[, c("duration", "installment_commitment")]), 
                             model = gbm_model)
xgb.plot.importance(importance_matrix = importance, main = "Feature Importance for GBM")

# Visualizing GBM Probabilities by Features
# Duration vs Probability
ggplot(data = df, aes(x = duration, y = gbm_prob)) +
  geom_point(aes(color = as.factor(class)), alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "GBM Probability of \nBad Credit Risk by Duration", 
       x = "Duration (months)", y = "Predicted Probability") +
  theme_minimal()

# Installment Commitment vs Probability
ggplot(data = df, aes(x = installment_commitment, y = gbm_prob)) +
  geom_point(aes(color = as.factor(class)), alpha = 0.6) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "GBM Probability of Bad Credit Risk by Installment Commitment", 
       x = "Installment Commitment", y = "Predicted Probability") +
  theme_minimal()
