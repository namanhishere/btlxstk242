library(randomForest)
library(e1071)
library(pROC)
library(caret)
library(ggplot2)
library(dplyr)
# --------------------------------------------------------------------

# --- 1. Build Logistic Regression model ---
logistic_model <- glm(Patient_Status ~ ., data = train_set, family = "binomial")


# --- 2. Build Random Forest model ---
train_set$Patient_Status <- as.factor(train_set$Patient_Status)
test_set$Patient_Status <- as.factor(test_set$Patient_Status)

set.seed(456) # For reproducible results
rf_model <- randomForest(Patient_Status ~ ., data = train_set, ntree = 500, importance = TRUE)
print(rf_model)

# --- 3. Build Naive Bayes model ---
nb_model <- naiveBayes(Patient_Status ~ ., data = train_set)
print(nb_model)

# --- 4. Predict on the test_set ---
cat("\nPerforming predictions on the test_set...\n")

# Predict probabilities for the ROC curve
pred_prob_lr <- predict(logistic_model, newdata = test_set, type = "response")
positive_class_label <- levels(test_set$Patient_Status)[2]
pred_prob_rf <- predict(rf_model, newdata = test_set, type = "prob")[, positive_class_label]
pred_prob_nb <- predict(nb_model, newdata = test_set, type = "raw")[, positive_class_label]

# Predict classes for the confusion matrix
# Use a 0.5 threshold for Logistic Regression
positive_level <- levels(test_set$Patient_Status)[2] # Assuming the second level is the positive class
negative_level <- levels(test_set$Patient_Status)[1]
pred_class_lr <- factor(ifelse(pred_prob_lr > 0.5, positive_level, negative_level), levels = levels(test_set$Patient_Status))
pred_class_rf <- predict(rf_model, newdata = test_set, type = "class")
pred_class_nb <- predict(nb_model, newdata = test_set, type = "class")

# --- 5. Evaluate models ---
cat("\nEvaluating models...\n")

# Get actual labels from the test_set
actual_status <- test_set$Patient_Status

# Create Confusion Matrix and calculate metrics
cm_lr <- confusionMatrix(data = pred_class_lr, reference = actual_status, positive = positive_level)
cm_rf <- confusionMatrix(data = pred_class_rf, reference = actual_status, positive = positive_level)
cm_nb <- confusionMatrix(data = pred_class_nb, reference = actual_status, positive = positive_level)

print("Confusion Matrix & Statistics - Logistic Regression:")
print(cm_lr)
print("Confusion Matrix & Statistics - Random Forest:")
print(cm_rf)
print("Confusion Matrix & Statistics - Naive Bayes:")
print(cm_nb)

# --- 6. Compare using plots ---

# 6.1. ROC Curves plot
roc_lr <- roc(response = actual_status, predictor = pred_prob_lr, levels = levels(actual_status))
roc_rf <- roc(response = actual_status, predictor = pred_prob_rf, levels = levels(actual_status))
roc_nb <- roc(response = actual_status, predictor = pred_prob_nb, levels = levels(actual_status))


# Get AUC values
auc_lr <- auc(roc_lr)
auc_rf <- auc(roc_rf)
auc_nb <- auc(roc_nb)

# Plot ROC curves
plot(roc_lr, col = "blue", main = "ROC Curves Comparison", legacy.axes = TRUE)
lines(roc_rf, col = "red")
lines(roc_nb, col = "darkgreen")
legend("bottomright",
       legend = c(paste("Logistic Regression (AUC =", round(auc_lr, 3), ")"),
                  paste("Random Forest (AUC =", round(auc_rf, 3), ")"),
                  paste("Naive Bayes (AUC =", round(auc_nb, 3), ")")),
       col = c("blue", "red", "darkgreen"),
       lty = 1, # Line type
       cex = 0.8) # Text size

# 6.2. Bar chart comparing metrics (e.g., Accuracy, Sensitivity, Specificity, AUC)
metrics_data <- data.frame(
  Model = c("Logistic Regression", "Random Forest", "Naive Bayes"),
  Accuracy = c(cm_lr$overall['Accuracy'], cm_rf$overall['Accuracy'], cm_nb$overall['Accuracy']),
  Sensitivity = c(cm_lr$byClass['Sensitivity'], cm_rf$byClass['Sensitivity'], cm_nb$byClass['Sensitivity']),
  Specificity = c(cm_lr$byClass['Specificity'], cm_rf$byClass['Specificity'], cm_nb$byClass['Specificity']),
  AUC = c(auc_lr, auc_rf, auc_nb) # Add AUC to the data frame
  # F1_Score = c(cm_lr$byClass['F1'], cm_rf$byClass['F1'], cm_nb$byClass['F1']) # Add F1 if needed
)

# Convert data to long format for easier plotting with ggplot2
metrics_long <- tidyr::pivot_longer(metrics_data, cols = -Model, names_to = "Metric", values_to = "Value")

# Plot bar chart
ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = round(Value, 3)), vjust = -0.3, position = position_dodge(width = 0.9), size = 3) +
  facet_wrap(~Metric, scales = "free_y") + # Display each metric in a separate panel
  labs(title = "Compare Performance Metrics Between Models",
       x = "Model",
       y = "Value") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1),
        legend.position = "none") # Hide legend as facet_wrap provides context

