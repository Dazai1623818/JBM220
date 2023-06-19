## Run data_preprocessing file first

# Split data into training and testing sets
set.seed(42)
scan_types <- c("OM", "TP", "TB")

#----------------------------------------------------------------------------
## Logistic regression

## Freshness classification
for (scan_type in scan_types) {
  df_subset <- df_clean[df_clean$Scan_type == scan_type, ]
  # Split the data into training and testing sets
  data_splits <- sample(c(TRUE, FALSE), nrow(df_subset), replace = TRUE, prob = c(0.7, 0.3))
  train_data <- df_subset[data_splits, ]
  test_data <- df_subset[!data_splits, ]
  
  # Create the training control object for cross-validation
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Train the logistic regression model using cross-validation on the training set
  model <- train(Freshness ~ PC1 + PC2 + PC3, data = train_data[,-c(1:3)], method = "glm",
                 family = binomial(link = "logit"), trControl = ctrl)
  
  
  # Compute predicted probabilities on the testing set using the cross-validated model
  prob <- predict(model, newdata = test_data[,-c(1:3)], type = "prob")
  pred_labels <- colnames(prob)[apply(prob, 1, which.max)]
  factor(pred_labels)
  cf = confusionMatrix(factor(pred_labels), factor(test_data$Freshness))
  print(paste("Scan Type:", scan_type))
  print(cf)
}


## Production system classification

for (scan_type in scan_types) {
  df_subset <- df_clean[df_clean$Scan_type == scan_type, ]
  
  # Split the data into training and testing sets
  data_splits <- sample(c(TRUE, FALSE), nrow(df_subset), replace = TRUE, prob = c(0.7, 0.3))
  train_data <- df_subset[data_splits, ]
  test_data <- df_subset[!data_splits, ]
  
  # Define the training control object for cross-validation
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Fit a multiclass logistic regression model using cross-validation
  model <- train(
    Production_system ~ PC1 + PC2 + PC3, 
    data = train_data[,-c(1,3,4)],
    method = "multinom",
    trControl = ctrl
  )
  
  # Make predictions on test data
  new_data <- test_data[, -c(1, 3, 4)]  # Remove the unwanted columns
  pred_probs <- predict(model, newdata = new_data, type = "prob")
  
  # Convert predicted probabilities to class labels
  pred_labels <- colnames(pred_probs)[apply(pred_probs, 1, which.max)]
  
  # Compute confusion matrix
  cf = confusionMatrix(factor(pred_labels,levels = unique(df_clean$Production_system)), 
                  factor(test_data$Production_system,levels = unique(df_clean$Production_system)))
  # Print scan type and confusion matrix
  print(paste("Scan Type:", scan_type))
  print(cf)
}


# -------------------------------------------------------------------------------
## KNN 
## Freshness classification
library(class)

scan_types <- c("OM", "TP", "TB")

for (scan_type in scan_types) {
  df_subset <- df_clean[df_clean$Scan_type == scan_type, ]
  
  # Split the data into training and testing sets
  data_splits <- sample(c(TRUE, FALSE), nrow(df_subset), replace = TRUE, prob = c(0.7, 0.3))
  train_data <- df_subset[data_splits, ]
  test_data <- df_subset[!data_splits, ]
  
  # Specify the predictor variables and the response variable
  predictors <- train_data[, -c(1:4)]
  response <- train_data$Freshness
 
  # Create the training control object for cross-validation
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Perform kNN on the training set using cross-validation
  k <- 2 
  knn_model <- train(x = predictors, y = response, method = "knn", trControl = ctrl, tuneGrid = data.frame(k = k))
  
  
  # Make predictions on the testing set
  knn_pred <- predict(knn_model, newdata = test_data[, -c(1:3)])
  
  # Compute confusion matrix
  cf <- confusionMatrix(factor(knn_pred), factor(test_data$Freshness))
  
  # Print scan type and confusion matrix
  print(paste("Scan Type:", scan_type))
  print(cf)
}

## Production system classification

for (scan_type in scan_types) {
  df_subset <- df_clean[df_clean$Scan_type == scan_type, ]
  
  # Split the data into training and testing sets
  data_splits <- sample(c(TRUE, FALSE), nrow(df_subset), replace = TRUE, prob = c(0.7, 0.3))
  train_data <- df_subset[data_splits, ]
  test_data <- df_subset[!data_splits, ]
  
  # Specify the predictor variables and the response variable
  train_data <- train_data %>% drop_na()
  predictors <- train_data[, -c(1:4)]
  response <- train_data[,2]
  nrow(response)
  # Specify the k value
  k <- 9
  
  # Create the training control object for cross-validation
  ctrl <- trainControl(method = "cv", number = 10)
  
  # Specify the k values to evaluate
  k_values <- 9
  # Perform k-NN with cross-validation
  knn_model <- train(predictors, response, method = "knn", trControl = ctrl, tuneGrid = data.frame(k = k_values),
                     metric = "Accuracy")
  
  # Make predictions on the testing set
  knn_pred <- predict(knn_model, newdata = test_data[, -c(1,3,4)])
  
  # Compute confusion matrix
  cf <- confusionMatrix(factor(knn_pred), factor(test_data$Production_system))
  
  # Print scan type and confusion matrix
  print(paste("Scan Type:", scan_type))
  print(cf)
}












