# install and load libraries
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("randomForest")
install.packages("caret")
install.packages("gplots")
install.packages("mltools")
install.packages('plsgenomics')
libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "caret", "gplots", "mltools", "plsgenomics")
lapply(libraries, library, character.only = TRUE)

a = 1
b = 2
c = 3
print(c(a, b, c))

# Define functions
create_train_test_val <- function(data, target_variable, train_prop, test_prop) {
  # Create the train dataset
  train_idx <- createDataPartition(data[[target_variable]], p = train_prop, list = FALSE)
  train_data <- data[train_idx, ]
  
  # Create the remaining dataset
  remaining_data <- data[-train_idx, ]
  
  # Create the test dataset
  test_idx <- createDataPartition(remaining_data[[target_variable]], p = test_prop, list = FALSE)
  test_data <- remaining_data[test_idx, ]
  
  # Create the validation dataset
  val_data <- remaining_data[-test_idx, ]
  
  # Return the train, test, and validation datasets
  return(list(train = train_data, test = test_data, val = val_data))
}


train_model <- function(data, scan_type, model_type) {
  cat_cols <- c(-1, -3, -4)
  
  # Filter data based on scan type
  df <- data[data$Scan_type == scan_type, cat_cols]
  
  # Convert to factor and scale the data
  df$Production_system <- as.factor(df$Production_system)
  df[, -1] <- scale(df[, -1])
  
  # Split the data
  data_splits <- create_train_test_val(df, "Production_system", 0.7, 0.5)
  train_data <- data_splits$train
  val_data <- data_splits$val
  
  # Fit the model
  if (model_type == "plsda") {
    model <- pls.lda(train_data[, -1], train_data[, 1], Xtest = val_data[, -1], ncomp = 1:15, nruncv = 100)
    confusion <- confusionMatrix(model$predclass, val_data[, 1])
  } else if (model_type == "randomforest") {
    model <- randomForest(Production_system ~ ., data = train_data, ntree = 300)
    predictions <- predict(model, newdata = val_data)
    confusion <- confusionMatrix(predictions, val_data$Production_system)
  } else {
    stop("Invalid model specified.")
  }
  
  # Normalize the confusion matrix
  normalized_confusion_mat <- confusion$table / colSums(confusion$table)
  
  # Plot the heatmap
  windows();heatmap.2(normalized_confusion_mat,
            col = colorRampPalette(c("white", "steelblue"))(100),
            main = sprintf("Normalized Confusion Matrix Heatmap %s", scan_type),
            xlab = "Predicted Class",
            ylab = "True Class",
            trace = "none",
            dendrogram = "none",
            Rowv = FALSE,
            Colv = FALSE,
            cellnote = round(normalized_confusion_mat, 2),
            notecol = "black",
            fontcol = "white",
            cexCol = 1,
            cexRow = 1)
  
  # Add caption to the plot
  caption <- sprintf("Model: %s\nF1 Score: %.2f", model_type, mean(confusion$byClass[,"F1"]))
  mtext(caption, side = 1, line = 3, adj = 0, cex = 0.8, font = 2)
  
  # Return the model performance
  return(round(confusion$overall, digits = 2))
}


set.seed(42)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")
# remove trailing whitespace in rows
data$Production_system <- trimws(data$Production_system)


# Call the function for each scan type
train_model(data, "OM", "plsda")  # For OM scan type
train_model(data, "TB", "plsda")  # For TB scan type
train_model(data, "TP", "plsda")  # For TP scan type


