# install and load libraries
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("rstudioapi")
install.packages("randomForest")
install.packages("caret")
install.packages("gplots")
libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "caret", "gplots")
lapply(libraries, library, character.only = TRUE)


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

set.seed(42)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")

# remove trailing whitespace in rows
data$Production_system <- trimws(data$Production_system)

# extract numerical data
numerical <- data[, (5):ncol(data)]
on_meat = data[data$Scan_type == "OM", ]
numerical <- on_meat[, (5):ncol(on_meat)]


# Convert to factor
on_meat$Production_system <- as.factor(on_meat$Production_system)
 
# split the data
data_splits <- create_train_test_val(on_meat, "Production_system", 0.7, 0.5)
# train_data <- data_splits$train
test_data <- data_splits$test
val_data <- data_splits$val

# Train and predict
model <- randomForest(Production_system ~ ., data = train_data, ntree = 300)
predictions <- predict(model, newdata = val_data)

# Create the confusion matrix
confusion <- confusionMatrix(predictions, val_data$Production_system)
normalized_confusion_mat <- t(t(confusion_mat) / colSums(confusion_mat))

# Create a heatmap of the normalized confusion matrix
windows()
heatmap.2(normalized_confusion_mat,
          col = colorRampPalette(c("white", "steelblue"))(100),
          main = "Normalized Confusion Matrix Heatmap",
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
round(confusion$overall, digits = 2)
