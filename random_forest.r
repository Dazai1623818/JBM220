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


set.seed(42)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")

# remove trailing whitespace in rows
data$Production_system <- trimws(data$Production_system)
# extract numerical data
numerical <- data[, (5):ncol(data)]
summary(numerical)
mean(cor(numerical))

# Apply PCA
results <- prcomp(numerical, scale = FALSE)
# we can see that 99% is explained by first 2 components
summary(results)
plot(results, type="l")

# we can roughly see three different classes
windows();biplot(results, scale = 0, xlim = c(-4, 4), ylim = c(-4, 4), cex = 0.25)

# we further investigate and color them by variables
vis = cbind(data, results$x[, 1:2])

windows()
ggplot(vis, aes(PC1, PC2, col = Scan_type, fill = Scan_type)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))

windows()
ggplot(vis, aes(PC1, PC2, col = Freshness, fill = Freshness)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))

windows()
ggplot(vis, aes(PC1, PC2, col = Production_system, fill = Production_system)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))












on_meat = data[data$Scan_type == "OM", ]
numerical <- on_meat[, (5):ncol(on_meat)]
results <- prcomp(numerical, scale = FALSE)
summary(results)
on_meat = cbind(on_meat, results$x[, 1:2])

windows()
ggplot(on_meat, aes(PC1, PC2, col = Freshness, fill = Freshness)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))
str(on_meat)

on_meat$Production_system <- as.factor(on_meat$Production_system)  # Convert to factor



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

data_splits <- create_train_test_val(on_meat, "Production_system", 0.7, 0.5)

# Access the train, test, and validation datasets
train_data <- data_splits$train
test_data <- data_splits$test
val_data <- data_splits$val

# Train the random forest model on the training set
model <- randomForest(Production_system ~ ., data = train_data, ntree = 300)

# Make predictions on the test set
predictions <- predict(model, newdata = val_data)

# Create the confusion matrix
confusion <- confusionMatrix(predictions, val_data$Production_system)

confusion_mat <- as.matrix(confusion$table)

# Get the levels of the class labels
class_labels <- levels(val_data$Production_system)

# Reorder the rows and columns of the confusion matrix based on the class label order
confusion_mat_reordered <- confusion_mat[match(class_labels, rownames(confusion_mat)), match(class_labels, colnames(confusion_mat))]

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
levels(on_meat$Production_system)
