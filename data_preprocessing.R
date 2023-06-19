setwd(dirname(rstudioapi::getSourceEditorContext()$path))
#a) read in the Table called '.txt' 
chick = read.csv("Chicken Fillet NIR data.csv", header=TRUE, sep=';')
df = chick
column_classes <- sapply(df, class)  # Get the class of each column
#column_classes 
# make boxplots for the numeric columns
boxplot(df[, 5:ncol(df)])
# summary for the columns
summary(df[, 5:ncol(df)])

# data pre-processing
library(tidyverse)
library(ggplot2)
library(caret)
# Calculate z-scores for each column except the first four
z_scores <- apply(df[, 5:ncol(df)], 2, function(x) scale(x))

# Calculate the percentage of outliers (absolute z-scores greater than 3)
outlier_percentage <- apply(z_scores, 2, function(x) {
  outliers <- abs(x) > 3
  sum(outliers) / length(x)
})
outlier_percentage

# Identify rows with any outliers
rows_with_outliers <- apply(z_scores, 1, function(x) any(abs(x) > 3))

# Remove rows with outliers
df_clean <- df[!rows_with_outliers, ]
# Remove missing values
df_clean <- df_clean %>% drop_na()
#---------------------------------------------------------------
## encode categorical column
df_clean$Freshness <- factor(df_clean$Freshness,levels = unique(df_clean$Freshness))

df_clean$Production_system <- factor(df_clean$Production_system,levels = unique(df_clean$Production_system))

# normalize the data
df_clean[,5:ncol(df_clean)] <- scale(df_clean[,5:ncol(df_clean)])
colnames(df_clean)
# Dimensionality reduction
# Perform PCA
pca <- prcomp(df_clean[,5:ncol(df_clean)], center = TRUE, scale. = TRUE)
# Explained variance
variance <- pca$sdev^2
variance_prop <- variance / sum(variance)
cumulative_prop <- cumsum(variance_prop)

# Scree plot
scree_data <- data.frame(Principal_Component = 1:length(variance_prop),
                         Variance_Explained = variance_prop,
                         Cumulative_Variance = cumulative_prop)
ggplot(scree_data, aes(x = Principal_Component, y = Variance_Explained)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Scree Plot", x = "Principal Component", y = "Variance Explained")

# Accessing results
summary(pca)  # Summary statistics
pca$rotation  # Principal component loadings
pca$x  # Transformed data (scores)
# we decided to keep the first 3 PCs. We add them to our data
df_clean <- cbind(df_clean, pca$x[,1:3])
# Get column names of PC1 and PC2
pc1_col <- colnames(pca$x)[1]
pc2_col <- colnames(pca$x)[2]
pc3_col <- colnames(pca$x)[3]
print(colnames(df_clean))