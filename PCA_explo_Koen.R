# install and load libraries
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("tidyverse")
# install.packages("rstudioapi")
# install.packages("xgboost")
# install.packages("caret")
# install.packages("gplots")
# install.packages("MLmetrics")
# install.packages("factoextra")
install.packages("factoextra")
install.packages("gplots")

libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "caret", "gplots", "dplyr", "factoextra")
lapply(libraries, library, character.only = TRUE)


set.seed(1234)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")
data$Production_system <- trimws(data$Production_system)
numerical_data <- data[, (5):ncol(data)]

# PCA
results <- prcomp(numerical_data, scale = FALSE)
plot(results, type="l")
windows();biplot(results, scale = 0, xlim = c(-4, 4), ylim = c(-4, 4), cex = 0.25)
vis = cbind(data, results$x[, 1:2])

# Freshness
windows()
ggplot(vis, aes(PC1, PC2, col = Freshness, fill = Freshness)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))

# Scan_type
windows()
ggplot(vis, aes(PC1, PC2, col = Scan_type, fill = Scan_type)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))


# Contribution of variables to PC1 and PC2
contrib <- fviz_contrib(results, choice = "var", axes = 1, top = 10)
print(contrib)

contrib <- fviz_contrib(results, choice = "var", axes = 2, top = 10)
print(contrib)

windows()
# Cumulative explained variance plot
fviz_eig(results, addlabels = TRUE, ylim = c(0, 50))

windows()
# Biplot with variable vectors
fviz_pca_biplot(results, repel = TRUE)

windows()
# Production_system
ggplot(vis, aes(PC1, PC2, col = Production_system, fill = Production_system)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))

# You can replace 'Production_system' with any other categorical variable in your dataset to visualize its effect on the PCA plot.

