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