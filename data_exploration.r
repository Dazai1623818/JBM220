# install and load libraries
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("rstudioapi")
# install.packages("randomForest")
# install.packages("caret")
# install.packages("gplots")
libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "caret", "gplots", "dplyr")
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



# Rotate the data
rotation_angle <- 90  # Specify the rotation angle in degrees
rotation_matrix <- matrix(c(cos(rotation_angle), -sin(rotation_angle),
                            sin(rotation_angle), cos(rotation_angle)), ncol = 2)

# Apply rotation to the relevant variables
vis_rotated <- vis %>%
  select(PC1, PC2) %>%
  as.matrix() %>%
  `%*%`(rotation_matrix) %>%
  as.data.frame() %>%
  setNames(c("Rotated_PC1", "Rotated_PC2"))

# Combine the rotated variables with the original data
vis <- cbind(vis, vis_rotated)

# Plot the rotated data
windows()
ggplot(vis, aes(Rotated_PC1, Rotated_PC2, col = Freshness, fill = Freshness)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))


# Density Plot
windows()
ggplot(vis, aes(PC1, PC2, col=Freshness, fill = Freshness)) +
  geom_density_2d(alpha = 0.5) +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  scale_fill_manual(values = c("thawed" = "blue", "fresh" = "green"))

# Contour Plot
windows()
ggplot(vis, aes(PC1, PC2, col=Freshness, fill = Freshness)) +
  stat_density_2d(aes(alpha = ..level..), geom = "polygon") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  scale_fill_manual(values = c("thawed" = "blue", "fresh" = "green"))


# Violin Plot
windows()
ggplot(vis, aes(Freshness, Rotated_PC1)) +
  geom_violin() +
  ylim(-4, 4) +
  scale_fill_manual(values = c("thawed" = "blue", "fresh" = "green"))



windows()
ggplot(vis, aes(PC1, PC2, col = Freshness, fill = Freshness)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4)) +
  coord_fixed(ratio = 1)  
  coord_flip()

windows()
ggplot(vis, aes(PC1, PC2, col = Production_system, fill = Production_system)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black") +
  coord_cartesian(xlim = c(-4, 4), ylim = c(-4, 4))

# Contour Plot
windows()
ggplot(vis, aes(PC1, PC2, col=Production_system, fill = Production_system)) +
  geom_density_2d(alpha = 0.5) +
  scale_fill_manual(values = c("1 Star" = "blue", "2 Stars" = "green", "CF" = "red", "CONV" = "purple",
                               "FR" = "orange", "MAR" = "yellow", "ORG" = "brown", "STD" = "pink"))
