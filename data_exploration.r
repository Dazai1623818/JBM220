# install and load libraries
install.packages("ggplot2")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("rstudioapi")
libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse")
lapply(libraries, library, character.only = TRUE)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")

# select variables and plot corr
# numerical <- data[, (125):ncol(data)]
numerical <- data[, (5):ncol(data)]
summary(numerical)
windows();pairs(numerical)

#calculate principal components https://www.statology.org/principal-components-analysis-in-r/
results <- prcomp(numerical, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation
results$x <- -1*results$x
biplot(results, scale = 0)
#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
windows();qplot(c(1:125), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
