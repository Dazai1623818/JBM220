install.packages("ggplot2")
install.packages("Hmisc")
libraries <- c("rstudioapi", "Hmisc", "ggplot2")
lapply(libraries, library, character.only = TRUE)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
data <- read.csv("data.csv", sep = ";")

numerical <- data[, (5):ncol(data)]
summary(numerical)
windows();pairs(numerical)