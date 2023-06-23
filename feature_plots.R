libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "xgboost", "VGAM", "class", "e1071", "plsgenomics", "caret", "readxl", "writexl")
lapply(libraries, library, character.only = TRUE)

set.seed(42)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
df <- read_excel("features_model_result.xlsx")

df$Features_Nr <- as.numeric(df$Features_Nr)
df$F1_Score <- as.numeric(df$F1_Score)

result_a <- df %>%
  filter(Y_Feature == 'Freshness')

result_b <- df %>%
  filter(Y_Feature == 'Production_system')

result_a$Scan_Type <- as.factor(result_a$Scan_Type)
plot(result_a$Features_Nr, result_a$F1_Score, type = 'l', col = result_a$Scan_Type)

ggplot(data = result_a, aes(Features_Nr, F1_Score, group = Scan_Type, color = Scan_Type)) +
  geom_path(size = 1.2) +
  labs(x = "Number of Selected Features", y = "F1 Score", color = "Scan Type") +
  ggtitle("Freshness: F1 Score by Number of Selected Features") +
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 14))

ggplot(data = result_b, aes(Features_Nr, F1_Score, group = Scan_Type, color = Scan_Type)) +
  geom_path(size = 1.2) +
  labs(x = "Number of Selected Features", y = "F1 Score", color = "Scan Type") +
  ggtitle("Production system: F1 Score by Number of Selected Features") +
  theme(axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.title = element_text(hjust = 0.5, size = 14))

  


