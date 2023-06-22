# install and load libraries
# install.packages("ggplot2")
# install.packages("Hmisc")
# install.packages("tidyverse")
# install.packages("rstudioapi")
# install.packages("xgboost")
# install.packages("randomForest")
# install.packages("VGAM")
# install.packages("class")
# install.packages("e1071")
# install.packages("caret")
# install.packages("gplots")
# install.packages("MLmetrics")
# install.packages('plsgenomics')
# install.packages("writexl")

libraries <- c("rstudioapi", "Hmisc", "ggplot2", "tidyverse", "randomForest", "xgboost", "VGAM", "class", "e1071", "plsgenomics", "caret", "readxl", "writexl")
lapply(libraries, library, character.only = TRUE)

set.seed(42)

# set wd and load data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
df_f <- read_excel("features_model_result.xlsx")

df_f$Features_Nr <- as.numeric(df_f$Features_Nr)
df_f$F1_Score <- as.numeric(df_f$F1_Score)

# The number of features that has the highest average F1 score

y_lst <- c("Production_system", "Freshness")
scan_type_lst <- c("OM", "TB", "TP")
features_l <- c()

for (y in y_lst){
    for (type in scan_type_lst) {
        df_x <- df_f %>%
            filter((Y_Feature == y) & (Scan_Type == type))
        best_param <- df_x$Features_Nr[which.max(df_x$F1_Score)]
        cat(paste("Y Feature:", y, "Scan Type:", type, "Best Parameter:", best_param, "\n"))
        features_l <- c(features_l, unname(best_param))
    }
}

data <- read.csv("data.csv", sep = ";")
data$Production_system <- trimws(data$Production_system)
data <- data %>% drop_na()

#-------------------------------------------------------------------------------
# Train the model with optimal features

testing <- function(scan_type, data_1, data_2, col_name, model_type, importance_1, importance_2, important_nr) {
    print("Start")
    df_a <- data.frame(matrix(, nrow = 0, ncol = 7))
    names(df_a) <- c("Scan_Type", "Y_Feature", "Features_Nr", "Model_Used", "Accuracy", "F1_Score", "Confusion_Matrix")
    acc <- 0
    f1 <- 0
    if (col_name == "Production_system") {
        a <- 8
        x_columns <- importance_2
    } else if (col_name == "Freshness") {
        a <- 2
        x_columns <- importance_1
    }
    class_labels <- c(1: a)
    conf_mat <- table(Actual = class_labels, Predicted = rev(class_labels))
    # flds <- createFolds(as.integer(rownames(data_1)), k = k, list = TRUE, returnTrain = FALSE)
    x_columns <- colnames(data_1)
    x_columns <- x_columns[!x_columns %in% c("Sample_number", "Scan_type", "Freshness", "Production_system")]

    # cross-validation
    # for (i in 1: k) {
    #     other_data <- data_1[-flds[[i]], ]
    #     val_data <- data_1[flds[[i]], ]
    #     new_x_columns = append(x_columns, col_name)
    #     other_data <- other_data[, colnames(other_data) %in% new_x_columns]
    #     val_data <- val_data[, colnames(val_data) %in% new_x_columns]
    #     if (col_name == "Production_system") {
    #         lr_model <- vglm(Production_system ~ ., data = other_data, family = multinomial)
    #     } else if (col_name == "Freshness") {
    #         lr_model <- vglm(Freshness ~ ., data = other_data, family = multinomial)
    #     }
    #     pred <- factor(as.integer(unname(max.col(predict(lr_model, val_data, type = "response")))), levels = 1: a)
    #     val_y <- factor(val_data[, col_name], levels = 1: a)
        
    #     result <- confusionMatrix(pred, val_y)
    #     acc <- acc + unname(result$overall["Accuracy"])
    #     if (col_name == "Production_system") {
    #         f1 <- f1 + mean(unname(result$byClass[, "F1"]), na.rm = TRUE)
    #     } else if (col_name == "Freshness") {
    #         f1 <- f1 + mean(result$byClass["F1"], na.rm = TRUE)
    #     }
    #     conf_mat <- conf_mat + result$table
    # }
    
    # testing
    new_x_columns = append(x_columns, col_name)
    data_1 <- data_1[, colnames(data_1) %in% new_x_columns]
    data_2 <- data_2[, colnames(data_2) %in% new_x_columns]
    if (col_name == "Production_system") {
        lr_model <- vglm(Production_system ~ ., data = data_1, family = multinomial)
    } else if (col_name == "Freshness") {
        lr_model <- vglm(Freshness ~ ., data = data_2, family = multinomial)
    }
    pred <- factor(as.integer(unname(max.col(predict(lr_model, data_2, type = "response")))), levels = 1: a)
    val_y <- factor(data_2[, col_name], levels = 1: a)
    
    final_result <- confusionMatrix(pred, val_y)
    final_acc <- unname(final_result$overall["Accuracy"])
    if (col_name == "Production_system") {
        final_f1 <- mean(unname(final_result$byClass[, "F1"]), na.rm = TRUE)
    } else if (col_name == "Freshness") {
        final_f1 <- mean(final_result$byClass["F1"], na.rm = TRUE)
    }
    final_conf_mat <- final_result$table  
    
    print(c(scan_type, col_name, model_type, important_nr))
    print("--------------------")
    print("Accuracy: ")
    print(final_acc)
    print("F1 Score: ")
    print(final_f1)
    print("Confusion Matrix: ")
    print(final_conf_mat)
    df_a[nrow(df_a) + 1,] = c(scan_type, col_name, important_nr, model_type, final_acc, final_f1, paste(as.vector(final_conf_mat), collapse = " "))
    return(df_a)
}

testing_training <- function(scan_type, important_nr) {
    print(scan_type)
    print(important_nr)
    df_b <- data.frame(matrix(, nrow = 0, ncol = 7))
    names(df_b) <- c("Scan_Type", "Y_Feature", "Features_Nr", "Model_Used", "Accuracy", "F1_Score", "Confusion_Matrix")
    # extract numerical data
    on_meat <- data[data$Scan_type == scan_type, ]



    # convert to factor Change column name to production_system or Freshness
    on_meat$Production_system <- as.numeric(factor(on_meat$Production_system))
    on_meat$Freshness <- as.numeric(factor(on_meat$Freshness))

    on_meat <- on_meat[complete.cases(on_meat$Production_system, on_meat$Freshness), ]
    
    # split the data
    data_splits <- sample(c(TRUE, FALSE), nrow(on_meat), replace = TRUE, prob = c(0.7, 0.3))
    train_data <- on_meat[data_splits, ]
    test_data <- on_meat[!data_splits, ]

    # remove the outliers in training dataset (any absolute standardized values larger than 3)
    columns <- colnames(train_data)
    columns <- columns[!columns %in% c("Sample_number", "Scan_type", "Freshness", "Production_system")]
    train_data_org_subset <- train_data[, columns]
    train_data_org_subset <- scale(train_data_org_subset)
    idx <- which(rowSums(abs(train_data_org_subset) > 3) == 0)
    print("Percentage of outliers in training dataset: ")
    print((dim(train_data)[1] - length(idx)) / dim(train_data)[1] * 100)
    train_data <- train_data[idx, ]

    # standardize the data
    train_data_new_subset <- train_data[, columns]
    train_data_new_subset <- scale(train_data_new_subset)
    train_data[, columns] <- train_data_new_subset

    # apply the mean and sd from the training dataset to standardize the testing dataset
    train_mean <- apply(train_data_new_subset, 2, mean)
    train_sd <- apply(train_data_new_subset, 2, sd)
    test_data_subset <- test_data[, columns]
    test_data_subset <- as.data.frame(scale(test_data_subset, center = train_mean, scale = train_sd))
    test_data[, columns] <- test_data_subset

    # find the important features using xgboost
    temp_train_x <- as.matrix(train_data[colnames(train_data) %in% columns])
    temp_train_y1 <- as.matrix(train_data["Freshness"])
    temp_train_y2 <- as.matrix(train_data["Production_system"]) 
    train_matrix_y1 <- xgb.DMatrix(data = temp_train_x, label = temp_train_y1)
    train_matrix_y2 <- xgb.DMatrix(data = temp_train_x, label = temp_train_y2)    
    xgboost_model_1 <- xgb.train(data = train_matrix_y1, nrounds = 10, objective = "multi:softmax", num_class = 2 + 1)
    xgboost_model_2 <- xgb.train(data = train_matrix_y2, nrounds = 10, objective = "multi:softmax", num_class = 8 + 1)
    importance_1 <- xgb.importance(model = xgboost_model_1)
    importance_2 <- xgb.importance(model = xgboost_model_2)

    importance_1 <- importance_1[order(importance_1$Gain, decreasing = TRUE), ]
    importance_2 <- importance_2[order(importance_2$Gain, decreasing = TRUE), ]

    importance_1 <- c(importance_1$Feature[1: important_nr])
    importance_2 <- c(importance_2$Feature[1: important_nr])

    y_lst <- c("Production_system", "Freshness")

    for (y in y_lst) {
        df_c <- testing(scan_type, train_data, test_data, y, "logistic_regression", importance_1, importance_2, important_nr)
        df_b <- rbind(df_b, df_c)
    }
    print("------------------------------------------------------------")
    return(df_b)
}

scan_type_lst <- c("OM", "TB", "TP")

# Create empty dataframe
df_result <- data.frame(matrix(, nrow = 0, ncol = 7))
names(df_result) <- c("Scan_Type", "Y_Feature", "Features_Nr", "Model_Used", "Accuracy", "F1_Score", "Confusion_Matrix")

full_scan_type_lst <- c("OM", "TB", "TP", "OM", "TB", "TP")
for (nr in 1: 6) {
    df_result <- rbind(df_result, testing_training(full_scan_type_lst[nr], features_l[nr]))
}

write_xlsx(df_result, "testing_model_result.xlsx")

