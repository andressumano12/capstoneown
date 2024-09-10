
#This code allows me to load my data frame and start inspecting the structure,
#dimensions and characteristics of the data.  

options(warn=-1)
library(tidyverse)
library(ggplot2)
data <- read.csv("winequality-red.csv", sep = ";") 
wine <- data
head(data) 
tail(data) 
dim(data) 
str(data) 
summary(data)
if (!require(caret)) install.packages("caret", dependencies = TRUE)

library(caret)

#This code allows me to visually inspect the data, specially through histograms 
#and boxplots.

install.packages("patchwork")
histogram_boxplot <- function(data, feature, bins = 30) {
  
  boxplot <- ggplot(data, aes_string(x = feature)) +
    geom_boxplot(fill = "violet", color = "black", outlier.colour = "red") +
    labs(title = paste("Boxplot of", feature), x = feature, y = "Values") +
    theme_minimal()
  
  
  histogram <- ggplot(data, aes_string(x = feature)) +
    geom_histogram(aes(y = ..density..), fill = "violet", bins = bins, alpha = 0.7) +
    geom_vline(aes(xintercept = mean(get(feature))), color = "green", linetype = "dashed") +
    geom_vline(aes(xintercept = median(get(feature))), color = "black", linetype = "solid") +
    labs(title = paste("Histogram of", feature), x = feature, y = "Density") +
    theme_minimal()

  
  library(patchwork)
  combined_plot <- boxplot / histogram
  
  
  print(combined_plot)
}


histogram_boxplot(data, 'fixed.acidity')
histogram_boxplot(data, 'volatile.acidity')
histogram_boxplot(data, 'citric.acid')
histogram_boxplot(data, 'residual.sugar')
histogram_boxplot(data, 'chlorides')
histogram_boxplot(data, 'free.sulfur.dioxide')
histogram_boxplot(data, 'total.sulfur.dioxide')



data$`total.sulfur.dioxide` <- pmin(data$`total.sulfur.dioxide`, 165)
histogram_boxplot(data, 'total.sulfur.dioxide')

labeled_barplot <- function(data, feature, perc = FALSE, n = NULL) {
  if (perc) {
    data %>%
      count(!!sym(feature)) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ggplot(aes_string(x = feature, y = ifelse(perc, "percentage", "n"))) +
      geom_bar(stat = "identity", fill = "violet") +
      geom_text(aes(label = sprintf("%.1f%%", percentage)), vjust = -0.5) +
      theme_minimal() +
      labs(x = feature, y = ifelse(perc, "Percentage", "Count"))
  } else {
    data %>%
      count(!!sym(feature)) %>%
      ggplot(aes_string(x = feature, y = "n")) +
      geom_bar(stat = "identity", fill = "violet") +
      geom_text(aes(label = n), vjust = -0.5) +
      theme_minimal() +
      labs(x = feature, y = "Count")
  }
}

labeled_barplot(data, "quality", perc = TRUE)

#this code creates a variable called quality_class which will help me
#in the machine learning models.

range(data$quality)


bins <- c(min(data$quality), 6, max(data$quality) + 1) 
labels <- c('non-premium', 'premium')


data$quality_class <- cut(data$quality, breaks = bins, labels = labels, right = FALSE)


sum(is.na(data$quality_class))


table(data$quality_class)



install.packages(c("ggplot2", "dplyr", "corrplot", "GGally"))

# Load libraries
library(ggplot2)
library(dplyr)
library(corrplot)
library(GGally)

#this code allows me to develop correlation heat maps and pairs plots
numeric_data <- data %>%
  select_if(is.numeric)


cor_matrix <- cor(numeric_data, use = "complete.obs")


corrplot(cor_matrix, method = "color", col = colorRampPalette(c("red", "white", "blue"))(200), 
         addCoef.col = "black", tl.col = "black", tl.srt = 45, 
         title = "Correlation Matrix Heatmap")


ggpairs(data, columns = 1:11, aes(color = quality_class)) + 
  theme_minimal() + 
  labs(title = "Pairplot of Wine Quality Data")

#now i am going to run some boxplots to visually analyze the relation between 
#quality class and the other variables.

cols <- c('fixed.acidity', 'volatile.acidity', 'citric.acid')
par(mfrow = c(1, 3))
for (variable in cols) {
  print(ggplot(data, aes_string(x = 'quality_class', y = variable)) +
          geom_boxplot(aes(fill = quality_class), palette = "PuBu") +
          labs(title = variable, x = "Quality Class", y = variable) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}


cols2 <- c('free.sulfur.dioxide', 'total.sulfur.dioxide', 'sulphates')
par(mfrow = c(1, 3))
for (variable in cols2) {
  print(ggplot(data, aes_string(x = 'quality_class', y = variable)) +
          geom_boxplot(aes(fill = quality_class), palette = "PuBu") +
          labs(title = variable, x = "Quality Class", y = variable) +
          theme_minimal() +
          theme(plot.title = element_text(hjust = 0.5)))
}


boxplot_quality <- function(variable) {
  print(ggplot(data, aes_string(x = 'quality_class', y = variable)) +
          geom_boxplot(aes(fill = quality_class), palette = "PuBu") +
          labs(title = paste("Boxplot of", variable, "by Quality Class"), x = "Quality Class", y = variable) +
          theme_minimal())
}


boxplot_quality('chlorides')
boxplot_quality('density')
boxplot_quality('pH')
boxplot_quality('residual.sugar')
boxplot_quality('alcohol')


#The following code develops a decision tree model to predict wine quality 
#using the independent variables.

if (!require(rpart)) install.packages("rpart", dependencies = TRUE)
if (!require(caret)) install.packages("caret", dependencies = TRUE)

library(rpart)
library(caret)


set.seed(1)  
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]


d_tree <- rpart(as.factor(y_train) ~ ., data = X_train, method = "class")


print(d_tree)


predictions <- predict(d_tree, X_test, type = "class")


conf_matrix <- confusionMatrix(predictions, factor(y_test))


print(conf_matrix)


plot_confusion_matrix <- function(predictions, actual) {
  cm <- table(Prediction = predictions, Actual = actual)
  
 
  cm_perc <- prop.table(cm) * 100
  
 
  labels <- paste0(cm, "\n", round(cm_perc, 2), "%")
  
  
  cm_df <- as.data.frame(cm)
  cm_df$Perc <- as.vector(cm_perc)
  cm_df$Label <- as.vector(labels)
  
  
  ggplot(data = cm_df, aes(x = Prediction, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Confusion Matrix", x = "Predicted Label", y = "True Label") +
    theme_minimal()
}


plot_confusion_matrix(predictions, factor(y_test))


#This code uses hyperparameters tuning to better tune the decision tree model

str(X_train)
str(y_train)


set.seed(1)  
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]  
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]


train_data <- data.frame(X_train, y_train = as.factor(y_train))  


train_control <- trainControl(method = "cv",  
                              number = 10,    
                              verboseIter = TRUE)


tune_grid <- expand.grid(
  cp = seq(0.001, 0.05, by = 0.005)  
)


set.seed(1)
tuned_model <- train(
  y_train ~ .,  
  data = train_data, 
  method = "rpart",
  trControl = train_control,
  tuneGrid = tune_grid,
  control = rpart.control(maxdepth = 20, minsplit = 20) 
)


print(tuned_model$bestTune)


plot(tuned_model)


best_predictions <- predict(tuned_model, newdata = X_test)


conf_matrix_tuned <- confusionMatrix(best_predictions, factor(y_test))


print(conf_matrix_tuned)


plot_confusion_matrix <- function(predictions, actual) {
  cm <- table(Prediction = predictions, Actual = actual)
  
  
  cm_perc <- prop.table(cm) * 100
  
  
  labels <- paste0(cm, "\n", round(cm_perc, 2), "%")
  
  
  cm_df <- as.data.frame(cm)
  cm_df$Perc <- as.vector(cm_perc)
  cm_df$Label <- as.vector(labels)
  
  
  ggplot(data = cm_df, aes(x = Prediction, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Confusion Matrix - Tuned Model", x = "Predicted Label", y = "True Label") +
    theme_minimal()
}


plot_confusion_matrix(best_predictions, factor(y_test))

#This code develops a random forest model to predict wine quality based on the
#independent variables.


if (!require(randomForest)) install.packages("randomForest", dependencies = TRUE)
library(randomForest)


X <- data %>% select(-quality_class, -quality)  
y <- as.factor(data$quality_class)  


set.seed(1)  
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]


set.seed(1)
rf_model <- randomForest(X_train, y_train, 
                         ntree = 500,   
                         mtry = sqrt(ncol(X_train)),  
                         importance = TRUE)  


print(rf_model)


varImpPlot(rf_model, main = "Variable Importance in Random Forest")


rf_predictions <- predict(rf_model, newdata = X_test)


rf_conf_matrix <- confusionMatrix(rf_predictions, factor(y_test))


print(rf_conf_matrix)


plot_confusion_matrix <- function(predictions, actual) {
  cm <- table(Prediction = predictions, Actual = actual)
  
  
  cm_perc <- prop.table(cm) * 100
  
  
  labels <- paste0(cm, "\n", round(cm_perc, 2), "%")
  
  
  cm_df <- as.data.frame(cm)
  cm_df$Perc <- as.vector(cm_perc)
  cm_df$Label <- as.vector(labels)
  
  
  ggplot(data = cm_df, aes(x = Prediction, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Confusion Matrix - Random Forest Model", x = "Predicted Label", y = "True Label") +
    theme_minimal()
}


plot_confusion_matrix(rf_predictions, factor(y_test))

#this code uses hypertuning parameters to better tune the previous random forest model


set.seed(1)

X <- data %>% select(-quality_class, -quality)  
y <- as.factor(data$quality_class)  


trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
X_test <- X[-trainIndex, ]
y_train <- y[trainIndex]
y_test <- y[-trainIndex]


train_data <- data.frame(X_train, y_train = y_train)


train_control <- trainControl(
  method = "cv",  # Cross-validation
  number = 5,     # Number of folds
  search = "grid",  # Grid search
  verboseIter = TRUE
)


tune_grid <- expand.grid(
  mtry = c(2, 4, 6, 8),           
  splitrule = "gini",             
  min.node.size = c(1, 5, 10)     
)


set.seed(1)
tuned_rf_model <- train(
  y_train ~ .,  
  data = train_data,
  method = "ranger",  
  trControl = train_control,
  tuneGrid = tune_grid,
  importance = "impurity"  
)


print(tuned_rf_model$bestTune)


plot(tuned_rf_model)


best_rf_predictions <- predict(tuned_rf_model, newdata = X_test)


rf_conf_matrix_tuned <- confusionMatrix(best_rf_predictions, factor(y_test))


print(rf_conf_matrix_tuned)


plot_confusion_matrix <- function(predictions, actual) {
  cm <- table(Prediction = predictions, Actual = actual)
  
  
  cm_perc <- prop.table(cm) * 100
  
  
  labels <- paste0(cm, "\n", round(cm_perc, 2), "%")
  
  
  cm_df <- as.data.frame(cm)
  cm_df$Perc <- as.vector(cm_perc)
  cm_df$Label <- as.vector(labels)
  
  
  ggplot(data = cm_df, aes(x = Prediction, y = Actual, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Label), size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Confusion Matrix - Tuned Random Forest Model", x = "Predicted Label", y = "True Label") +
    theme_minimal()
}


plot_confusion_matrix(best_rf_predictions, factor(y_test))

#This code compares the performance metrics of the 4 models developed:
#decision tree, tuned decision tree, random forest, and tuned random forest.


get_metrics <- function(conf_matrix) {
  
  accuracy <- conf_matrix$overall["Accuracy"]
  
  
  precision <- tryCatch(conf_matrix$byClass["Pos Pred Value"], error = function(e) NA)
  recall <- tryCatch(conf_matrix$byClass["Sensitivity"], error = function(e) NA)
  f1 <- tryCatch(conf_matrix$byClass["F1"], error = function(e) NA)
  
  
  return(c(Accuracy = as.numeric(accuracy), 
           Precision = as.numeric(precision), 
           Recall = as.numeric(recall), 
           F1_Score = as.numeric(f1)))
}


dt_metrics <- get_metrics(conf_matrix)


tuned_dt_metrics <- get_metrics(conf_matrix_tuned)


rf_metrics <- get_metrics(rf_conf_matrix)


tuned_rf_metrics <- get_metrics(rf_conf_matrix_tuned)


comparison_df <- data.frame(
  Model = c("Decision Tree", "Tuned Decision Tree", "Random Forest", "Tuned Random Forest"),
  Accuracy = c(dt_metrics["Accuracy"], tuned_dt_metrics["Accuracy"], rf_metrics["Accuracy"], tuned_rf_metrics["Accuracy"]),
  Precision = c(dt_metrics["Precision"], tuned_dt_metrics["Precision"], rf_metrics["Precision"], tuned_rf_metrics["Precision"]),
  Recall = c(dt_metrics["Recall"], tuned_dt_metrics["Recall"], rf_metrics["Recall"], tuned_rf_metrics["Recall"]),
  F1_Score = c(dt_metrics["F1_Score"], tuned_dt_metrics["F1_Score"], rf_metrics["F1_Score"], tuned_rf_metrics["F1_Score"])
)


print(comparison_df)


library(ggplot2)
library(reshape2)


comparison_melted <- melt(comparison_df, id.vars = "Model")


ggplot(comparison_melted, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(title = "Model Performance Comparison",
       x = "Model",
       y = "Metric Value",
       fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))