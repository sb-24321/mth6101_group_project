#' Form a confusion matrix using the values from `code/1_binary_classifiers/1_binary_classifer'

# Imports ================================================================================
library(dplyr)

# Creates the confusion matrix ===========================================================
confusion_matrix <- function(folder_name){

  # Extract the Y true values and the Y predicted values.

  data <- read.csv(paste0("outputs/1_binary_classifier_outputs/", folder_name, "/data.csv"))

  data <- data%>%
    dplyr::transmute(Y_true, Y_pred)
  
  # Creates the confusion matrix.
  #
  # ------------------------
  # #   True    #  False   #
  # # Positive  # Negative # 
  # ------------------------
  # #   False   #   True   #
  # # Positive  # Negative # 
  # ------------------------
  #
  
  cat("Creating confusion matrix...\n")
  confusion_mat <- table(
    Actual = factor(data$Y_true, levels = c(1, 0)),
    Predicted = factor(data$Y_pred, levels = c(1, 0))
  )

  # Measure Performance.
  performance_details <- data.frame(
                                    True_Positive_Rate  = numeric(), 
                                    False_Positive_Rate = numeric(),
                                    False_Negative_Rate = numeric(),
                                    True_Negative_Rate  = numeric(),
                                    Precision = numeric(),
                                    Negative_predictive_value = numeric(),
                                    Accuracy   = numeric(),
                                    F1_score   = numeric(),
                                    Error_rate = numeric(),
  )


 
  filepath <- paste0("./outputs/1_binary_classifier_outputs/", folder_name)

  cat("Saving confusion matrix...\n")
  filename <- paste0("/confusion_matrix.csv")
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  write.csv(confusion_mat, paste0(filepath, filename), row.names = FALSE)
  cat(paste0("Results saved in: ", filepath, filename, "\n"))
  
  cat("Saving classifier performance info...\n")
  filename <- paste0("/classifier_performance.csv")
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)
  write.csv(performance_details, paste0(filepath, filename), row.names = FALSE)
  cat(paste0("Results saved in: ", filepath, filename, "\n"))

}


