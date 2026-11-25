library(dplyr)

# Able to find the best threshold for a given method
# 
# mse = Find the thresholds which give the smallest MSE

calculate_methods <- function(y_true, y_pred){

  # Create the confusion matrix of this data.
  confusion_mat <- table(
                         Actual = factor(y_true, levels = c(1, 0)),
                         Predicted = factor(y_pred, levels = c(1, 0))
  )

  
  # Extract values from table
  TP <- as.numeric(confusion_mat[1])
  FP <- as.numeric(confusion_mat[2])
  FN <- as.numeric(confusion_mat[3])
  TN <- as.numeric(confusion_mat[4])
  P <- TP + FN
  N <- FP + TN

  Precision <- (TP) / (TP + FP) 
  Recall <- (TP) / (TP + FN) 
  
  MCC_den <- (TP + FP)*(P)*(N)*(TN + FN)

  return(list(
              # Aim to minimise
              MSE = ((FP + FN) / (P + N)),

              # Aim to maximise the following
              Sensitivity = (TP / P), 
              Specificity = (TN / N),
              Precision = (TP) / (TP + FP), # Reliability of positive predictions.
              MCC = (TP * TN - FP * FN)/(sqrt(MCC_den)),
              F1_score = 2*(Precision * Recall)/(Precision + Recall),
              Balanced_Accuracy = ((TP / P) + (TN/N))/2

              )
  )

}




best_threshold <- function(folder_name){
  # This function find the best threshold for the dataset.
  # We will do this by minimising the MSE.
  # We will just brute force ts.

  # Start by getting a dataset where the similarity has already been calculated.
  # This is given by `folder_name`

  filepath <- paste0("outputs/1_binary_classifier_outputs/", folder_name, "/data.csv")
  data <- read.csv(filepath)

  data <- data%>%
    dplyr::transmute(Similarity, Y_true)

  # Get the predicted y values for all the thresholds.
  for (thresh in seq(from=0, to=1, by=0.001)){
    data[as.character(thresh)] = as.integer(data$Similarity > thresh)
  }

  thresh_values <- colnames(data)[-c(1,2)]

  results <- data.frame(
                        Threshold = numeric(),
                        Stats = numeric()
  )

  for (thresh_value in thresh_values){
    threshold_stats <- calculate_methods(y_true = data$Y_true, y_pred = data[,thresh_value])
    results <- rbind(results, data.frame(Threshold = thresh_value, Stats = threshold_stats))
  }
  

  
  cat("\n\n### BEST MSE ###\n")
  print(results[which.min(results$Stats.MSE), ])
  cat("\n\n### BEST SENSITIVITY ###\n") 
  print(results[which.max(results$Stats.Sensitivity), ])
  cat("\n\n### BEST SPECIFICITY ###\n")
  print(results[which.max(results$Stats.Specificity), ])
  cat("\n\n### BEST PRECISION ###\n")
  print(results[which.max(results$Stats.Precision), ])
  cat("\n\n### BEST MCC ###\n")
  print(results[which.max(results$Stats.MCC), ])
  cat("\n\n### BEST F1 SCORE ###\n")
  print(results[which.max(results$Stats.F1_score), ])
  cat("\n\n### BEST BALANCED ACCURACY ###\n")
  print(results[which.max(results$Stats.Balanced_Accuracy), ])


}

cat("\n\n#################################\n")
cat("### BEST LEFT-LEFT THRESHOLD: ###\n")
cat("#################################\n\n")
best_threshold('left_left_threshold_0.5')

#cat("\n\n### BEST RIGHT-LEFT THRESHOLD: ###\n") 
#print(best_threshold('right_left_threshold_0.5'))



# NOTE: DUE TO THE SEVERE IMBALANCE BETWEEN POSITIVE AND NEGATIVE CLASSES, WE SHOULD
#       PRIORISE THE FOLLOWING STATISTICS:
#       > MCC (considers all four confusion matrix conclusions)
#       > F1 Score (balances precision and recall)
#       > Balanced Accuracy (considers both specificity and specificity)
