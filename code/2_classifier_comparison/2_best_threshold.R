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
  TP <- confusion_mat[1]
  FP <- confusion_mat[2]
  FN <- confusion_mat[3]
  TN <- confusion_mat[4]
  P <- TP + FN
  N <- FP + TN

  return(list(
              MSE = ((FP + FN) / (P + N)), # Aim to minimise
              Sensitivity = (TP / P), # Aim to maximise
              Specificity = (TN / N), # Aim to maximise

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
                        MSE = numeric()
  )

  for (thresh_value in thresh_values){
    mse <- calculate_methods(y_true = data$Y_true, y_pred = data[,thresh_value])
    results <- rbind(results, data.frame(Threshold = thresh_value, MSE = mse))
  }


  min_index <- (which.min(results$MSE))
  return(results[min_index,])


}

cat("\n\n### BEST LEFT-LEFT THRESHOLD: ###\n")
print(best_threshold('left_left_threshold_0.5'))

#cat("\n\n### BEST RIGHT-LEFT THRESHOLD: ###\n") 
#print(best_threshold('right_left_threshold_0.5'))
