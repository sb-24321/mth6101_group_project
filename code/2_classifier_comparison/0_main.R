#' Compiles and performs all the analysis for task (ii):
#' comparing the left-left and right-left classifiers using ROC curves and AUC.

# Get all the functions ==================================================================
source("code/2_classifier_comparison/1_roc_auc.R")

# Create function task_ii ================================================================
# This main function interfaces and performs all the individual functions that are
# implemented in the directory `2_classifier_comparison`.

task_ii <- function(folder_ll, folder_rl) {

  #' folder_ll: (str) the task (i) folder holding the left-left results.
  #' folder_rl: (str) the task (i) folder holding the right-left results.

  # ROC curves and AUC

  results <- roc_auc(folder_ll = folder_ll, folder_rl = folder_rl)

  print(results$auc)
  print(results$test)

  # Choose T for each classifier (Youden's J)
  best_ll <- coords(results$roc_ll, "best", best.method = "youden",
                    ret = c("threshold", "sensitivity", "specificity"))
  best_rl <- coords(results$roc_rl, "best", best.method = "youden",
                    ret = c("threshold", "sensitivity", "specificity"))

  print(best_ll)
  print(best_rl)

}


# Call here ==============================================================================

task_ii(
        folder_ll = "left_left_threshold_0.5",
        folder_rl = "right_left_threshold_0.5"
)
