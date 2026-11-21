#' Compiles and performs all the analysis for task (ii):
#' comparing the left-left and right-left classifiers using ROC curves and AUC.

# Get all the functions ==================================================================
source("code/2_classifier_comparison/1_roc_auc.R")
source("code/1_binary_classifiers/0_main.R")

# Create function task_ii ================================================================
# This main function interfaces and performs all the individual functions that are
# implemented in the directory `2_classifier_comparison`.

task_ii <- function(folder_ll, folder_rl) {

  #' folder_ll: (str) the task (i) folder holding the left-left results.
  #' folder_rl: (str) the task (i) folder holding the right-left results.

  # ROC curves and AUC

  results <- roc_auc(folder_ll = folder_ll, folder_rl = folder_rl)

  # Choose T for each classifier (Youden's J)
  best_ll <- coords(results$roc_ll, "best", best.method = "youden",
                    ret = c("threshold", "sensitivity", "specificity"))
  best_rl <- coords(results$roc_rl, "best", best.method = "youden",
                    ret = c("threshold", "sensitivity", "specificity"))

  
  invisible(list(results = results, best_ll = best_ll, best_rl = best_rl))

}

# Call here ==============================================================================
main <- function(){
  
  # Testing basic 0.5 thresholds
  results <- task_ii(
                     folder_ll = "left_left_threshold_0.5",
                     folder_rl = "right_left_threshold_0.5"
  )
  
  # Testing the thresholds given by Youdena (these are NOT optimal)
  best_thresh_rl <- results$best_rl$threshold
  folder_name_rl <- paste0("right_left_threshold_", round(best_thresh_rl, 3)) 
  best_thresh_ll <- results$best_ll$threshold
  folder_name_ll <- paste0("left_left_threshold_", round(best_thresh_ll, 3)) 
  results <- task_ii(folder_ll = folder_name_ll, folder_rl = folder_name_rl)


}

main()
