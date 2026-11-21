
# using ROC curve and AUC value to compare LL and RL classifiers
library(pROC)

roc_auc <- function(folder_ll, folder_rl){ 
  # Read the saved similarities (threshold-independent) 

  ll <- read.csv(paste0("outputs/1_binary_classifier_outputs/", folder_ll, "/data.csv"))
  rl <- read.csv(paste0("outputs/1_binary_classifier_outputs/", folder_rl, "/data.csv"))

  
  # Build ROC objects.
  
  # levels = c(0, 1): 0 = controls (different turtles), 1 = cases (same turtle)
  # direction = "<": higher similarity means more likely the same turtle
  cat("Building ROC curves...\n")
  roc_ll <- roc(response = ll$Y_true, predictor = ll$Similarity,
                levels = c(0, 1), direction = "<")
  roc_rl <- roc(response = rl$Y_true, predictor = rl$Similarity,
                levels = c(0, 1), direction = "<")

  
  # AUC with 95% confidence intervals
  
  ci_ll <- ci.auc(roc_ll)
  ci_rl <- ci.auc(roc_rl)

  auc_table <- data.frame(
    Classifier = c("Left-Left", "Right-Left"),
    AUC        = c(as.numeric(auc(roc_ll)), as.numeric(auc(roc_rl))),
    CI_lower   = c(ci_ll[1], ci_rl[1]),
    CI_upper   = c(ci_ll[3], ci_rl[3])
  )

  
  # Test whether the AUCs differ significantly.
  # Unpaired: the two classifiers are evaluated on different sets of pairs.
  
  auc_test <- roc.test(roc_ll, roc_rl, paired = FALSE)

  
  # Save outputs
  
  filepath <- paste0("./outputs/2_comparison_outputs/", folder_ll, "_against_", folder_rl)
  dir.create(filepath, showWarnings = FALSE, recursive = TRUE)

  cat("Saving ROC plot...\n")
  png(paste0(filepath, "/roc_curves.png"), width = 1600, height = 1600, res = 250)
  plot(roc_ll, col = "#1f77b4", lwd = 2, legacy.axes = TRUE,
       main = "ROC curves: Left-Left vs Right-Left")
  plot(roc_rl, col = "#d62728", lwd = 2, lty = 2, add = TRUE)
  legend("bottomright",
         legend = c(sprintf("Left-Left (AUC = %.3f)",  auc_table$AUC[1]),
                    sprintf("Right-Left (AUC = %.3f)", auc_table$AUC[2])),
         col = c("#1f77b4", "#d62728"), lty = c(1, 2), lwd = 2)
  dev.off()

  cat("Saving AUC table...\n")
  write.csv(auc_table, paste0(filepath, "/auc.csv"), row.names = FALSE)

  cat(paste0("Results saved in: ", filepath, "\n"))

  
  # Return the objects so they can be reused (e.g. for choosing T)
  
  invisible(list(roc_ll = roc_ll, roc_rl = roc_rl,
                 auc = auc_table, test = auc_test))
}
