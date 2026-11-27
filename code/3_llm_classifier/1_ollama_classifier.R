#AI DISCLAIMER: Code has been written with OPENAI tools.

# Task (iii): LLM classifier for right-left, different-year turtle pairs.
# Run from the repository root. Install httr2 and jsonlite once.
# Install Ollama and run: ollama pull qwen2.5:7b
# No credentials or external API calls are used.
# Review sample proportions before changing RUN_LLM to TRUE.

# ** Proportional Stratified Random Sampling Without Replacement. ** 
# ** Set N >= 34 or else there will be no matching pairs in the sample. **



RUN_LLM <- FALSE
N <- 34L
SEED <- 123L
MODEL <- "qwen2.5:7b"

source("code/0_load_data.R")
stopifnot(ncol(features) == 1540L)
X <- as.matrix(features[, paste0("feature_", 1:1536)])
stopifnot(is.numeric(X), all(is.finite(X)), !anyNA(features$identity))
years <- substr(as.character(features$date), 1, 4)
stopifnot(all(grepl("^[0-9]{4}$", years)))
pairs <- expand.grid(right = which(features$orientation == "right"),
                     left = which(features$orientation == "left"))
pairs <- pairs[years[pairs$right] != years[pairs$left], , drop = FALSE]
pairs$Y_true <- as.integer(features$identity[pairs$right] == features$identity[pairs$left])
stopifnot(N >= 20L, N <= nrow(pairs))
pos <- which(pairs$Y_true == 1L)
neg <- which(pairs$Y_true == 0L)
npos <- round(N * length(pos) / nrow(pairs))
set.seed(SEED)
ids <- c(pos[sample.int(length(pos), npos)],
         neg[sample.int(length(neg), N - npos)])
results <- pairs[ids[sample.int(length(ids))], , drop = FALSE]
rownames(results) <- NULL
results$pair_id <- seq_len(N)
print(rbind(population = prop.table(table(factor(pairs$Y_true, levels = 0:1))),
            sample = prop.table(table(factor(results$Y_true, levels = 0:1)))))
if (length(unique(results$Y_true)) < 2L)
  warning("Sample has only one class; consider increasing N before running.")

# Exact instruction is identical for each pair; only the vectors change.
# Round prompt values to six decimal places to reduce context length.
PROMPT <- paste("You are a binary classifier for individual turtle identification.",
 "The two 1536-dimensional MegaDescriptor vectors describe opposite sides",
 "photographed in different years. Predict whether they represent the same",
 "individual turtle. Return a JSON object with prediction equal to same or different.")
vector_text <- function(i) paste(sprintf("%.6f", X[i, ]), collapse = ", ")
results$prompt <- vapply(seq_len(N), function(k) paste0(PROMPT,
 "\nVector A: [", vector_text(results$right[k]), "]",
 "\nVector B: [", vector_text(results$left[k]), "]"), character(1))
results$response <- NA_character_
results$Y_pred <- NA_integer_
results$model <- MODEL
results$local_model <- NA_character_
results$input_tokens <- NA_integer_
results$output_tokens <- NA_integer_
results$error <- NA_character_
results$Similarity <- vapply(seq_len(N), function(k) {
 a <- X[results$right[k], ]; b <- X[results$left[k], ]
 sum(a*b)/sqrt(sum(a*a)*sum(b*b))
}, numeric(1))
stopifnot(all(is.finite(results$Similarity)))

if (RUN_LLM) {
  stopifnot(requireNamespace("httr2", quietly = TRUE),
            requireNamespace("jsonlite", quietly = TRUE))
  # Only contact the local Ollama service. No proxy or redirects.
  out <- file.path("outputs", "3_ollama_classifier_outputs",
                   format(Sys.time(), "%Y%m%d_%H%M%S"))
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  save_results <- function() {
    saveRDS(results, file.path(out, "results.rds"))
    write.csv(results, file.path(out, "results.csv"), row.names = FALSE)
  }
  save_results()
  for (k in seq_len(N)) {
    tryCatch({
      body <- list(model = MODEL, prompt = results$prompt[k], stream = FALSE,
        options = list(temperature = 0, seed = SEED, num_ctx = 32768L, num_predict = 64L),
        format = list(type = "object",
          properties = list(prediction = list(type = "string", enum = c("same", "different"))),
          required = list("prediction"), additionalProperties = FALSE))
      res <- httr2::request("http://127.0.0.1:11434/api/generate") |>
        httr2::req_options(proxy = "", followlocation = FALSE) |>
        httr2::req_body_json(body) |>
        httr2::req_timeout(1800) |>
        httr2::req_perform()
      raw <- httr2::resp_body_string(res)
      writeLines(raw, file.path(out, sprintf("response_%03d.json", k)))
      obj <- jsonlite::fromJSON(raw, simplifyVector = FALSE)
      results$local_model[k] <- obj$model
      results$input_tokens[k] <- if (is.null(obj$prompt_eval_count)) NA_integer_ else obj$prompt_eval_count
      results$output_tokens[k] <- if (is.null(obj$eval_count)) NA_integer_ else obj$eval_count
      results$response[k] <- obj$response
      if (!isTRUE(obj$done) || !identical(obj$done_reason, "stop")) stop("Incomplete response")
      answer <- jsonlite::fromJSON(results$response[k])$prediction
      if (length(answer) != 1L || !answer %in% c("same", "different"))
        stop("Invalid binary response")
      results$Y_pred[k] <- as.integer(answer == "same")
    }, error = function(e) {
      results$error[k] <<- conditionMessage(e)
    })
    save_results()
    cat("Pair", k, "of", N, ":", results$Y_pred[k], "\n")
    if (!is.na(results$error[k])) {
      warning("Stopped after local model/response error. Partial results saved in ", out)
      break
    }
  }
  # Compare all classifiers on the identical successfully classified subset.
  evaluate <- function(actual, predicted) {
    cm <- table(actual = factor(actual, levels = 0:1),
                predicted = factor(predicted, levels = 0:1))
    tn <- cm[1,1]; fp <- cm[1,2]; fn <- cm[2,1]; tp <- cm[2,2]
    div <- function(a,b) if (b == 0) NA_real_ else a/b
    c(TN=tn, FP=fp, FN=fn, TP=tp, accuracy=div(tp+tn,sum(cm)),
      precision=div(tp,tp+fp), recall=div(tp,tp+fn),
      F1=div(2*tp,2*tp+fp+fn),
      balanced_accuracy=(div(tp,tp+fn)+div(tn,tn+fp))/2,
      MCC=div(tp*tn-fp*fn,sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn))))
  }
  ok <- !is.na(results$Y_pred)
  metrics <- rbind(LLM = evaluate(results$Y_true[ok], results$Y_pred[ok]))
  for (t in c(0.5, 0.386, 0.304, 0.111)) {
    metrics <- rbind(metrics, evaluate(results$Y_true[ok],
                                       as.integer(results$Similarity[ok] > t)))
    rownames(metrics)[nrow(metrics)] <- paste0("cosine_", t)
  }
  write.csv(metrics, file.path(out, "metrics.csv"))
  print(metrics)
  cat("Completed", sum(ok), "of", N, "comparisons. Output:", out, "\n")
} else {
  message("Preview only: no model requests made. Review proportions, then set RUN_LLM <- TRUE.")
}

