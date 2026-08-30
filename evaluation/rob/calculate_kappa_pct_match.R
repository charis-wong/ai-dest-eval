library(tidyverse)
library(irr)


data<-read.csv("evaluation/rob/rob_overall.csv")

# Function to split labels and create confusion matrix
calculate_results <- function(eval_data) {
  # Split the label into two columns
  split_labels <- strsplit(eval_data$label, "-")
  
  # Extract tool1 and tool2 ratings
  tool1 <- sapply(split_labels, function(x) trimws(x[1]))
  tool2 <- sapply(split_labels, function(x) trimws(x[2]))
  
  # Create a data frame with expanded counts
  expanded_data <- data.frame(
    tool1 = rep(tool1, eval_data$count),
    tool2 = rep(tool2, eval_data$count)
  )
  

  # Calculate matches
  matches <- expanded_data$tool1 == expanded_data$tool2
  match_pct <- (sum(matches) / nrow(expanded_data)) * 100
  
  # Create confusion matrix
  conf_matrix <- table(expanded_data$tool1, expanded_data$tool2)
  
  # Calculate Cohen's Kappa
  # Convert to matrix format required by kappa2
  ratings_matrix <- cbind(
    as.numeric(factor(expanded_data$tool1, levels = unique(c(expanded_data$tool1, expanded_data$tool2)))),
    as.numeric(factor(expanded_data$tool2, levels = unique(c(expanded_data$tool1, expanded_data$tool2))))
  )
  
  kappa_result <- kappa2(ratings_matrix, weight = "unweighted")
  
  return(list(
    kappa = kappa_result$value,
    pval = kappa_result$p.value,
    match_pct = match_pct,
    conf_matrix = conf_matrix,
    n = nrow(expanded_data)
  ))
}

# Calculate results for each evaluation
results <- list()
unique_evals <- unique(data$eval)

for (eval_name in unique_evals) {
  eval_data <- data[data$eval == eval_name, ]
  results[[eval_name]] <- calculate_results(eval_data)
}

# Print results
for (eval_name in names(results)) {
  cat("\n", eval_name, "\n")
  cat("Cohen's Kappa:", round(results[[eval_name]]$kappa, 3), "\n")
  cat("p value:", round(results[[eval_name]]$pval,3), "\n")
  cat("Percentage of matched tags:", round(results[[eval_name]]$match_pct,3), "\n")
  cat("Total observations:", results[[eval_name]]$n, "\n")
  cat("Confusion Matrix:\n")
  print(results[[eval_name]]$conf_matrix)
  cat("----------------------------------------\n")
}

# Alternative: Create a summary table
summary_table <- data.frame(
  Evaluation = names(results),
  Kappa = sapply(results, function(x) round(x$kappa, 3)),
  p.val = sapply(results, function(x) round(x$pval, 5)),
  match_percentage = sapply(results, function(x) round(x$match_pct, 1)),
  N = sapply(results, function(x) x$n)
)

cat("\n\nSummary Table:\n")
print(summary_table)


write.csv(summary_table, "output/rob_kappa.csv", row.names=FALSE)



