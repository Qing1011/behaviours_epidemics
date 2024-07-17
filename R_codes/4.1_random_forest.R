### random forest####
library(ranger)
library(dplyr)
library(ggplot2)


# Selecting subset where name is not in the list
independent_var_list <- c(independent_vars_linear_base, independent_vars_smooth_base)

plot_list <- list()  # To store ggplot objects for later display

set.seed(123)  # For reproducibility across models

for (y in dependent_var_list) {
  formula <- as.formula(paste(y, "~", paste(independent_var_list, collapse = "+")))
  
  # Fit the initial model to compute baseline accuracy
  original_model <- ranger(
    formula = formula,
    data = visits_scores_wk,
    num.trees = 500
  )
  
  # Compute the baseline accuracy or prediction error
  baseline_accuracy <- original_model$prediction.error
  
  # Function to compute importance by shuffling features
  compute_importance <- function(data, feature) {
    temp_data <- data
    temp_data[[feature]] <- sample(temp_data[[feature]])  # Shuffle the feature
    model <- ranger(
      formula = formula,
      data = temp_data,
      num.trees = 500
    )
    accuracy <- model$prediction.error
    decrease_in_accuracy <- accuracy - baseline_accuracy
    return(decrease_in_accuracy)
  }
  
  # Apply this function to each feature
  importance <- sapply(independent_var_list, function(feature) compute_importance(visits_scores_wk, feature))
  importance_normalized <- importance / max(importance)


  # Create dataframe for plotting
  importance_df <- data.frame(
    Feature = names(importance),
    Importance = as.numeric(importance_normalized) #### to normalise it
  )
  
  # Plot using ggplot2
  p <- ggplot(importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_col(fill = "blue") +
    coord_flip() +
    labs(title = paste("Permutation Importance for", y), x = "Features", y = "Decrease in Model Accuracy") +
    theme_minimal()
  
  plot_list[[y]] <- p  # Store the plot for later use
}


results_list <- list()

for (y in dependent_var_list) {
  # Construct the formula dynamically
  formula <- as.formula(paste(y, "~", paste(independent_var_list, collapse = "+")))
  rf.visit <- ranger(
    formula = formula,
    data = visits_scores_wk,
    importance = 'permutation',
    num.trees = 500
  )
  
  # Call the importance_pvalues function
  results <- importance_pvalues(rf.visit,
    method = "altmann",
    formula = formula,
    data = visits_scores_wk,
    num.permutations = 500
  )

  results_list[[y]] <- results
} 

print(results_list)

# Combine all results into one data frame with an additional column for the dependent variable name
combined_df <- do.call(rbind, lapply(seq_along(results_list), function(i) {
  df <- data.frame(results_list[[i]])
  df$Feature <- rownames(df)
  df$DependentVariable <- names(results_list)[i]
  return(df)
}))

# Convert factors if necessary (especially if you have factor levels that need ordering)
combined_df$Feature <- factor(combined_df$Feature, levels = unique(combined_df$Feature))

# Plotting
p <- ggplot(combined_df, aes(x = reorder(Feature, -importance), y = importance, fill = pvalue < 0.05)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey"), name = "Significant (p < 0.05)") +
  facet_wrap(~DependentVariable, scales = "free_y", ncol = 2) +  # Adjust ncol as needed
  labs(title = "Feature Importance and Significance across Dependent Variables",
       x = "Features",
       y = "Importance") +
  theme_minimal() +
  theme(legend.position = "top",
        strip.text = element_text(face = "bold", size = 8, color = "darkblue"))  # Adjusting the facet label appearance



