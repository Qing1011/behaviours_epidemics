### random forest####
library(ranger)
library(dplyr)
library(ggplot2)
library(caret)


# Selecting subset where name is not in the list
independent_var_list <- c(independent_vars_linear_base, independent_vars_smooth_base)
####################predictions####################################
tune_random_forest <- function(formula, data) {
  # Cross-validation setup (5-fold CV)
  control <- trainControl(method = "cv", number = 5)
  # Hyperparameter grid for tuning
  tune_grid <- expand.grid(
    mtry = c(2, 4, 6, 8),               # Number of variables considered at each split
    splitrule = c("variance", "extratrees"),  # Splitting criterion
    min.node.size = c(1, 5, 10)          # Minimum size of terminal nodes
  )
  
  # Random Forest model with hyperparameter tuning
  rf_model <- train(
    formula,
    data = data,
    method = "ranger",
    trControl = control,
    tuneGrid = tune_grid,
    num.trees = 50,                     # Number of trees in the forest
    importance = 'impurity'              # Measure variable importance
  )
  
  return(rf_model)
}


rsq_ls <- list()

for (y in dependent_var_list) {
  formula <- as.formula(paste(y, "~", paste(independent_var_list, collapse = "+")))
  
  # Fit and tune the random forest model
  tuned_model <- tune_random_forest(formula, visits_scores_wk)
  
  # Get the accuracy (or 1 - prediction error for regression tasks)
  tuned_accuracy <- max(tuned_model$results$Rsquared)  # Or you can use R-squared for regression
  
  # Store the accuracy in the list
  rsq_ls[[y]] <- tuned_accuracy
  
  # Print model summary for each dependent variable
  #print(tuned_model)
}
####################feature importance#############################

# Function to compute importance by shuffling features
compute_importance <- function(data, feature) {
  temp_data <- data
  temp_data[[feature]] <- sample(temp_data[[feature]])  # Shuffle the feature
  model <- ranger(
    formula = formula,
    data = temp_data,
    num.trees = 100
  )
  accuracy <- model$prediction.error
  decrease_in_accuracy <- accuracy - baseline_accuracy
  return(decrease_in_accuracy)
}

plot_list <- list()  # To store ggplot objects for later display

set.seed(123)  # For reproducibility across models

for (y in dependent_var_list) {
  formula <- as.formula(paste(y, "~", paste(independent_var_list, collapse = "+")))
  
  # Fit the initial model to compute baseline accuracy
  original_model <- ranger(
    formula = formula,
    data = visits_scores_wk,
    num.trees = 500,importance = 'permutation'
  )
  
  # Compute the baseline accuracy or prediction error
  #baseline_accuracy <- original_model$prediction.error
  
  # Apply this function to each feature
  #importance <- sapply(independent_var_list, function(feature) compute_importance(visits_scores_wk, feature))
  importance <- original_model$variable.importance
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
    num.permutations = 100
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
combined_df$Feature <- factor(combined_df$Feature, levels = unique(combined_df$Feature[order(combined_df$importance, decreasing = TRUE)]))

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



