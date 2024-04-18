### random forest####
library('ranger')
visits_scores_wk <- read.csv(file.choose())
names_to_exclude <- c(10003,10004,10007,10009)

# Selecting subset where name is not in the list
visits_scores_wk <- visits_scores_wk[!visits_scores_wk$zip_char %in% names_to_exclude, ]
columns_to_divide <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                       'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                       'Educations_visits_weekly', 'Healthcares_visits_weekly',
                       'others_visits_weekly',"BACHELOR_S","BLACK","HISPANIC","AREA",'vehicle_owned')


divisor_column <- "POPULATION"

# Loop through each column to divide
for (col in columns_to_divide) {
  # Construct the new column name
  new_col_name <- paste(col, "pp", sep = "_")
  visits_scores_wk[[new_col_name]] <- visits_scores_wk[[col]] / visits_scores_wk[[divisor_column]]
}

visits_scores_wk[['no_vehciles_perhousehold']] <- 
  visits_scores_wk[['no_vehicles']] / visits_scores_wk[['household_num']]



dependent_var_list <- c('Glocery.Pharmacies_visits_weekly_pp', 'Retails_visits_weekly_pp', 
                        'Arts.Entertainment_visits_weekly_pp', 'Restaurants.Bars_visits_weekly_pp',
                        'Educations_visits_weekly_pp', 'Healthcares_visits_weekly_pp')

independent_var_list = c("week","borough_case_count_log",
                         "regulated_loss_median",'regulated_agency_median', "regulated_scores_median",
                                 'StringencyIndex_WeightedAverage',"BACHELOR_S_pp", "NO_HEALTH_INSURANCE","BLACK_pp",
                                 "HISPANIC_pp","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                                 "EstimatedAverageAge","no_vehciles_perhousehold",
                         'zip_char','DEATH_COUNT_log') 

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
  
  # Create dataframe for plotting
  importance_df <- data.frame(
    Feature = names(importance),
    Importance = as.numeric(importance)
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

library(dplyr)
library(ggplot2)

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
ggplot(combined_df, aes(x = reorder(Feature, -importance), y = importance, fill = pvalue < 0.05)) +
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




