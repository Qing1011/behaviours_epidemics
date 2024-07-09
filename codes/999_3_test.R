library(ggplot2)

# Selecting columns to plot
cols_to_plot <- c('Glocery_and_Pharmacy', 'General_Retail', 
                  'Art_and_Entertainment', 'Restaurant_and_Bar',
                  'Education', 'Healthcare')
cols_to_plot <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                    'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                    'Educations_visits_weekly', 'Healthcares_visits_weekly',
                    'others_visits_weekly')
# Creating plots for selected columns
plots <- lapply(cols_to_plot, function(col) {
  ggplot(visits_scores_wk, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "blue", color = "black") +
    ggtitle(paste("Distribution of", col)) +
    scale_x_log10()
})

# Display plots
plots
### test the linear regression model
for (dependent_var in dependent_var_list)
{
  smooth_parts <- paste(independent_vars_smooth_base, collapse = " + ")
  linear_parts <- paste(independent_vars_linear_base, collapse = " + ")
  formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts))
  lm_model <- lm(formula, data = visits_scores_wk)
  sum_lm_model <- summary(lm_model)
  model_summary <- capture.output(sum_lm_model)
  
  summary_file_name <- paste0("../results/", dependent_var, "_lm_summary.txt")
  writeLines(model_summary, summary_file_name)
}

