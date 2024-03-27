if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")

library(openxlsx)
library(mgcv)
library(stats)
library(ggplot2)
library(reshape2)
library(magick)
###
#{0: 'Glocery&Pharmacies', 1: 'Retails', 2: 'Arts&Entertainment', 3: 'Restaurants&Bars',
 # 4: 'Educations', 5: 'Healthcares', 6: 'others'}
##

visits_scores_wk <- read.csv(file.choose())
visits_scores_wk$agency <- visits_scores_wk$rescale_avail + visits_scores_wk$rescale_avail
### only use the fourth week of march
# Define the columns you want to divide
# The column to divide by
divisor_column <- "POPULATION"

# Loop through each column to divide
for (col in columns_to_divide) {
  # Construct the new column name
  new_col_name <- paste(col, "pp", sep = "_")
  visits_scores_wk[[new_col_name]] <- visits_scores_wk[[col]] / visits_scores_wk[[divisor_column]]
}



gam_model <- gam(Restaurants.Bars_visits_weekly ~ s(CASE_COUNT) + 
                      s(DEATH_COUNT) + s(week) + gain_loss_bias + 
                      agency + score + HOUSEHOLD_SIZE + 
                      HOUSEHOLD_INCOME +  BLACK + HISPANIC +
                      AGE65_PLUS + BACHELOR_S + NO_HEALTH_INSURANCE,
                    data = df_small, family = gaussian())

summary(gam_model)

df_small <- visits_scores_wk[visits_scores_wk["week"] > 0, ]
dependent_var_list <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                        'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                        'Educations_visits_weekly', 'Healthcares_visits_weekly',
                        'others_visits_weekly')

independent_vars <- c("CASE_COUNT", "DEATH_COUNT", "week", "gain_loss_bias",
                      "agency", "score","HOUSEHOLD_SIZE", "POPULATION",
                      "HOUSEHOLD_INCOME", "BLACK", "HISPANIC", "AGE65_PLUS",
                      "BACHELOR_S", "NO_HEALTH_INSURANCE")

for (dependent_var in dependent_var_list) {
  # Building the formula
  formula_parts <- paste("s(", independent_vars, ")", collapse = " + ")
  formula <- as.formula(paste(dependent_var, "~", formula_parts))
  
  # Model fitting
  gam_model <- gam(formula, data = df_small, family = gaussian())
  
  # Summary and plot of the model
  # summary(gam_model)
  model_summary <- capture.output(summary(gam_model))
  
  # Generate a file name based on the current dependent variable
  summary_file_name <- paste0(dependent_var, "_model_summary.txt")
  
  # Write the captured summary to a text file
  writeLines(model_summary, summary_file_name)
  
  file_name <- paste0(dependent_var, "_gam_model_plot.png")
  
  # Open a PNG device
  png(file_name, width = 800, height = 600) # Adjust the size as needed
  
  # Plot the model
  plot(gam_model, pages = 1, all.terms = TRUE)
  
  # Close the device
  dev.off()
  
  #plot(gam_model, pages = 1, all.terms = TRUE)
  
  # Making predictions and updating the dataframe
  predictions <- predict(gam_model, type = "response")
  df_small$predicted <- predictions
  
  # Select relevant columns for plotting
  df <- df_small[, c("week", "predicted", dependent_var, "zip_char")]
  
  # Melt the dataframe to long format
  df_long <- melt(df, id.vars = c("week", "zip_char"), variable.name = "type", value.name = "value")
  
  # Adjusting color mapping for plotting
  color_mapping <- c(predicted = "red")
  color_mapping[dependent_var] <- "blue"
  
  # Plot
  p <- ggplot(df_long, aes(x = week, y = value, color = type)) +
    geom_line() +
    facet_wrap(~zip_char, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "True vs Predicted Values by Zipcode",
      x = "Week",
      y = "Value"
    ) +
    scale_color_manual(values = color_mapping)
  ggsave(paste(dependent_var, ".png", sep = ""), plot = p)
}

##############new data set 
dependent_var_list <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                        'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                        'Educations_visits_weekly', 'Healthcares_visits_weekly',
                        'others_visits_weekly')

independent_vars <- c("CASE_COUNT", "DEATH_COUNT", "gain_loss_bias",
                      "agency", "score","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                      "EstimatedAverageAge",'StringencyIndex_WeightedAverage',
                      "BACHELOR_S", "NO_HEALTH_INSURANCE","week","POPULATION")
#,"BLACK","HISPANIC"

independent_vars_smooth = c("CASE_COUNT", "DEATH_COUNT","week") 
# Variables to include with smoothing
independent_vars_linear = c("gain_loss_bias",
                            "agency", "score","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", 
                            "EstimatedAverageAge",'StringencyIndex_WeightedAverage',
                            "BACHELOR_S", "NO_HEALTH_INSURANCE","POPULATION","BLACK","HISPANIC") 
# Variables to include as linear terms

 
####
numeric_df <- visits_scores_wk[sapply(visits_scores_wk, is.numeric)]
summaries <- list()
# Loop over the columns of the data frame
for (variable_name in names(numeric_df)) {
  # Calculate the statistics for each column
  summaries[[variable_name]] <- c(
    Mean = mean(numeric_df[[variable_name]], na.rm = TRUE),
    StdDev = sd(numeric_df[[variable_name]], na.rm = TRUE),
    Min = min(numeric_df[[variable_name]], na.rm = TRUE),
    P25 = quantile(numeric_df[[variable_name]], 0.25, na.rm = TRUE),
    Median = median(numeric_df[[variable_name]], na.rm = TRUE),
    P75 = quantile(numeric_df[[variable_name]], 0.75, na.rm = TRUE),
    Max = max(numeric_df[[variable_name]], na.rm = TRUE)
  )
}
summary_df <- do.call(rbind, summaries)
summary_df <- data.frame(Variable = rownames(summary_df), summary_df, row.names = NULL)
write.csv(summary_df, file = "Summary_Visits_Scores.csv", row.names = FALSE)


for (dependent_var in dependent_var_list) {
  # Building the formula
  #formula_parts <- paste("s(", independent_vars, ")", collapse = " + ")
  #formula <- as.formula(paste(dependent_var, "~", formula_parts))
  smooth_parts = paste("s(", independent_vars_smooth, ")", collapse = " + ")
  linear_parts = paste(independent_vars_linear, collapse = " + ")
  formula <- as.formula(paste(dependent_var, "~", smooth_parts, "+", linear_parts))
  
  # Model fitting
  gam_model <- gam(formula, data = visits_scores_wk, family = gaussian())
  
  # Summary and plot of the model
  # summary(gam_model)
  model_summary <- capture.output(summary(gam_model))
  
  # Generate a file name based on the current dependent variable
  summary_file_name <- paste0(dependent_var, "_model_summary.txt")
  
  # Write the captured summary to a text file
  writeLines(model_summary, summary_file_name)
  
  #file_name <- paste0(dependent_var, "_gam_model_plot.png")
  # Open a PNG device
  #png(file_name, width = 1600, height = 1200) # Adjust the size as needed
  
  #ggsave(paste(dependent_var, ".png", sep = ""), plot = p, width = 10, height = 8, dpi = 450)
  
  
  # Plot the models
  #plot(gam_model, pages = 1, all.terms = TRUE)
  
  # Close the device
  #dev.off()
  num_terms <- length(independent_vars)
  file_name <- paste0(dependent_var, "_gam_model_plot.png")
  png(file_name, width = 400, height = num_terms* 400, res = 300) # Adjust the height as needed
  
  # Set up the plotting area
  par(mfrow=c(num_terms, 1), mar=c(4, 4, 2, 1))
  
  # Loop through each term and plot
  for(i in 1:num_terms) {
    plot(gam_model, select = i, all.terms = TRUE, main = paste("Term", i))
  }
  
  # Reset par to default
  par(mfrow=c(1, 1), mar=c(5, 4, 4, 2) + 0.1)
  
  # Close the device
  dev.off()
  
  # Making predictions and updating the dataframe
  predictions <- predict(gam_model, type = "response")
  visits_scores_wk$predicted <- predictions
  
  # Select relevant columns for plotting
  df <- visits_scores_wk[, c("week", "predicted", dependent_var, "zip_char")]
  
  # Melt the dataframe to long format
  df_long <- melt(df, id.vars = c("week", "zip_char"), variable.name = "type", value.name = "value")
  
  # Adjusting color mapping for plotting
  color_mapping <- c(predicted = "red")
  color_mapping[dependent_var] <- "blue"
  
  # Plot
  p <- ggplot(df_long, aes(x = week, y = value, color = type)) +
    geom_line() +
    facet_wrap(~zip_char, scales = "free_y") +
    theme_minimal() +
    labs(
      title = "True vs Predicted Values by Zipcode",
      x = "Week",
      y = "Value"
    ) +
    scale_color_manual(values = color_mapping)
  ggsave(paste(dependent_var, ".png", sep = ""), plot = p)
}

#### fixed one variable 
case_count_range <- seq(min(df_small$CASE_COUNT, na.rm = TRUE), 
                        max(df_small$CASE_COUNT, na.rm = TRUE), length.out = 100)

# Step 2: Create a data frame for predictions
# Here, we use mean values for other predictors; adjust as necessary for your analysis
predict_data <- expand.grid(
  CASE_COUNT = case_count_range,
  DEATH_COUNT = mean(df_small$DEATH_COUNT, na.rm = TRUE),
  week = mean(df_small$week, na.rm = TRUE),
  gain_loss_bias = mean(df_small$gain_loss_bias, na.rm = TRUE),
  agency = mean(df_small$agency, na.rm = TRUE), # Note: For categorical variables, choose representative categories
  score = mean(df_small$score, na.rm = TRUE),
  POPULATION = mean(df_small$POPULATION, na.rm = TRUE),
  HOUSEHOLD_SIZE = mean(df_small$HOUSEHOLD_SIZE, na.rm = TRUE),
  HOUSEHOLD_INCOME = mean(df_small$HOUSEHOLD_INCOME, na.rm = TRUE),
  BLACK = mean(df_small$BLACK, na.rm = TRUE),
  HISPANIC = mean(df_small$HISPANIC, na.rm = TRUE),
  AGE65_PLUS = mean(df_small$AGE65_PLUS, na.rm = TRUE),
  BACHELOR_S = mean(df_small$BACHELOR_S, na.rm = TRUE),
  NO_HEALTH_INSURANCE = mean(df_small$NO_HEALTH_INSURANCE, na.rm = TRUE)
)

# Step 3: Generate predictions
predict_data$predicted <- predict(gam_model, newdata = predict_data, type = "response")

# Step 4: Plotting the results
ggplot(predict_data, aes(x = CASE_COUNT, y = predicted)) + 
  geom_line() + 
  labs(x = "CASE_COUNT", y = "Predicted Restaurants and Bars Visits Weekly") + 
  theme_minimal()


### residuals
# Step 2: Create a plot
plot(visits_scores_wk$visits_weekly, predictions, xlab = "Actual Values", ylab = "Predicted Values", main = "Actual vs Predicted")

# Add a reference line for perfect prediction
abline(a = 0, b = 1, col = "red")

# add the predicted values
# visits_scores_wk$predicted<- predictions
df_small$predicted <- predictions
