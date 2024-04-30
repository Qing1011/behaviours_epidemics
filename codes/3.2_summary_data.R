get_summary <- function(data_s, filename){
  numeric_df <- data_s[sapply(data_s, is.numeric)]
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
  write.csv(summary_df, file = filename, row.names = FALSE)
}




independ_data <- visits_scores_wk[c(independent_vars_linear_base,'week')]
independ_data_s <- subset(independ_data, independ_data$week==1)
get_summary(independ_data_s, "Summary_linear_term.csv")


independ_data_smooth <- visits_scores_wk[c(dependent_var_list, independent_vars_smooth_base)] 
get_summary(independ_data_smooth, "Summary_visits_smooth_term.csv")
