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

visits_scores_wk <- read.csv('../results/unpivot_merged_data_raw_v9.csv')

### only use the fourth week of march
names_to_exclude <- c(10003,10004,10007,10009)

# Selecting subset where name is not in the list
visits_scores_wk <- visits_scores_wk[!visits_scores_wk$zip_char %in% names_to_exclude, ]

# Define the columns you want to divide
# The column to divide by
columns_to_divide_100 <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                       'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                       'Educations_visits_weekly', 'Healthcares_visits_weekly',
                       'others_visits_weekly')
columns_to_divide <- c("BACHELOR_S","BLACK","HISPANIC")

divisor_column <- "POP_DENOMINATOR"

# Loop through each column to divide
for (col in columns_to_divide_100) {
  # Construct the new column name
  new_col_name <- paste(col, "pp", sep = "_")
  visits_scores_wk[[new_col_name]] <- (visits_scores_wk[[col]] / visits_scores_wk[[divisor_column]])*100
}
for (col in columns_to_divide) {
  # Construct the new column name
  new_col_name <- paste(col, "pp", sep = "_")
  visits_scores_wk[[new_col_name]] <- (visits_scores_wk[[col]] / visits_scores_wk[[divisor_column]])
}
visits_scores_wk[["density"]] <- visits_scores_wk[[divisor_column]]/visits_scores_wk[["AREA"]]

visits_scores_wk[['no_vehciles_perhousehold']] <- visits_scores_wk[['no_vehicles']]/visits_scores_wk[['household_num']]

#####  not mapping yet #######

name_mapping_dep <- list( 'week' = 'week',
  'borough_case_count_log'= 'log borough case count', 
 'DEATH_COUNT_log'= 'log NYC death count', 
 'regulated_scores_median' = 'temporal discounting score',
 'regulated_loss_median' = 'loss aversion score' ,
 'regulated_agency_median' = 'agency score',
  'StringencyIndex_WeightedAverage' = 'stringency index',
  'NO_HEALTH_INSURANCE' = 'no health insurance rate',
  'no_vehciles_perhousehold' = 'no vehicle household rate',
  'HOUSEHOLD_SIZE' = 'household size',
  'HOUSEHOLD_INCOME' = 'household income',
  'BACHELOR_S_pp' = 'percent people own bachelor degrees',
  'EstimatedAverageAge' = 'weighted averages age',
  'BLACK_pp' = 'percent Black residents',
  'HISPANIC_pp' = 'percent Hispanic residents',
  'Glocery.Pharmacies_visits_weekly_pp' = 'Glocery.Pharmacies',
  'Retails_visits_weekly_pp' ='Retails',
  'Arts.Entertainment_visits_weekly_pp' = 'Arts.Entertainment',
  'Restaurants.Bars_visits_weekly_pp' = 'Restaurants.Bars',
  'Educations_visits_weekly_pp'= 'Educations',
  'Healthcares_visits_weekly_pp'= 'Healthcares'
  )


dependent_var_list <- c('Glocery.Pharmacies_visits_weekly_pp', 'Retails_visits_weekly_pp', 
                        'Arts.Entertainment_visits_weekly_pp', 'Restaurants.Bars_visits_weekly_pp',
                        'Educations_visits_weekly_pp', 'Healthcares_visits_weekly_pp')

independent_vars_smooth_base = c("week","borough_case_count_log")  #,'DEATH_COUNT_log'
independent_vars_linear_base = c("regulated_scores_median","regulated_loss_median",'regulated_agency_median', 
                            'StringencyIndex_WeightedAverage', "NO_HEALTH_INSURANCE","no_vehciles_perhousehold","HOUSEHOLD_SIZE", "HOUSEHOLD_INCOME", "BACHELOR_S_pp", "EstimatedAverageAge","BLACK_pp",
                            "HISPANIC_pp") 


independ_data <- visits_scores_wk[c(independent_vars_linear_base,'week')]
independ_data_s <- subset(independ_data, independ_data$week==1)
get_summary(independ_data_s, "../results/Summary_linear_term.csv")


independ_data_smooth <- visits_scores_wk[c(dependent_var_list, independent_vars_smooth_base)] 
get_summary(independ_data_smooth, "../results/Summary_visits_smooth_term.csv")
