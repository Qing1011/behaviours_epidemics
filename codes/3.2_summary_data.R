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

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
visits_scores_wk <- read.csv('../data/unpivot_merged_data_raw.csv')

### only use the fourth week of Feb
mod_counts <- read.csv('../results/modzcta_zip_counts.csv')
select_mod <- mod_counts[mod_counts$modzcta_count > 10, "MODZCTA"]

# Selecting subset where name is not in the list
visits_scores_wk <- visits_scores_wk[visits_scores_wk$MODZCTA %in% select_mod, ]


# Define the columns you want to divide
# The column to divide by
columns_to_divide_100 <- c('Glocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                       'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                       'Educations_visits_weekly', 'Healthcares_visits_weekly',
                       'others_visits_weekly')
columns_to_divide <- c("weighted_Bachelor","weighted_Black","weighted_Hispanic",'weighted_no_health_insurance')

divisor_column <- "weighted_Population"

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
#visits_scores_wk[["density"]] <- visits_scores_wk[[divisor_column]]/visits_scores_wk[["AREA"]]

visits_scores_wk[['no_vehciles_perhousehold']] <- visits_scores_wk[['weighted_No_vehicle']]/visits_scores_wk[['weighted_Households_num']]



#####  not mapping yet #######
name_mapping_dep <- list( 'week' = 'week',
                          'borough_case_count_log'= 'log borough case count', 
                          'COVID_CASE_COUNT_log' = 'log modzcta case count',
                          'DEATH_COUNT_log'= 'log NYC death count', 
                          'regulated_scores_median' = 'temporal discounting score',
                          'regulated_loss_median' = 'loss aversion score' ,
                          'regulated_agency_median' = 'agency score',
                          'StringencyIndex_WeightedAverage' = 'stringency index',
                          'weighted_no_health_insurance_pp' = 'no health insurance rate',
                          'no_vehciles_perhousehold' = 'no vehicle household rate',
                          'weighted_household_size_mean' = 'household size',
                          'weighted_household_income_mean' = 'household income',
                          'weighted_Bachelor_pp' = 'percent people own bachelor degrees',
                          'weighted_estimated_average_age_mean' = 'weighted average age',
                          'weighted_Black_pp' = 'percent Black residents',
                          'weighted_Hispanic_pp' = 'percent Hispanic residents',
                          'Glocery.Pharmacies_visits_weekly_pp' = 'Glocery and Pharmacy',
                          'Retails_visits_weekly_pp' ='General Retail',
                          'Arts.Entertainment_visits_weekly_pp' = 'Art and Entertainment',
                          'Restaurants.Bars_visits_weekly_pp' = 'Restaurant and Bar',
                          'Educations_visits_weekly_pp'= 'Education',
                          'Healthcares_visits_weekly_pp'= 'Healthcare'
)

names(visits_scores_wk)[names(visits_scores_wk) %in% names(name_mapping_dep)] <- name_mapping_dep

dependent_var_list <- c('Glocery and Pharmacy', 'General Retail', 
                        'Art and Entertainment', 'Restaurant and Bar',
                        'Education', 'Healthcare')

independent_vars_smooth_base = c("week","log borough case count","log modzcta case count")  #,'DEATH_COUNT_log'
independent_vars_linear_base = c("temporal discounting score","loss aversion score",'agency score', 
                            'stringency index', "no health insurance rate","no vehicle household rate","household size", "household income", "percent people own bachelor degrees", 
                            "weighted average age","percent Black residents",
                            "percent Hispanic residents") 


independ_data <- visits_scores_wk[c(independent_vars_linear_base,'week')]
independ_data_s <- subset(independ_data, independ_data$week==1)
get_summary(independ_data_s, "../results/Summary_linear_term.csv")


independ_data_smooth <- visits_scores_wk[c(dependent_var_list, independent_vars_smooth_base)] 
get_summary(independ_data_smooth, "../results/Summary_visits_smooth_term.csv")
