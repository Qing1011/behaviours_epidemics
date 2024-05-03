if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
if (!requireNamespace("dlnm", quietly = TRUE)) install.packages("dlnm")

library(openxlsx)
library(mgcv)
library(stats)
library(ggplot2)
library(reshape2)
library(magick)
library(dlnm)
library(gridExtra)

source('999_2_regression_fun.R')

##### if have run summary please, skip this part #####
visits_scores_wk <- read.csv('results/unpivot_merged_data_raw_v9.csv')

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

#####  used in the title #######

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


##### plot the full gam model results ######
plot_gam_models(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, name_mapping_dep)

###### combine the dependence plots ######
png_files <- paste0(dependent_var_list, "_gam_model_plot.png")
images <- list()

# Read the images, with a check to ensure each file is successfully read
for (file_name in png_files) {
  if (file.exists(file_name)) {
    images[[file_name]] <- image_read(file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}

# Ensure there are images to combine
if (length(images) > 0) {
  # Combine images horizontally
  combined_image <- image_append(image_join(images), stack = FALSE)
  
  # Save the combined image
  image_write(combined_image, "../results/combined_image.png")
} else {
  warning("No images were read successfully.")
}

##### only the score plots ######

plot_scores(dependent_var_list)
png_files <- paste0(dependent_var_list, "_score_plot.png")
images <- list()

# Read the images, with a check to ensure each file is successfully read
for (file_name in png_files) {
  if (file.exists(file_name)) {
    images[[file_name]] <- image_read(file_name)
  } else {
    warning(paste("File not found:", file_name))
  }
}
# Ensure there are images to combine
if (length(images) > 0) {
  # Combine images horizontally
  combined_image <- image_append(image_join(images), stack = FALSE)
  
  # Save the combined image
  image_write(combined_image, "../results/combined_image_scores.png")
} else {
  warning("No images were read successfully.")
}

##### plot only selected zipcodes ####

selected_zips <- list('10025','11234','11375','10304')
plot_zipcodes_for_multiple_vars(visits_scores_wk, selected_zips, dependent_var_list)
