if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
if (!requireNamespace("dlnm", quietly = TRUE)) install.packages("dlnm")
install.packages("pdp")

library(openxlsx)
library(mgcv)
library(stats)
library(ggplot2)
library(reshape2)
library(magick)
library(dlnm)
library(gridExtra)
library(dplyr)
library(pdp)
library(broom)
library(lmtest)



source('999_2_regression_fun_v2.R')

##### if have run summary please, skip this part #####
visits_scores_wk <- read.csv('../data/unpivot_merged_data_raw_1to1_s.csv')
subfolder_n <- c('/gam_model_1to1/') #loss_visitors
#visits_scores_wk <- read.csv(file.choose())


# Define the columns you want to divide
# The column to divide by
columns_to_divide_100 <- c('Grocery.Pharmacies_visits_weekly', 'Retails_visits_weekly', 
                           'Arts.Entertainment_visits_weekly', 'Restaurants.Bars_visits_weekly',
                           'Educations_visits_weekly', 'Healthcares_visits_weekly',
                           'others_visits_weekly','Grocery.Pharmacies_visits_weekly_lag1', 'Retails_visits_weekly_lag1', 
                           'Arts.Entertainment_visits_weekly_lag1', 'Restaurants.Bars_visits_weekly_lag1',
                           'Educations_visits_weekly_lag1', 'Healthcares_visits_weekly_lag1',
                           'others_visits_weekly_lag1')
columns_to_divide <- c("Bachelor",'No_health_insurance')

divisor_column <- "Population"

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
visits_scores_wk[['No_vehciles_perhousehold']] <- visits_scores_wk[['No_vehicles']]/visits_scores_wk[['Household_num']]
#####  used in the title #######
visits_scores_wk <- visits_scores_wk %>%
  rename(log_borough_case_count = 'borough_case_count_log', 
         temporal_discounting_score = 'regulated_tdscores_mode', #median
         loss_aversion_score = 'regulated_loss_mode', #
         agency_score = 'regulated_agency_mode',#
         stringency_index = 'StringencyIndex_WeightedAverage',
         no_health_insurance_rate = 'No_health_insurance_pp',
         no_vehicle_household_rate = 'No_vehciles_perhousehold',
         household_income = 'Household_income',
         percent_people_own_bachelor_degrees  = 'Bachelor_pp',
         weighted_average_age = 'weighted_estimated_average_age',
         Grocery_and_Pharmacy  = 'Grocery.Pharmacies_visits_weekly_pp',
         General_Retail  = 'Retails_visits_weekly_pp',
         Art_and_Entertainment  = 'Arts.Entertainment_visits_weekly_pp',
         Restaurant_and_Bar  = 'Restaurants.Bars_visits_weekly_pp',
         Education  = 'Educations_visits_weekly_pp',
         Healthcare = 'Healthcares_visits_weekly_pp',
         longitude = 'longitude',
         latitude = 'latitude'
)

name_display <- list('Grocery_and_Pharmacy' = 'Grocery/Pharmacy',
                  'General_Retail' = 'General Retail',
                   'Art_and_Entertainment' = 'Art/Entertainment',
                   'Restaurant_and_Bar' = 'Restaurant/Bar',
                    'Education'= 'Education',
                    'Healthcare' = 'Healthcare'
)

dependent_var_list <- c('Grocery_and_Pharmacy', 'General_Retail', 
                        'Art_and_Entertainment', 'Restaurant_and_Bar',
                        'Education', 'Healthcare')

independent_vars_smooth_base = c("week","log_borough_case_count")  #,'DEATH_COUNT_log' "borough_case_count"
independent_vars_linear_base = c("temporal_discounting_score","loss_aversion_score",'agency_score', 
                                 'stringency_index',"no_health_insurance_rate","no_vehicle_household_rate", "household_income", "percent_people_own_bachelor_degrees", 
                                 "weighted_average_age")

#### there are zeros in the raw and if i want to log it, replace an eplison value
### which is close to the second smallest not too small.

visits_scores_wk <- visits_scores_wk %>%
  mutate(
    Grocery_and_Pharmacy = ifelse(Grocery_and_Pharmacy == 0, 0.01, Grocery_and_Pharmacy),
    Education = ifelse(Education == 0, 0.01, Education),
    Healthcare = ifelse(Healthcare == 0, 0.01, Healthcare)
  )

##### run the full gam model results ######
run_gam_models(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, name_display, subfolder_n)
plot_gam_models(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, name_display, subfolder_n)
dependence_terms(dependent_var_list, independent_vars_linear_base, visits_scores_wk, subfolder_n)

###### combine the ALL THE dependence plots ######
png_files <- paste0("../results/", subfolder_n, dependent_var_list, "_gam_model_plot.png")
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
  image_name <-  paste0("../results",subfolder_n, "/combined_image.png")
  image_write(combined_image, image_name)
} else {
  warning("No images were read successfully.")
}

##### only the score plots ######
plot_scores(dependent_var_list,independent_vars_linear_base, subfolder_n)
png_files <- paste0("../results/", subfolder_n, dependent_var_list, "_score_plot.png")
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
  combined_score_image_name <- paste0("../results/",subfolder_n,"combined_image_scores_NEW.png")
  image_write(combined_image, combined_score_image_name)
} else {
  warning("No images were read successfully.")
}

##### plot only selected zipcodes ####

selected_zips <- list('10025','11234','11375','10304','10453')
plot_zipcodes_for_multiple_vars(visits_scores_wk, selected_zips, dependent_var_list,subfolder_n)
