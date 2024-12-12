if (!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
if (!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")
if (!requireNamespace("openxlsx", quietly = TRUE)) install.packages("openxlsx")
if (!requireNamespace("magick", quietly = TRUE)) install.packages("magick")
if (!requireNamespace("dlnm", quietly = TRUE)) install.packages("dlnm")
install.packages("pdp")

library(openxlsx)
library(stats)
library(ggplot2)
library(reshape2)
library(magick)
library(dlnm)
library(gridExtra)
library(dplyr)

library(rlang)


source('999_2_regression_fun_v2.R')

##### if have run summary please, skip this part #####
visits_scores_wk <- read.csv('../data/unpivot_merged_data_raw_loss_visitors_s.csv')
subfolder_n <- c('/gam_model_loss_visitors/') #1to1_k10 1to1_k9 1to1_k11 loss_visitors 1tomore
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
library(tidyverse)
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

independent_vars_smooth_base = c("week", "log_borough_case_count")  #,'DEATH_COUNT_log' "borough_case_count"
independent_vars_linear_base = c("temporal_discounting_score","loss_aversion_score",'agency_score', 
                                 'stringency_index',"no_health_insurance_rate","no_vehicle_household_rate", "household_income", 
                                 "weighted_average_age")

#"percent_people_own_bachelor_degrees", 

#### there are zeros in the raw and if i want to log it, replace an eplison value
### which is close to the second smallest not too small.

visits_scores_wk <- visits_scores_wk %>%
  mutate(
    Grocery_and_Pharmacy = ifelse(Grocery_and_Pharmacy == 0, 0.01, Grocery_and_Pharmacy),
    Education = ifelse(Education == 0, 0.01, Education),
    Healthcare = ifelse(Healthcare == 0, 0.01, Healthcare)
  )

##### run the full gam model results ######
library(mgcv)
library(pdp)
library(broom)
library(lmtest)

### please change the k when do the model test!!
### when do the mapping check please set k = 10
run_gam_models(dependent_var_list, independent_vars_smooth_base, independent_vars_linear_base, visits_scores_wk, name_display, subfolder_n, my_k=10)

independent_vars <- c(independent_vars_smooth_base, independent_vars_linear_base,"log_y_lag1")

dependence_terms(dependent_var_list, independent_vars, visits_scores_wk, subfolder_n)
