df <- visits_scores_wk %>%
  arrange(MODZCTA, week) %>%
  group_by(MODZCTA) %>%
  mutate(y_lag1 = lag(Art_and_Entertainment, 1), 
         y_lag2 = lag(Art_and_Entertainment, 2)) %>%
  ungroup()

formula_lag <-
  as.formula('Art_and_Entertainment ~ s(week, k = 30) + s(log_borough_case_count,k=6) + 
   + temporal_discounting_score + loss_aversion_score + 
  agency_score + stringency_index + no_health_insurance_rate + 
  no_vehicle_household_rate + household_income + percent_people_own_bachelor_degrees + 
  weighted_average_age + y_lag1 + y_lag2')
  
gam_model_lag <- gam(formula_lag, data=df, 
                             family = gaussian(link='log'), method = "ML",na.action = na.exclude)

df_with_r <- df %>%
  mutate( Art_r = residuals(gam_model_lag))

install.packages("broom")

# Load the broom package
library(broom)
library(lmtest)

dw_results <- df_with_r %>%
  group_by(MODZCTA) %>%                     # Group by location
  arrange(week) %>%                         # Ensure residuals are ordered by week
  do(tidy(dwtest(as.formula(paste0('Art_r', " ~ 1")), data = .))) %>%  # Apply Durbin-Watson test
  ungroup() 


formula_lag_spatial <-
  as.formula('Art_and_Entertainment ~ s(week, k = 6) + log_borough_case_count + 
  s(longitude, latitude,bs = "tp",k=5) + 
  temporal_discounting_score + loss_aversion_score + 
  agency_score + stringency_index + no_health_insurance_rate + 
  no_vehicle_household_rate + household_income + percent_people_own_bachelor_degrees + 
  weighted_average_age')

gam_ar1 <- gam(
  formula_lag_spatial,
  data = df,
  correlation = corAR1(form = ~ week | MODZCTA)
)

df_with_r <- df %>%
  mutate( Art_r = residuals(gam_ar1))

formula_original <-
  as.formula('Art_and_Entertainment ~ s(week, k = 30) + s(log_borough_case_count,k=6) + 
  temporal_discounting_score + loss_aversion_score + 
  agency_score + stringency_index + no_health_insurance_rate + 
  no_vehicle_household_rate + household_income + percent_people_own_bachelor_degrees + 
  weighted_average_age')

gamm_model_arma <- gam(
  formula_original,
  data = df, family = gaussian(link='log'),  # ARMA(2,1) structure
  method = "ML"
)

df_with_r <- df %>%
  mutate( Art_r = residuals(gamm_model_arma))


bootstrap_pd <- function(data, model, var, n = 100) {
  # Resample data
  boot_data <- data %>% sample_frac(1, replace = TRUE)
  
  # Refit the model on bootstrap sample
  gam_model_boot <- gam(formula(gam_model), data = boot_data)
  
  # Compute partial dependence on the bootstrap model
  partial(gam_model_boot, pred.var = var, grid.resolution = n)
}

n_boot <- 100

# Compute partial dependence for each bootstrap sample
bootstrap_results <- replicate(n_boot, bootstrap_pd(df_lag, gam_model, "temporal_discounting_score"), simplify = FALSE)

# Combine results
pd_combined <- bind_rows(bootstrap_results, .id = "bootstrap")


pd_summary <- pd_combined %>%
  group_by(temporal_discounting_score) %>%
  summarise(mean_pd = mean(yhat), 
            lower_ci = quantile(yhat, 0.025), 
            upper_ci = quantile(yhat, 0.975))

ggplot(pd_summary, aes(x = temporal_discounting_score, y = mean_pd)) +
  geom_line(color = "blue") +  # Plot the mean partial dependence line
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +  # Plot CI as a shaded area
  labs(title = "Partial Dependence of temporal_discounting_score with Confidence Intervals",
       x = "temporal_discounting_score", y = "Partial Dependence") +
  theme_minimal()



