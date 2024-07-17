install.packages("influential")


library(mgcv)
library(influential)

# Example data
set.seed(123)
visits_scores_wk <- data.frame(
  response = rlnorm(100, meanlog = 3, sdlog = 1),  # Log-normal response variable
  skewed_predictor1 = rlnorm(100, meanlog = 2, sdlog = 0.5),
  skewed_predictor2 = rlnorm(100, meanlog = 1, sdlog = 0.3),
  linear_predictor = rnorm(100),
  week = 1:100,
  MODZCTA = sample(10001:10005, 100, replace = TRUE)
)

# Log-transform skewed predictors
visits_scores_wk$log_skewed_predictor1 <- log(visits_scores_wk$skewed_predictor1)
visits_scores_wk$log_skewed_predictor2 <- log(visits_scores_wk$skewed_predictor2)

# Define the formula
formula <- response ~ s(log_skewed_predictor1, k = 6) + s(log_skewed_predictor2, k = 6) + linear_predictor

# Fit the GAM model with Gamma family and log link
gam_model <- gam(formula, data = visits_scores_wk, family = Gamma(link = "log"), method = "ML")

# Summary of the model
summary(gam_model)

# Apply influence measures
influence_measures <- influence.measures(gam_model)

# Print summary of influence measures
summary(influence_measures)

# Plotting influence measures
par(mfrow = c(2, 2))
plot(influence_measures, which = 1:4)  # Plot different influence measures

# Identifying influential observations
cutoff <- 2 * mean(influence_measures$infmat$cookd)
influential_obs <- which(influence_measures$infmat$cookd > cutoff)
print(influential_obs)

# Robustness check by refitting the model without influential observations
visits_scores_wk_robust <- visits_scores_wk[-influential_obs, ]
gam_model_robust <- gam(formula, data = visits_scores_wk_robust, family = Gamma(link = "log"), method = "ML")

# Compare models
summary(gam_model)
summary(gam_model_robust)

# Plotting model diagnostics for robust model
par(mfrow = c(2, 2))
gam.check(gam_model_robust)
