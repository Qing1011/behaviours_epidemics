if(!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")
if(!requireNamespace("reshape2", quietly = TRUE)) install.packages("reshape2")

library(mgcv)
library(stats)

visits_scores_wk <- read.csv(file.choose())

gam_model <- gam(visits_weekly ~ s(score) + s(gain_loss_bias) + te(rescale_avail,rescale_realistic) + s(CASE_COUNT) + s(DEATH_COUNT) + s(week),
                 data = visits_scores_wk,family = gaussian())


gam_model <- gam(visits_weekly ~ s(score) + s(gain_loss_bias) + te(rescale_avail,rescale_realistic) + 
                   s(CASE_COUNT) + s(DEATH_COUNT) + s(week) + s(Age) + s(Risk_preference) + 
                   s(POPULATION) + s(RESTAURANT_COUNT) + s(GROCERY_COUNT) + s(EDUCATION_COUNT) + s(ENTERTAINMENT_COUNT) + s(HEALTHCARE_COUNT) 
                   + s(RETAIL_COUNT) + s(OTHER_COUNT) + s(GROCERY_VISIT_PERCENT) + s(HOUSEHOLD_SIZE) + s(HOUSEHOLD_INCOME) + 
                   s(BLACK) + s(HISPANIC) + s(AGE65_PLUS) + s(BACHELOR_S) + s(NO_HEALTH_INSURANCE),
                 data = visits_scores_wk,family = gaussian())

gam_model <- gam(visits_weekly ~ s(score) + s(gain_loss_bias) + te(rescale_avail,rescale_realistic) + 
                   s(CASE_COUNT) + s(DEATH_COUNT) + s(week) + s(Age) + s(Risk_preference) + 
                   s(POPULATION) + s(RESTAURANT_COUNT) + s(GROCERY_COUNT) + s(EDUCATION_COUNT) + s(ENTERTAINMENT_COUNT) + s(HEALTHCARE_COUNT) 
                 + s(RETAIL_COUNT) + s(OTHER_COUNT) + s(GROCERY_VISIT_PERCENT) + s(HOUSEHOLD_SIZE) + s(HOUSEHOLD_INCOME) + 
                   s(BLACK) + s(HISPANIC) + s(AGE65_PLUS) + s(BACHELOR_S) + s(NO_HEALTH_INSURANCE),
                 data = visits_scores_wk,family = gaussian())

summary(gam_model)
# If you want to visualize the effects of the smooth terms
plot(gam_model, pages = 2, all.terms = TRUE)
# Assuming you have already fitted your GAM model (gam_model)

# Step 1: Obtain predicted values
predictions <- predict(gam_model, type = "response")

# Step 2: Create a plot
plot(visits_scores_wk$visits_weekly, predictions, xlab = "Actual Values", ylab = "Predicted Values", main = "Actual vs Predicted")

# Add a reference line for perfect prediction
abline(a = 0, b = 1, col = "red")

# add the predicted values
visits_scores_wk$predicted <- predictions


library(ggplot2)
df <- visits_scores_wk[,c('week','predicted','visits_weekly','zip_char')]

# First, melt your dataframe to long format for easier plotting with ggplot2
df_long <- reshape2::melt(df, id.vars = c("week", "zip_char"), variable.name = "type", value.name = "value")

# Plot
ggplot(df_long, aes(x = week, y = value, color = type)) + 
  geom_line() + 
  facet_wrap(~ zip_char, scales = "free_y") + 
  theme_minimal() + 
  labs(title = "True vs Predicted Values by Zipcode",
       x = "Week",
       y = "Value") + 
  scale_color_manual(values = c("visits_weekly" = "blue", "predicted" = "red"))
