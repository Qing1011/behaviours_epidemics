if(!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

library(mgcv)
library(stats)

visits_scores_wk <- read.csv(file.choose())

gam_model <- gam(visits_weekly ~ s(score) + te(gain_bias,loss_bias) + s(rescale_avail) + s(rescale_realistic) + s(CASE_COUNT_log) + s(DEATH_COUNT_log) + s(week),
                 data = visits_scores_wk,family = gaussian(link = "log"))

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