if(!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

library(mgcv)
### read the cleaned data results
visits_scores_wk <- read.csv(file.choose())

x_cols <- c('score_p', 'gain_bias_p', 'loss_bias_p', 'rescale_avail_p', 'rescale_realistic_p',
            'cumucaserate', 'case', 'test')

########## linear regression with some log ###
library(stats)
y <- log(abs(visits_scores_wk$visits_weekly_diff))
lm_model <- lm(visits_weekly_diff ~ score_p + gain_bias_p + loss_bias_p + rescale_avail_p + rescale_realistic_p + 
              logcumucaserate + logcase + logtest, data = visits_scores_wk)
summary(lm_model)
# Assuming 'visits_scores_wk' is a data frame in R
X <- visits_scores_wk[x_cols]


######GAM############
# Prepare the response variable y
y <- log(abs(visits_scores_wk$visits_weekly_diff))
gam_model <- gam(y ~ s(score_p) + te(gain_bias_p, loss_bias_p) + te(rescale_avail_p, rescale_realistic_p) + s(cumucaserate) + s(case) + s(test),
          data = visits_scores_wk)

y <- abs(visits_scores_wk$visits_weekly_diff)

gam_linear_model <- gam(y ~ s(score_p) + s(gain_bias_p) + s(loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(logcumucaserate) + s(logcase) + s(logtest),
                 data = visits_scores_wk)
summary(gam_linear_model)

gam_model <- gam(y ~ s(score_p) + s(gain_bias_p) + s(loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(logcumucaserate) + s(logcase) + s(logtest),
                 data = visits_scores_wk,family = poisson(link = "log"))

gam_model <- gam(y ~ s(score_p) + te(gain_bias_p, loss_bias_p) + te(rescale_avail_p, rescale_realistic_p) + s(cumucaserate) + s(case) + s(test),
                 data = visits_scores_wk, family = poisson(link = "log"))

# Print the summary of the model
summary(gam_model)
# If you want to visualize the effects of the smooth terms
plot(gam_model, pages = 1, all.terms = TRUE)


##########CAR############
visits_scores_wk['y'] <- log(abs(visits_scores_wk$visits_weekly_diff))
visits_scores_wk_score <- subset(visits_scores_wk, score_p > 11)
lm_score <- lm(y ~ gain_bias_p + loss_bias_p + rescale_avail_p + rescale_realistic_p + logcumucaserate + logcase + logtest, data = visits_scores_wk_score)
summary(lm_score)
