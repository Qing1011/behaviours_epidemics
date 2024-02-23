##### use the weekly sum visit ###
library(mgcv)
### read the cleaned data results
visits_scores_wk <- read.csv(file.choose())

visits_scores_wk_small <- visits_scores_wk[visits_scores_wk$week %in% 1:8, ]

x_cols <- c('score_p', 'gain_bias_p', 'loss_bias_p', 'rescale_avail_p', 'rescale_realistic_p',
            'cumucaserate', 'case', 'test')

########## linear regression with some log ###
library(stats)
y <- log(abs(visits_scores_wk$visits_weekly))
lm_model <- lm(y ~ score_p + gain_bias_p + loss_bias_p + rescale_avail_p + rescale_realistic_p + 
                 logcumucaserate + logcase + logtest, data = visits_scores_wk)
summary(lm_model)


######GAM############
if(!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

library(mgcv)
# Prepare the response variable y
y <- log(abs(visits_scores_wk$visits_weekly))
gam_model <- gam(y ~ s(score_p) + te(gain_bias_p, loss_bias_p) + te(rescale_avail_p, rescale_realistic_p) + s(cumucaserate) + s(case) + s(test),
                 data = visits_scores_wk)



gam_linear_model <- gam(y ~ s(score_p) + s(gain_bias_p) + s(loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(logcumucaserate) + s(logcase) + s(logtest),
                        data = visits_scores_wk)
summary(gam_linear_model)

gam_model <- gam(visits_weekly ~ s(score_p) + te(gain_bias_p, loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(logcumucaserate) + s(logcase) + s(logtest),
                 data = visits_scores_wk,family = poisson(link = "log"))


gam_model <- gam(visits_weekly ~ s(score_p) + te(gain_bias_p, loss_bias_p) + te(rescale_avail_p, rescale_realistic_p) + s(cumucaserate) + s(case) + s(test),
                 data = visits_scores_wk_small, family = poisson(link = "log"))

# Print the summary of the model
summary(gam_model)
# If you want to visualize the effects of the smooth terms
plot(gam_model, pages = 1, all.terms = TRUE)