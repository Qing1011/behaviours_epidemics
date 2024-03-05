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
                 CASE_COUNT_log + DEATH_COUNT_log + borough_case_count_log +borough_death_count_log + week, data = visits_scores_wk)
summary(lm_model)


######GAM############
if(!requireNamespace("mgcv", quietly = TRUE)) install.packages("mgcv")

library(mgcv)
# Prepare the response variable y
y <- log(visits_scores_wk$visits_weekly)
gam_linear_model <- gam(y ~ s(score_p) + s(gain_bias_p) + s(loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(CASE_COUNT_log) + s(DEATH_COUNT_log) + s(borough_case_count_log)+s(borough_death_count_log) + s(week),
                        data = visits_scores_wk)


summary(gam_linear_model)
plot(gam_linear_model, pages = 2, all.terms = TRUE)


gam_model <- gam(visits_weekly ~ s(score_p) + s(gain_bias_p) + s(loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(CASE_COUNT) + s(DEATH_COUNT) + s(borough_case_count)+s(borough_death_count) + s(week),
                 data = visits_scores_wk, family = poisson(link = "log"))

gam_model <- gam(visits_weekly ~ s(score_p) + te(gain_bias_p, loss_bias_p) + s(rescale_avail_p) + s(rescale_realistic_p) + s(CASE_COUNT) + s(DEATH_COUNT) + s(borough_case_count)+s(borough_death_count) + s(week),
                 data = visits_scores_wk,family = poisson(link = "log"))


gam_model <- gam(visits_weekly ~ s(score_p) + te(gain_bias_p, loss_bias_p) + te(rescale_avail_p, rescale_realistic_p) +  s(CASE_COUNT) + s(DEATH_COUNT) + s(borough_case_count)+s(borough_death_count) + s(week),
                 data = visits_scores_wk, family = poisson(link = "log"))

# Print the summary of the model
summary(gam_model)
# If you want to visualize the effects of the smooth terms
plot(gam_model, pages = 2, all.terms = TRUE)