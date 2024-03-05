##### use the weekly sum visit ###
library(mgcv)
### read the cleaned data results
mask_scores_wk<- read.csv(file.choose())

### linear regression #####
library(stats)
lm_model <- lm(behaviour_mask ~ CASE_COUNT_log + DEATH_COUNT_log + borough_case_count_log + 
                 borough_death_count_log + Age + Risk_preference + days_from_start + 
                 score_p + gain_bias_p + loss_bias_p +  
                 rescale_avail_p + rescale_realistic_p , data = mask_scores_wk)
summary(lm_model)

library(mgcv)

gam_model <- gam(behaviour_mask_p ~ s(score_p,k = 5) + s(gain_bias_p,k = 4) + s(loss_bias_p,k = 5) 
                 + s(rescale_avail_p,k = 5) + s(rescale_realistic_p,k = 5) 
                 + s(borough_case_count,k = 5) + s(borough_death_count,k = 5)
                 + s(Age,k = 5) + s(days_from_start,k = 5), 
                 data = mask_scores_wk, family = poisson(link = "log"))


# Print the summary of the model
summary(gam_model)
# If you want to visualize the effects of the smooth terms
plot(gam_model, pages = 1, all.terms = TRUE)