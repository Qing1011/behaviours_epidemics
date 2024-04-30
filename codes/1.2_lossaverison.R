### Calculate the loss aversion ###
# There are three questions about gain and three about loss respectively
# Then we can calculate the gain (0-3), loss (0-3) 
# The loss aversion is the add up of gain bias and loss bias
dat_lossgain <- dat %>%
  select(ResponseId, loss_gain_1:loss_loss_3, mask_march2020:mask_winter2020, indoor_march2020:indoor_winter2020 )
# there is one id R_3CKphd7v4L4nZVw who did not finish the questions, 
# we need to delete it from the whole sample 
dat_lossgain <- na.omit(dat_lossgain)

dat_lossgain <- dat_lossgain %>%
  mutate(loss_gain_1 = recode(loss_gain_1, "An 80% chance of receiving $6,000" = 0, 
                       "A 100% chance of receiving $4,500" = 1)) %>%
  mutate(loss_gain_2 = recode(loss_gain_2, "A 20% chance of receiving $6,000" = 0, 
                       "A 25% chance of receiving $4,500" = 1)) %>%
  mutate(loss_gain_3 = recode(loss_gain_3, "A 45% chance of receiving $9,000" = 0, 
                       "A 90% chance of receiving $4,500" = 1)) %>%
  
  mutate(loss_loss_1 = recode(loss_loss_1, "An 80% chance of losing $6,000" = 1, 
                       "A 100% chance of losing $4,500" = 0)) %>%
  mutate(loss_loss_2 = recode(loss_loss_2, "A 20% chance of losing $6,000" = 1, 
                       "A 25% chance of losing $4,500" = 0)) %>%
  mutate(loss_loss_3 = recode(loss_loss_3, "A 45% chance of losing $9,000" = 1, 
                       "A 90% chance of losing $4,500" = 0))

dat_lossgain <- dat_lossgain %>%
  mutate(gain_bias = loss_gain_1 + loss_gain_2 + loss_gain_3) %>%
  mutate(loss_bias = loss_loss_1 + loss_loss_2 + loss_loss_3) %>%
  mutate(loss_aversion_scores = loss_gain_1 + loss_gain_2 + loss_gain_3 + loss_loss_1 + loss_loss_2 + loss_loss_3)

#install.packages("ggplot2")
library(ggplot2)
ggplot(dat_lossgain, aes(x = gain_bias)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of gain_bias", x = "gain_bias", y = "Frequency")

ggplot(dat_lossgain, aes(x = loss_bias)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histogram of loss_bias", x = "loss_bias", y = "Frequency")

ggplot(dat_lossgain, aes(x = loss_aversion_scores)) + 
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Histogram of loss_aversion_scores", x = "loss_aversion_scores_bias", y = "Frequency")

write_csv(dat_lossgain,'./results/dat_lossgain.csv')