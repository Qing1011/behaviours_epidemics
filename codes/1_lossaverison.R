### Calculate the loss aversion ###
# There are three questions about gain and three about loss respectively
# Then we can calculate the gain (0-3), loss (0-3) 
# The loss aversion is the add up of gain bias and loss bias
dat_lossgain <- dat %>%
  select(ResponseId, Q6.1:Q6.6, Q9.1_1:Q9.2_5, Q10.1_1:Q10.2_5)
# there is one id R_3CKphd7v4L4nZVw who did not finish the questions, 
# we need to delete it from the whole sample 
dat_lossgain <- na.omit(dat_lossgain)

dat_lossgain <- dat_lossgain %>%
  mutate(Q6.1 = recode(Q6.1, "An 80% chance of receiving $6,000" = 0, 
                       "A 100% chance of receiving $4,500" = 1)) %>%
  mutate(Q6.3 = recode(Q6.3, "A 20% chance of receiving $6,000" = 0, 
                       "A 25% chance of receiving $4,500" = 1)) %>%
  mutate(Q6.5 = recode(Q6.5, "A 45% chance of receiving $9,000" = 0, 
                       "A 90% chance of receiving $4,500" = 1)) %>%
  
  mutate(Q6.2 = recode(Q6.2, "An 80% chance of losing $6,000" = 1, 
                       "A 100% chance of losing $4,500" = 0)) %>%
  mutate(Q6.4 = recode(Q6.4, "A 20% chance of losing $6,000" = 1, 
                       "A 25% chance of losing $4,500" = 0)) %>%
  mutate(Q6.6 = recode(Q6.6, "A 45% chance of losing $9,000" = 1, 
                       "A 90% chance of losing $4,500" = 0))

dat_lossgain <- dat_lossgain %>%
  mutate(gain_bias = Q6.1 + Q6.3 + Q6.5) %>%
  mutate(loss_bias = Q6.2 + Q6.4 + Q6.6) %>%
  mutate(loss_aversion_scores = Q6.1 + Q6.3 + Q6.5+Q6.2 + Q6.4 + Q6.6)

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