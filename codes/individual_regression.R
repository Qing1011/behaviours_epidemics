
df <- read.csv(file.choose())

df$agency_rescaled <- df$rescale_avail + df$rescale_realistic


score_dependent <- c('score', 'agency_rescaled','agency')
df_score <- df[score_dependent]

demographic_independent <- c('alloc_debt', 'alloc_spend', 'alloc_save',
                              'alloc_invest', 'Age', 'Risk_preference', 
                              'expectation_num', 'internet_num',
                              'financial_2020_num', 'financial_change_num',
                              'credit_debt_num', 'gender_num', 'education_num', 'employment_num',
                              'race_num')

are_all_numeric <- all(sapply(df[score_dependent], is.numeric))

numeric_checks <- sapply(df[demographic_independent], is.numeric)
# Identify columns that are not numeric
non_numeric_cols <- names(df)[!numeric_checks]
# Print the names of non-numeric columns
print(non_numeric_cols)

dependent_var <- 'score'
linear_parts <- paste(demographic_independent, collapse = " + ")
formula <- as.formula(paste(dependent_var, "~", linear_parts))

# Fit the model
lm_model <- lm(formula, data = df)
# Capture the model summary
sum_my_model <- summary(lm_model)


install.packages("nnet")
# Load the nnet package
library(nnet)
quartiles <- quantile(df$score, probs = c(0, 0.25, 0.5, 0.75, 1))
quartiles <- c(-Inf, 7.9, 11.9, 30, Inf)
df$class <- cut(df$score, breaks = quartiles, include.lowest = TRUE, labels = FALSE)
formula <- as.formula(paste('class', "~", linear_parts))
multi_logitic <- multinom(formula, data = df)

summary(multi_logitic)

predictions <- predict(multi_logitic, newdata = df, type = "class")

# Creating a confusion matrix
table(Predicted = predictions, Actual = df$class)

demographic_independent_smooth <- c('alloc_debt', 'alloc_spend', 'alloc_save',
                             'alloc_invest', 'Age')
demographic_independent_factor <- c('Risk_preference', 
                                    'expectation_num', 'internet_num',
                                    'financial_2020_num', 'financial_change_num',
                                    'credit_debt_num', 'gender_num', 'education_num', 'employment_num',
                                    'race_num')

#'agency_rescaled', score, loss_aversion_scores
dependent_var <- 'agency_rescaled' 
smooth_parts <- paste("s(", demographic_independent_smooth, ")", collapse = " + ")
factor_parts <- paste("factor(", demographic_independent_factor, ")", collapse = " + ")
formula <- as.formula(paste(dependent_var, "~", factor_parts, "+", smooth_parts))
gam_model <- gam(formula, data = df)
summary(gam_model)

library(dplyr)
library(ggplot2)

result <- df %>%
  group_by(zipcode) %>%
  summarize(
    count = n())%>%
  arrange(desc(count)) 

first_21_zipcodes <- result$zipcode[1:21]

# Filter the original data frame to keep only rows with those zipcodes
filtered_df <- df %>%
  filter(zipcode %in% first_21_zipcodes) %>%
  mutate(zipcode = factor(zipcode, levels = first_21_zipcodes)) 

filtered_df <- filtered_df %>%
  group_by(zipcode) %>%
  mutate(mean_agency_rescaled = mean(agency_rescaled, na.rm = TRUE),
         median_agency_rescaled = median(agency_rescaled, na.rm = TRUE)) %>%
  ungroup()

stats_df <- filtered_df %>%
  group_by(zipcode) %>%
  summarise(mean_agency_rescaled = mean(agency_rescaled, na.rm = TRUE),
            median_agency_rescaled = median(agency_rescaled, na.rm = TRUE),
            std_agency_rescaled = sd(agency_rescaled, na.rm = TRUE),
           )


ggplot(filtered_df, aes(x = factor(zipcode), y = agency_rescaled)) +
  geom_violin(fill = "blue", color = "black") +
  geom_point(data = stats_df, aes(x = zipcode, y = mean_agency_rescaled), color = "red", size = 3, show.legend = TRUE) +
  geom_point(data = stats_df, aes(x = zipcode, y = median_agency_rescaled), color = "purple", size = 3, show.legend = TRUE) +
  geom_errorbar(data = stats_df, aes(x = zipcode, ymin = mean_agency_rescaled-std_agency_rescaled, ymax = mean_agency_rescaled-std_agency_rescaled), width = 0.2, color = "orange", size = 1, show.legend = TRUE) +
  scale_color_identity() +
  theme_minimal() +
  labs(title = "Violin Plot of agency_rescaleds Across Zipcodes",
       x = "Zipcode",
       y = "Loss aversion scores") +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) + # Rotating x-axis labels
  theme(legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(size = 5), title = "", 
                              labels = c("Mean", "Median","STD")))





library(ggplot2)
library(dplyr)
library(rlang)


createViolinPlotWithStd <- function(df, colname) {
  # Dynamically reference the column
  col_sym = sym(colname)
  
  # Calculate statistics
  stats_df <- df %>%
    group_by(zipcode) %>%
    summarise(mean_value = mean(!!col_sym, na.rm = TRUE),
              median_value = median(!!col_sym, na.rm = TRUE),
              std_value = sd(!!col_sym, na.rm = TRUE),
              ymin = mean(!!col_sym, na.rm = TRUE) - sd(!!col_sym, na.rm = TRUE),
              ymax = mean(!!col_sym, na.rm = TRUE) + sd(!!col_sym, na.rm = TRUE)) 
  
  # Generate the violin plot
  ggplot(df, aes(x = factor(zipcode), y = !!col_sym)) +
    geom_violin(fill = "blue", color = "black") +
    geom_point(data = stats_df, aes(x = zipcode, y = mean_value), color = "red", size = 3, show.legend = TRUE) +
    geom_point(data = stats_df, aes(x = zipcode, y = median_value), color = "purple", size = 3, show.legend = TRUE) +
    geom_errorbar(data = stats_df, aes(x = zipcode, ymin = ymin, ymax = ymax), 
                  width = 0.2, color = "orange", size = 1, show.legend = TRUE) +
    scale_color_identity() +
    theme_minimal() +
    labs(title = paste("Violin Plot with Std of", colname, "Across Zipcodes"),
         x = "Zipcode",
         y = colname) +
    theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
    guides(color = guide_legend(override.aes = list(size = 5), title = "", 
                                labels = c("Mean", "Median", "STD")))
}