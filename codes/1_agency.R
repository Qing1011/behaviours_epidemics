### Calculate the agency ###
# There are 9 questions about gain and three about loss respectively
# Then we can calculate the gain (0-3), loss (0-3) 
# The loss aversion is the add up of gain bias and loss bias
######## select the agency data
dat_agency <- dat %>%
  select(ResponseId, agency_pub_trans_avail:agency_remote_realistic)
# there is one id R_3CKphd7v4L4nZVw who did not finish the questions,
# count the NA in each columns

na_count <- sapply(dat_agency, function(x) sum(is.na(x)))
# Convert the counts to a data frame for plotting
na_count_df <- data.frame(column = names(na_count)[2:19], na_count = na_count[2:19])
na_count_df$column <- factor(na_count_df$column, levels = names(na_count))
# Create the bar plot
ggplot(na_count_df, aes(x = column, y = na_count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Column", y = "Number of NAs", title = "NA Count in Each Column")
########combine Q8.9 and Q8.10
library(dplyr)

# Assuming df is your dataframe, and A and B are your column names
summary <- dat_agency %>%
  mutate(Category = case_when(
    !is.na(agency_IP_safe_avail) & agency_IP_safe_avail != "" & !is.na(agency_remote_avail) & agency_remote_avail != "" ~ "Answers in both",
    (is.na(agency_IP_safe_avail) | agency_IP_safe_avail == "") & (is.na(agency_remote_avail) | agency_remote_avail == "") ~ "No answers",
    TRUE ~ "Answer in one only"
  )) %>%
  count(Category)

# Print the summary
print(summary)

#dat_agency <- dat_agency %>% 
 # mutate(Q8.9and10_1 = ifelse(!is.na(Q8.9_1) & Q8.9_1 != "", Q8.9_1, Q8.10_1))

# we need to delete it from the whole sample 
dat_agency <- dat %>%
  select(ResponseId, agency_pub_trans_avail:agency_online_realistic)
dat_agency <- na.omit(dat_agency)
####### Scoring the answers ######
# Duplicate the dataframe
dat_agency_scores <- dat_agency
# Define a function to replace responses with numerical values
replace_responses <- function(x) {
  if (is.character(x) || is.factor(x)) {
    #x <- as.character(x)  # Convert factors to character if needed
    x[x == "Do not know"] <- 0
    x[x == "Strongly disagree"] <- -2
    x[x == "Disagree"] <- -1
    x[x == "Agree"] <- 1
    x[x == "Strongly agree"] <- 2
  }
  return(x)
}

# Apply the function to each column in the new dataframe
dat_agency_scores[2:15] <- data.frame(lapply(dat_agency_scores[2:15], replace_responses))

# Convert the replaced values back to numeric if needed
dat_agency_scores[2:15] <- data.frame(lapply(dat_agency_scores[2:15], 
                                             function(x) {as.numeric(x)} ))
##########PCA###########################
# to standarise the data in order to evaluate 
res_pca <- prcomp(dat_agency_scores[, 2:15], scale. = TRUE)

# Calculate eigenvalues from the PCA result
eigenvalues <- res_pca$sdev^2

# Number of principal components
pc_numbers <- 1:length(eigenvalues)

# Create the Scree plot
plot(pc_numbers, eigenvalues, type = "b", pch = 19, xlab = "Principal Component",
     ylab = "Eigenvalue", main = "Scree Plot")

# Optional: Add a line to mark the 'elbow'
abline(h = 1, col = "red", lty = 2)

## factor analysis 
library(factoextra)
library(psych)
standardized_data <- scale(dat_agency_scores[, 2:15],center = TRUE, scale = TRUE)
fa_result <- fa(standardized_data)
#fviz_eig(fa_result)
eigenvalues <- fa_result$values 
######################
##### write myself #####
######################
# Standardize the data
standardized_data <- scale(dat_agency_scores[, 2:15],center = TRUE, scale = TRUE)
# Compute the covariance (correlation) matrix
cov_matrix <- cor(data)(standardized_data)
# Perform eigen decomposition
eigen_decomp <- eigen(cov_matrix)
nfactors <- length(eigen_decomp$values)
  # Extract the factor loadings for the specified number of factors
loadings <- eigen_decomp$vectors[, 1:nfactors]
  # Extract the variance explained by the specified number of factors
eigenvalues <- eigen_decomp$values[1:nfactors]
######################
######################
plot(eigenvalues, type = "b", pch = 19, xlab = "Factor", ylab = "Eigenvalue",
      main = "Scree Plot")
abline(h = 1, col = "red", lty = 2) 

eigen_df <- data.frame(Factor = 1:length(eigenvalues), Eigenvalue = eigenvalues)

# Plot using ggplot2
ggplot(eigen_df, aes(x = Factor, y = Eigenvalue)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  labs(title = "Scree Plot", x = "Factor", y = "Eigenvalue")

library(ggplot2)
#calculate total variance explained by each principal component
var_explained = res_pca$sdev^2 / sum(res_pca$sdev^2)

qplot(c(1:14), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)
##############confirmation analysis######
## Model Specification
# Generate the formula parts for each variable
selected_colnames <- colnames(dat_agency_scores)[2:15]
formula_parts <- paste(selected_colnames, collapse = " + ")

# Combine all parts into one model formula string for CFA
model_formula <- 'F =~ agency_pub_trans_avail + agency_pub_trans_realistic + agency_uber_avail + agency_uber_realistic + agency_delivery_avail + agency_delivery_realistic + agency_grocery_avail + agency_grocery_realistic + agency_pharma_avail + agency_pharma_realistic + agency_docs_avail + agency_docs_realistic + agency_online_avail + agency_online_realistic 
                  F ~~ 1*F
                  agency_pub_trans_avail ~ 1'

# Display the generated model formula
cat(model_formula)

## Running Analysis
library(lavaan)
my.cfa <- cfa(model_formula, data = standardized_data, std.lv = T)

### Model fit
fitMeasures(my.cfa, c("chisq", "df", "pvalue", 
                      "rmsea", "srmr", "gfi", "cfi", "tli")) %>% 
  as.data.frame() %>% t() %>% as.data.frame() %>% round(3)

#### try two factors ####
m4b <- 'f1 =~ agency_pub_trans_avail +  agency_uber_avail + agency_delivery_avail + agency_grocery_avail + agency_pharma_avail + agency_docs_avail + agency_online_avail
        f2 =~ agency_pub_trans_realistic + agency_uber_realistic + + agency_delivery_realistic + agency_grocery_realistic + agency_pharma_realistic + agency_docs_realistic + agency_online_realistic
        f3 =~ 1*f1 + 1*f2
        f3 ~~ f3' 
twofac7items_b <- cfa(m4b, data=dat_agency_scores[2:15],std.lv=TRUE)
summary(twofac7items_b,fit.measures=TRUE,standardized=TRUE)

fitMeasures(twofac7items_b, c("chisq", "df", "pvalue", 
                      "rmsea", "srmr", "gfi", "cfi", "tli")) %>% 
  as.data.frame() %>% t() %>% as.data.frame() %>% round(3)


########some two dimensional plots#####
# Define the function
plot_scatter <- function(base_name, width = 8, height = 6) {
  # Construct the column names
  col1 <- paste0(base_name, "_avail")
  col2 <- paste0(base_name, "_realistic")
  
  # Calculate the count for each (x, y) pair
  point_counts <- dat_agency_scores %>%
    group_by(!!sym(col1), !!sym(col2)) %>%
    summarise(count = n(), .groups = 'drop') %>%
    mutate(sum_xy = !!sym(col1) + !!sym(col2))
  
  # Create the scatter plot
  ggplot(point_counts, aes_string(x = col1, y = col2, color = "count", size = "count")) +
    geom_point(alpha = 0.7) +
    geom_text(aes(label = sum_xy), vjust = -0.5, size = 4, color = "black") +  # Add text labels for the sum
    scale_color_gradient(low = "blue", high = "red") +
    scale_size(range = c(1, 20)) +
    theme_minimal() +
    labs(x = "Availability", y = "Realisticity", color = "Count",
         title = paste("Scatter Plot of combinations for", base_name))
}

plot_scatter("agency_online")
######## add up available realistic #####
mutate_sums <- function(df) {
  # Generate column names

  cols_1 <- paste0("agency_", c("pub_trans","uber","delivery","grocery","pharma","docs","online"), "_avail")
  cols_2 <- paste0("agency_", c("pub_trans","uber","delivery","grocery","pharma","docs","online"), "_realistic")
  
  # Check if the columns exist in the dataframe
  existing_cols_1 <- cols_1[cols_1 %in% names(df)]
  existing_cols_2 <- cols_2[cols_2 %in% names(df)]
  
  # Mutate new columns
  df <- df %>%
    mutate(
      avaliability = rowSums(select(., all_of(existing_cols_1)), na.rm = TRUE),
      realisticity = rowSums(select(., all_of(existing_cols_2)), na.rm = TRUE),
      agency = avaliability + realisticity
    )
  
  return(df)
}

dat_agency_scores <- mutate_sums(dat_agency_scores)

summary(dat_agency_scores$agency)

#############item response theory (IRT) models ##########
library(mirt)
library(lavaan)
library(e1071)
library(psych)
library(rmarkdown) 
library(knitr) 
# Assumption: Unidimensionality
desc.stat = matrix(nrow = 14, ncol = 8) %>% as.data.frame()
colnames(desc.stat) <- c("Mean", "SD", "Skewness", "Strongly disagree", "Disagree","Do not know", "Agree","Strongly agree")
rownames(desc.stat) <- colnames(dat_agency_scores[2:15])
#### Assigning value to the Descriptive Statistic Table
##### Mean
desc.stat$Mean = apply(dat_agency_scores[2:15], 2, mean) %>% round(3)

##### SD
desc.stat$SD = apply(dat_agency_scores[2:15], 2, sd) %>% round(3)

##### Skewness
desc.stat$Skewness = apply(dat_agency_scores[2:15], 2, skewness) %>% round(3)

##### Options Proportion
#desc.stat[1,4:8] = round(table(dat_agency_scores[2])/sum(table(dat_agency_scores[2])),3)

for (i in 1:14) {
  # Calculate the proportions for each level of the factor in column i+1 of dat_agency_scores
  proportions <- round(table(dat_agency_scores[[i+1]]) / sum(table(dat_agency_scores[[i+1]])), 3)
  # Ensure that the length of proportions matches the number of columns being assigned
  # This is important if the number of levels in the factor is less than 5
  proportions <- c(proportions, rep(NA, 5 - length(proportions)))
  # Assign the values to the corresponding row in desc.stat
  desc.stat[i, 4:8] <- proportions
}

write.csv(desc.stat, "agency_stat.csv", row.names = TRUE)
###correlations####
library(reshape2)
corr_matrix <- cor(dat_agency_scores[2:15]) %>% round(3)
corr_melted <- melt(corr_matrix)
ggplot(corr_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0.5, limit = c(0, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "", y = "", title = "Heatmap of Correlation Matrix")

##### Fit ######
my.grm = mirt(dat_agency_scores[2:15], 1, itemtype = "graded", verbose = F)
( res.grm = M2(my.grm, type = "C2") %>% round(3) %>% as.data.frame() %>% 
  paged_table() )

coef(my.grm, IRT = T, simplify = T)$items %>% round(3) %>% 
  as.data.frame() %>% paged_table()

#### No Correction
itemfit(x = my.grm, fit_stats = "S_X2", which.items = 1:14)
#### plotting pdf, cdf, Item information function (IIF)
library(ltm)
library(gridExtra)
library(gtable)
library(latticeExtra)


generate_plots <- function(type) {
  if (!type %in% c("trace", "threshold", "infoSE")) {
    stop("Invalid type. Choose from 'trace', 'threshold', 'infoSE'")
  }
  
  plot_list <- list()
  for (i in 1:14) {
    # Create plots for each item based on the specified type
    plot_list[[i]] <- update(itemplot(my.grm, item = i, type = type), 
                             main = colnames(dat_agency_scores)[i+1])
  }
  
  # Arrange plots in a grid
  g <- arrangeGrob(grobs = plot_list, ncol = 2, nrow = 7)
  
  # Save to a file with increased dimensions
  ggsave(paste0("agency_", type, "_plots.pdf"), g, width = 16, height = 24)
}

generate_plots("infoSE")
generate_plots("trace")
generate_plots("threshold")