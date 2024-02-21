####### Calculate the agency scores and validate the measure ##################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Create a list of the required packages:
list.of.packages <- c(
  "tidyverse",#wrangle data
  "lavaan", #fit CFA
  "userfriendlyscience", #check scale reliability
  "psychTools"
  )
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] #check for any uninstalled packages
if(length(new.packages)) install.packages(new.packages) #install any missing packages (requires internet access)
lapply(list.of.packages, require, character.only = TRUE) #library all required packages

#devtools::install_github("matherion/userfriendlyscience")

dat <- read.csv("agency_data.csv")

rm(new.packages, list.of.packages)
####### 1.0 Data Cleaning #########################################################
dat_agency <- dat %>%
  select(ResponseId, agency_pub_trans_avail:agency_remote_realistic)
# there is one id R_3CKphd7v4L4nZVw who did not finish the questions

na_count <- sapply(dat_agency, function(x) sum(is.na(x)))
# Convert the counts to a data frame for plotting
na_count_df <- data.frame(column = names(na_count)[2:19], na_count = na_count[2:19])
na_count_df$column <- factor(na_count_df$column, levels = names(na_count))
# Create the bar plot
ggplot(na_count_df, aes(x = column, y = na_count)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(x = "Column", y = "Number of NAs", title = "NA Count in Each Column")

# combine Q8.9 and Q8.10
summary <- dat_agency %>%
  mutate(Category = case_when(
    !is.na(agency_IP_safe_avail) & agency_IP_safe_avail != "" & !is.na(agency_remote_avail) & agency_remote_avail != "" ~ "Answers in both",
    (is.na(agency_IP_safe_avail) | agency_IP_safe_avail == "") & (is.na(agency_remote_avail) | agency_remote_avail == "") ~ "No answers",
    TRUE ~ "Answer in one only"
  )) %>%
  count(Category)
print(summary)

# we need to delete these questions from the whole sample 
dat_agency <- dat %>%
  select(ResponseId, agency_pub_trans_avail:agency_online_realistic)
dat_agency <- na.omit(dat_agency) #lose a final 4 obs

rm(na_count_df, summary, na_count, dat)
####### 1.2 Scoring the Agency answers ###########################################
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

###### 3.0 CFA #################################################################
## Model Specification
standardized_data <- scale(dat_agency_scores[, 2:15],center = TRUE, scale = TRUE)
# Combine all parts into one model formula string for CFA
model_formula <- 'F =~ agency_pub_trans_avail + agency_pub_trans_realistic + 
agency_uber_avail + agency_uber_realistic + agency_delivery_avail + 
agency_delivery_realistic + agency_grocery_avail + agency_grocery_realistic + 
agency_pharma_avail + agency_pharma_realistic + agency_docs_avail + 
agency_docs_realistic + agency_online_avail + agency_online_realistic'

cat(model_formula) #Display the generated model formula

agency.cfa <- cfa(model_formula, data = standardized_data, std.lv = T)

agency.cfa.measures <- fitMeasures(agency.cfa, c("chisq", "df", "pvalue", 
                      "rmsea", "srmr", "cfi", "tli")); agency.cfa.measures
agency.cfa.measures <- data.frame(agency.cfa.measures)

reliability <- scaleReliability(as.data.frame(standardized_data))
measures <- data.frame(
  chisq = agency.cfa.measures[1,1],
  df = agency.cfa.measures[2,1],
  pvalue = agency.cfa.measures[3,1],
  rmsea = agency.cfa.measures[4,1],
  srmr = agency.cfa.measures[5,1],
  cfi = agency.cfa.measures[6,1],
  tli = agency.cfa.measures[7,1],
  alpha = reliability$output$cronbach.alpha[1],
  alphaLL = reliability$output$alpha.ci[1],
  alphaUL = reliability$output$alpha.ci[2],
  omega = reliability$output$omega[1],
  omegaUL =  reliability$output$omega.ci[1],
  omegaLL = reliability$output$omega.ci[2])

rm(reliability, agency.cfa.measures)

#1 factor model is relaible (alpha and omega are good = >0.85)
# SRMR is acceptable - but RMSEA is unacceptable
# CFI and TLI are unacceptable

#consider the modification indices, freeing up covariances to imrpve fit of the model
# modification indices
mi_metric = modificationIndices(agency.cfa,
                                minimum.value = 10, sort = T);mi_metric

#these modification indices suggest that allowing covariances between the realistic and available
# items within the same subdomains would improve the model fit. 




