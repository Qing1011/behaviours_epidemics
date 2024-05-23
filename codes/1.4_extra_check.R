#### 
# check the zipcode answers
###
# columns Q1.2 and Q10.12 asked the questions about the location
# compare the answers of the two columns, 
# create a new col = zipcodeflag, if they are the same the value = 1, else = 0
# get the number of rows where both Q1.2 and Q10.12 == NA and filter out them
### Q1.2 zipcode Q10.12 is the work_zipcode  
na_count <- dat %>% 
            filter(is.na(zipcode) & is.na(work_zipcode)) %>%
            nrow()
# na_count
# [1] 0 Q1.2 is full completed

dat <- dat %>%
  mutate(zipcodeflag = ifelse(zipcode == work_zipcode, 1, 0))


selected_data <- dat %>%
       filter(zipcodeflag == 0 | is.na(zipcodeflag)) %>%
       select(zipcode, work_zipcode, zipcodeflag)

write.csv(selected_data, file = "zipcode_flags.csv", row.names = FALSE)
#### count the responses of each zipcode Q1.2
value_counts <- table(dat$zipcode)
# Convert the table to a dataframe
value_counts_df <- as.data.frame(value_counts)
# Rename the columns for clarity
colnames(value_counts_df) <- c("Value", "Count")
# Sort the dataframe by Count in descending order
sorted_value_counts_df <- value_counts_df[order(-value_counts_df$Count), ]
# Print the sorted dataframe
sorted_value_counts_df <- sorted_value_counts_df %>%
  mutate(zip_code = ifelse(is.na(Value), NA, substr(as.character(Value), 1, 5)))
write.csv(sorted_value_counts_df, file = "../results/zipcode_counts.csv", row.names = FALSE)
