#### 
# check the zipcode answers
###
# columns Q1.2 and Q10.12 asked the questions about the location
# compare the answers of the two columns, 
# create a new col = zipcodeflag, if they are the same the value = 1, else = 0
# get the number of rows where both Q1.2 and Q10.12 == NA and filter out them
na_count <- dat %>% 
            filter(is.na(Q1.2) & is.na(Q10.12)) %>%
            nrow()
# na_count
# [1] 0 Q1.2 is full completed

dat <- dat %>%
  mutate(zipcodeflag = ifelse(Q1.2 == Q10.12, 1, 0))


selected_data <- dat %>%
       filter(zipcodeflag == 0 | is.na(zipcodeflag)) %>%
       select(Q1.2, Q10.12, zipcodeflag)

write.csv(selected_data, file = "zipcode_flags.csv", row.names = FALSE)
