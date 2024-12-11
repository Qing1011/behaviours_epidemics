########################
# 0. Utility functions
########################
# Set the working directory to the folder that this file is in:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")

source("999_1_auxiliary_functions.R") #this is a reduced file with only the required functions
options(digits = 3)
options(scipen = 99)
library(dplyr)
########################
# 1. Data cleaning
########################
dat <- read_csv("../data/raw_survey/RAISE+Base+Survey_January+24,+2024_13.50.csv") #load in the data
dat <- dat[-c(1,2), -c(1:3, 9:12, 94:101) ] #remove the first two rows and several unneeded columns

#initial data quality checks
dat <- dat %>%
  mutate(Age = 2023 - as.numeric(Q10.7),
         duration = as.numeric(`Duration (in seconds)`),
         Progress = as.numeric(Progress)) %>%
  #basics: consent, passed the attention check, and completed most of the questions, remove if not
  filter(
    (Q1.1 == "CONSENT" & Q4.1 == "PASS" & Progress >= 90),  #down to N = 1443
  #advanced: minimum and maximum response times and age less than 100
    duration < median(duration) + mad(duration)* 3, #median + mad(duration)*3, down to N = 1066
    duration > 120, #min duration, N = 1063
    (is.na(Age) | Age < 100)) %>% #age must be plausible, N = 1062
  distinct(ResponseId, .keep_all = TRUE) %>% #make sure we have no duplicates, N = 1062
  dplyr::select(-UserLanguage, #user language
    -DistributionChannel, #channels for distribution
    -Q1.1, # Consent
    -Q4.1, # Attention_check,
    -duration,#duration
    -`Duration (in seconds)`, #duration
    -Progress,
    -Finished,
    -RecordedDate,
    -Q10.7)  

# Data cleaning and recoding of categories for main variables.
dat <- dat %>%
    rename(
        #TD vars
        Gain550 = "Q2.1",
        Gain600 = "Q2.2",
        Gain750 = "Q2.3",
        Gain510 = "Q2.4",
        Gain505 = "Q2.5",
        Pay550 = "Q3.1",
        Pay600 = "Q3.2",
        Pay750 = "Q3.3",
        Pay510 = "Q3.4",
        Pay505 = "Q3.5",
        Gain5500 = "Q5.1",
        Gain6000 = "Q5.2",
        Gain7500 = "Q5.3",
        Gain5100 = "Q5.4",
        Gain5050 = "Q5.5",
        
        #loss aversion vars
        loss_gain_1 = "Q6.1",
        loss_gain_2 = "Q6.3",
        loss_gain_3 = "Q6.5",
        loss_loss_1 = "Q6.2",
        loss_loss_2 = "Q6.4",
        loss_loss_3 = "Q6.6",
        
        #anomalies vars
        #Q7.1 - present bias, see below, (500$ in 12months versus indifference point in 24months)
        #Q7.2 - risk preference, see below
        alloc_debt = "Q7.3_2",
        alloc_spend = "Q7.3_3",
        alloc_save = "Q7.3_4",
        alloc_invest = "Q7.3_5",
        
        #agency vars
        work_situation = "Q8.1",
        agency_pub_trans_avail = "Q8.2_1",
        agency_pub_trans_realistic = "Q8.2_2",
        agency_uber_avail = "Q8.3_1",
        agency_uber_realistic = "Q8.3_2",
        agency_delivery_avail = "Q8.4_1",
        agency_delivery_realistic = "Q8.4_2",
        agency_grocery_avail = "Q8.5_1",
        agency_grocery_realistic = "Q8.5_2",
        agency_pharma_avail = "Q8.6_1",
        agency_pharma_realistic = "Q8.6_2",
        agency_docs_avail = "Q8.7_1",
        agency_docs_realistic = "Q8.7_2",
        agency_online_avail = "Q8.8_1",
        agency_online_realistic = "Q8.8_2",
        agency_IP_safe_avail = "Q8.9_1",
        agency_IP_safe_realistic = "Q8.9_2",
        agency_remote_avail = "Q8.10_1",
        agency_remote_realistic = "Q8.10_2",
        
        no_crowds = "Q8.11",
        covid = "Q8.12",
        covid_isolate_reality = "Q8.13",
        covid_isolate_imagine = "Q8.14",
        covid_no_isolate = "Q8.15",
        covid_no_isolate_text = "Q8.15_6_TEXT",
        
        #2020 behaviors vars
        mask_march2020 = "Q9.1_1",
        mask_spring2020 = "Q9.1_2",
        mask_summer2020 = "Q9.1_3",
        mask_fall2020 = "Q9.1_4",
        mask_winter2020 = "Q9.1_5",
        
        indoor_march2020 = "Q9.2_1",
        indoor_spring2020 = "Q9.2_2",
        indoor_summer2020 = "Q9.2_3",
        indoor_fall2020 = "Q9.2_4",
        indoor_winter2020 = "Q9.2_5",
        
        #finance and demographics vars
        expectation = "Q10.1",
        internet_access = "Q10.2",
        bills_2020 = "Q10.3",
        bills_current = "Q10.4",
        financial_2020 = "Q10.5",
        situation_debt = "Q10.6",
        Gender = "Q10.8",
        EducationCompleted = "Q10.9",
        Employment = "Q10.10",
        Ethnic = "Q10.11",
        Ethnic_text = "Q10.11_10_TEXT",
        zipcode = "Q1.2",
        work_zipcode = "Q10.12"
    ) %>%
    mutate(Gender = fct_recode(Gender,
        "Female" = "Woman",
        "Male" = "Man",
        "Other" = "Prefer not to answer",
        "Other" = "I prefer to use:"
    )) %>%
    select_all(list(~gsub(" ", "_", .))) %>%
    select_all(list(~gsub("-", "_", .))) %>%
    mutate(Risk_preference = case_when(
        str_detect(Q7.2, "A 25% chance of") ~ 4,
        str_detect(Q7.2, "A 50% chance of") ~ 3,
        str_detect(Q7.2, "A 67% chance of") ~ 2,
        str_detect(Q7.2, "A 75% chance of") ~ 1,
        str_detect(Q7.2, "Guarantee") ~ 0
    )) %>%
    dplyr::select( - Q7.2) %>%
    rename(Common_difference = "Q7.1") %>% #aka pres_bias
    relocate(Common_difference, .after = Gain5050)
    
#manually check on the gender_other text column
x <- subset(dat, Q10.8_4_TEXT != "NA") #two pp, neither meaningful entries
dat <- dat %>% select(-Q10.8_4_TEXT) #remove the column as we won't look into it
rm(x)

########################
# 1.2 Decision data
########################
#focus on the key decision biases and related responses

data_small <- dat[,-c(29:75)] #TEMP

dat_item <-
    data_small %>% #TEMP
    arrange(ResponseId) %>% 
    mutate(amount_later16 = case_when(
        Gain550 == "Receiving $550 in 12 months" & Gain510 == "Receiving $500 right now" ~ 550,
        Gain600 == "Receiving $600 in 12 months" ~ 600,
        Gain750 == "Receiving $500 right now" | Gain750 == "Receiving $750 in 12 months" ~ 750,
        Gain510 == "Receiving $510 in 12 months" & Gain505 == "Receiving $500 right now" ~ 510,
        Gain505 == "Receiving $505 in 12 months" ~ 505
    )) %>%
    mutate(amount_later17 = case_when(
        Gain550 == "Receiving $550 in 12 months" & Gain510 == "Receiving $500 right now" ~ 600,
        Gain600 == "Receiving $600 in 12 months" ~ 700,
        Gain750 == "Receiving $750 in 12 months" | Gain750 == "Receiving $500 right now" ~ 1000,
        Gain510 == "Receiving $510 in 12 months" & Gain505 == "Receiving $500 right now" ~ 520,
        Gain505 == "Receiving $505 in 12 months" ~ 510
    )) %>%
    pivot_longer(Gain550:Common_difference, names_to = c("question")) %>%
    mutate(
        delay_sooner = rep(c(rep(0, 15), 12), length(unique(ResponseId))),
        delay_later = rep(c(rep(12, 15), 24), length(unique(ResponseId))),
        amount_sooner = rep(c(
            rep(500, 10),
            rep(5000, 5),
            rep(500, 1)
        ), length(unique(ResponseId)))
    ) %>%
    mutate(amount_later = case_when(
        question == "Gain550" | question == "Pay550" ~ 550,
        question == "Gain600" | question == "Pay600" ~ 600,
        question == "Gain750" | question == "Pay750" ~ 750,
        question == "Gain510" | question == "Pay510" ~ 510,
        question == "Gain505" | question == "Pay505" ~ 505,
        question == "Gain5500" ~ 5500,
        question == "Gain6000" ~ 6000,
        question == "Gain7500" ~ 7500,
        question == "Gain5100" ~ 5100,
        question == "Gain5050" ~ 5050,
        question == "Common_difference" ~ amount_later16 #aka present bias 
    )) %>%
    mutate(question = paste("Q", rep(1:ncol(select(dat, Gain550:"Common_difference")), nrow(dat)), sep = "")) %>%
    mutate(question_num = as.numeric(gsub("Q", "", question))) %>%
    mutate(block = case_when(
        question_num < 6 ~ "block1",
        question_num >= 6 & question_num < 11 ~ "block2",
        question_num >= 11 & question_num < 16 ~ "block3",
        question_num == 16 ~ "anom1"
    )) %>%
    mutate(choice = case_when(
        is.na(value) ~ NA_real_,
        str_detect(value, "500 right now") ~ 0,
        str_detect(value, "5000 right now") ~ 0,
        str_detect(value, "5500 in 12 months") ~ 1,
        str_detect(value, "7500 in 12 months") ~ 1,
        str_detect(value, "500 in 12 months") ~ 0,
        str_detect(value, "Reduce") ~ 0,
        TRUE ~ 1
    )) %>%
    rename(choice_text = "value") %>%
    relocate(choice_text, .after = choice) %>%
    select(-amount_later16) %>%
    drop_na(choice) %>%
    mutate_if(is.character, as.factor)

###############################
# 2. Computing anomalies/scores
###############################
## We estimate the anomalies rates for the anomalies (present bias = presbias; absolute magnitude = absolmag, gain-loss asymmetry = gainloss).
## We use answers to the three baseline sets of questions plus the answers to the common difference to compute the temporal discount scores.

## Computing the rates of the anomalies #CHECK THESE ARE CORRECT where does the 6, 16, 11, etc come from in relation to our data 
presbias <- as.numeric(get_latest_answer(dat_item, 6)$choice != get_second_choices(dat_item, "Q16"))
absolmag <- as.numeric(get_latest_answer(dat_item, 6)$choice != as.numeric(get_latest_answer(dat_item, 16)$choice))
gainloss <- as.numeric(get_latest_answer(dat_item, 6)$choice == as.numeric(get_latest_answer(dat_item, 11)$choice))

anomalies_data <- list()
## We apply the function fixer_anom to estimate whether the anomalies are observed and whether those are consistent with the theory or not.
## Given a first and a second set of responses, plus an ID identifier, it will classify the pattern of responses in anomaly/not anomaly and in the first case, whether it is consistent or not.
## In the second case (not anomaly), it will also inform whether the first decision was a sooner or a later choice.


##### Qing: my Q16 is Q7.1
anomalies_data[[1]] <- fixer_anom(data.frame(
    fc = get_latest_answer(dat_item, 6)$choice,
    sc = get_second_choices(dat_item, "Q16"), #presbias / cmmon difference
    ResponseId = unique(dat_item$ResponseId)
), type = "presbias")

anomalies_data[[2]] <- fixer_anom(data.frame(
    fc = get_latest_answer(dat_item, 6)$choice,
    sc = as.numeric(get_latest_answer(dat_item, 16)$choice),
    ResponseId = unique(dat_item$ResponseId)
), type = "absolmag")

anomalies_data[[3]] <- fixer_anom(data.frame(
    fc = get_latest_answer(dat_item, 6)$choice,
    sc = as.numeric(get_latest_answer(dat_item, 11)$choice),
    ResponseId = unique(dat_item$ResponseId)
), type = "gainloss")

##### Qing# the type gainloss is called response instead of response.x.x
anomalies_congruent <- anomalies_data %>%
    ### We are only interested in individual responses
    reduce(left_join, by = "ResponseId") %>%
    rename(
        "presbias" = "response.x",
        "absolmag" = "response.y",
        "gainloss" = "response"
    ) %>%
    select(ResponseId, presbias, absolmag, gainloss) %>%
    mutate_at(
        vars(presbias, absolmag, gainloss),
        list(~ case_when(
            . == "Congruent" ~ 1,
            TRUE ~ 0
        ))
    )

##  We calculate the temporal discount scores from responses to each block · ## no Q17-Q19
### Careful with reversed scores when estimating the scores
scores <- get_scores(get_latest_answer(dat_item, 6))$score +
    5 - get_scores(get_latest_answer(dat_item, 11))$score +
    get_scores(get_latest_answer(dat_item, 16))$score +
    1 - get_second_choices(dat_item, "Q16")

q <- dat_item %>%
    count(ResponseId) %>%
    arrange()

dat_item <- dat_item %>%
    arrange(ResponseId) %>%
    mutate(
        score = rep(scores, q$n),
        presbias = rep(anomalies_congruent$presbias, q$n),
        absolmag = rep(anomalies_congruent$absolmag, q$n),
        gainloss = rep(anomalies_congruent$gainloss, q$n),
    )

## We remove unnecessary columns and variables
dat_unique <- dat_item %>%
    distinct(ResponseId, .keep_all = TRUE) %>%
    select(-question, -delay_sooner, -delay_later, -amount_sooner, -amount_later, -question_num, -block, -choice, -choice_text)

rm(q, scores, presbias, absolmag, gainloss)

write_csv(dat_item, "../results/scores/1_3_dat_unique_item.csv")
write_csv(dat, "../data/raw_survey/processed.csv")
