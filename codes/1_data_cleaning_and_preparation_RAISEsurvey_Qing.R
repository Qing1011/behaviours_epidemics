# This series of scripts will reproduce the analysis of the "The globalizability of temporal discounting". This document is preregistered at OSF.
## This script will clean and prepare the datasets for model estimation

########################
# 0. Utility functions
########################

## The following functions are necessary to handle data wrangling as well as to compute the different scores.

if (!require("tidyverse")) install.packages("tidyverse")
if (!require("scales")) install.packages("scales")

source("999_1_auxiliary_functions.R")

options(digits = 3)
options(scipen = 99)


########################
# 1. Data cleaning
########################
## In this section we upload the pilot dataset.
### We include a identification number for each case, compute age, apply the attention check and estimate spending allocation in relative (%) terms.
### We also select a main ethnic background for each individual (to be changed for allowing mixed ethnic background if used in analyses in the future).
### We also allocate delay-speedup questions next to each other for future computations.
### We further clean variables not explored in these analyses but of potential interest for researchers (e.g., time_vs_money).
dat_org <- read_csv(
    file = file.choose()
)
dat_org <- dat_org[-c(1, 2), ] ### the first two second rows are not needed
dat_org$`Duration (in seconds)` <- as.numeric(dat_org$`Duration (in seconds)`)
dat_org$Progress <- as.numeric(dat_org$Progress)


### Qing ####d
# do not need Q31-----Q34
#########
dat <- dat_org %>%
    # Data cleaning and recoding of categories for main variables.
    rename(
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
        duration = "Duration (in seconds)",
        # disc_debt = "Discretionary spend_2",
        # disc_non_fin = "Discretionary spend_3",
        # disc_savings = "Discretionary spend_4",
        # disc_invest = "Discretionary spend_5",
        expectation = "Q10.1",
        bills = "Q10.3", ### Qing ######not sure#########
        # income = "Q26_1",
        # debt = "Q26_2",
        # assets = "Q26_3",
        financial_2020 = "Q10.5",
        situation_debt = "Q10.6",
        # child_situation = "Q29",
        Gender = "Q10.8",
        EducationCompleted = "Q10.9",
        Employment = "Q10.10",
        Ethnic = "Q10.11"
    ) %>%
    ### Qing #####
    # I deleted all parts about countries and text cleaning
    #########
    mutate(Gender = fct_recode(Gender,
        "Female" = "Woman",
        "Male" = "Man",
        "Other" = "Prefer not to answer",
        "Other" = "I prefer to use:"
    )) %>%
    select_all(funs(gsub(" ", "_", .))) %>%
    select_all(funs(gsub("-", "_", .))) %>%
    mutate(duration = as.numeric(duration)) %>%
    # mutate(Progress = as.numeric(Progress)) %>%
    mutate(Age = 2023 - as.numeric(Q10.7)) %>%
    mutate(Risk_preference = case_when(
        str_detect(Q7.2, "A 25% chance of") ~ 4,
        str_detect(Q7.2, "A 50% chance of") ~ 3,
        str_detect(Q7.2, "A 67% chance of") ~ 2,
        str_detect(Q7.2, "A 75% chance of") ~ 1,
        str_detect(Q7.2, "Guarante") ~ 0
    ))
### Qing #####
# Cound not find  Interval_markup, Time_vs_Money; thus deleted the related codes.
#########
### We add the initial data quality checks. Those include pass the attention check, minimum and maximum response times and minimum progress of 90%
dat <- dat %>%
    filter(
        str_detect(Q4.1, "PASS"),
        str_detect(Status, "IP"),
        str_detect(Finished, "TRUE"),
        duration > (median(duration) - mad(duration) * 3),
        duration > 120,
        Progress > 90,
    ) %>%
    #### Qing#####
    # Q4.1 is the Attention_check
    #############
    ### We remove implausible responses for age and income
    #   mutate (income = replace(income, which(income <0), NA),
    #           debt = replace(debt, which(debt <0), NA),
    #           assets = replace(assets, which(assets <0), NA)) %>%

    filter(is.na(Age) | Age < 100) %>%
    #   filter(is.na(Other_gender)|
    #            str_detect(replace_na(Other_gender,""), "gender")|
    #            str_detect(replace_na(Other_gender,""), "Gender")|
    #            str_detect(replace_na(Other_gender,""), "NB")|
    #            str_detect(replace_na(Other_gender,""), "No ")|
    #            str_detect(replace_na(Other_gender,""), "No")|
    #            str_detect(replace_na(Other_gender,""), "no ")|
    #            str_detect(replace_na(Other_gender,""), "non")|
    #            str_detect(replace_na(Other_gender,""), "rans")) %>%

    ### We clean the dataset to keep only the most relevant columns and in the desired order.
    distinct(ResponseId, .keep_all = TRUE) %>%
    #   relocate(Delay_speedup_2, .after = Delay_speedup_1) %>%
    ##### Qing #####
    # Data cleaning and recoding of categories for main variables.  I do not have delay speedup 1-2, just have 7.1
    #############
    rename(Common_difference = "Q7.1") %>%
    relocate(Common_difference, .after = Gain5050) %>% ### no additivity and speedup measurement directly
    select(
        -StartDate, -EndDate, -Status, -Finished, -RecordedDate,
        -RecipientLastName, -RecipientFirstName, -RecipientEmail,
        -ExternalReference,
        -DistributionChannel,
        -Q1.1, # Consent
        -Q4.1, # Attention_check,
        -duration,
        -starts_with("Q10.11"), # starts_with("Q34") ethics,
        -starts_with("Q10.8") # starts_with("Q31") gender
    )
##### Qing########
# Countries related parts are deleted
#################

dat_item <-
    dat %>%
    arrange(ResponseId) %>% # not have response id originally, i assign to it
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
    # mutate(amount_later18 = case_when(
    #     amount_later16 == 505 ~ 505,
    #     amount_later16 == 510 ~ 510,
    #     amount_later16 == 550 ~ 550,
    #     amount_later16 == 600 ~ 600,
    #     amount_later16 == 750 ~ 750
    # )) %>%
    # mutate(amount_later19 = case_when(
    #     amount_later16 == 505 ~ 505,
    #     amount_later16 == 510 ~ 510,
    #     amount_later16 == 550 ~ 550,
    #     amount_later16 == 600 ~ 600,
    #     amount_later16 == 750 ~ 750
    # )) %>%
    #### Qing########
    # I do not have delay speedup 1-2, then amount_later18-19 are not necessary
    # I do not have subadditivity, then amount_later17 is not necessary
    # No Subadditivity, Delay-speedup 1 and Delay-speedup 2,
    # the lengths of delay_sooner(later/sooner) have 15+1 instead of 15+4
    # common difference is assigned to be 12
    #############
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
        question == "Common_difference" ~ amount_later16,
        # question == "Subadditivity" ~ amount_later17,
        # question == "Delay_speedup_2" ~ amount_later18,
        # question == "Delay_speedup_2" ~ amount_later19
    )) %>%
    mutate(question = paste("Q", rep(1:ncol(select(dat, Gain550:"Common_difference")), nrow(dat)), sep = "")) %>%
    mutate(question_num = as.numeric(gsub("Q", "", question))) %>%
    mutate(block = case_when(
        question_num < 6 ~ "block1",
        question_num >= 6 & question_num < 11 ~ "block2",
        question_num >= 11 & question_num < 16 ~ "block3",
        question_num == 16 ~ "anom1",
        # question_num == 17 ~ "anom2",
        # question_num == 18 ~ "anom3",
        # question_num == 19 ~ "anom3",
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

## We estimate the anomalies rates for the five anomalies (present bias = presbias; absolute magnitude = absolmag, gain-loss asymmetry = gainloss; delay-speedup = delayspeed; subadditivity = subaddit).
## We use answers to the three baseline sets of questions plus the answers to the common difference, subadditivity, and delay-speedup items to compute the temporal discount scores.

## Computing the rates of the five anomalies
presbias <- as.numeric(get_latest_answer(dat_item, 6)$choice != get_second_choices(dat_item, "Q16"))
absolmag <- as.numeric(get_latest_answer(dat_item, 6)$choice != as.numeric(get_latest_answer(dat_item, 16)$choice))
gainloss <- as.numeric(get_latest_answer(dat_item, 6)$choice == as.numeric(get_latest_answer(dat_item, 11)$choice))
##### Qing########
# no Q17 - Q19 in questions
#############
# delayspeed <- as.numeric(get_latest_answer(dat_item, 6)$choice != get_second_choices(dat_item, "Q19"))
# subaddit_all <- data.frame(
#     sub1 = as.numeric(get_latest_answer(dat_item, 6)$choice +
#         get_second_choices(dat_item, "Q16")),
#     sub2 = get_second_choices(dat_item, "Q17")
# )

# subaddit <- mutate(subaddit_all, sub = case_when(
#     sub1 == 0 & sub2 == 1 ~ 1,
#     sub1 == 2 & sub2 == 0 ~ 1,
#     TRUE ~ 0
# ))$sub

anomalies_data <- list()
## We apply the function fixer_anom to estimate whether the anomalies are observed and whether those are consistent with the theory or not.
## Given a first and a second set of responses, plus an ID identifier, it will classify the pattern of responses in anomaly/not anomaly and in the first case, whether it is consistent or not.
## In the second case (not anomaly), it will also inform whether the first decision was a sooner or a later choice.
##### Qing########
# my Q16 is Q5.5
#############
anomalies_data[[1]] <- fixer_anom(data.frame(
    fc = get_latest_answer(dat_item, 6)$choice,
    sc = get_second_choices(dat_item, "Q16"),
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

##### Qing#####
# the type gainloss is called response instead of response.x.x
##############
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

### We correct the scores for the Estonian sample (see Supplementary Data for details)
### Qing########
# no Estonian sample
#############
# est_scores <- rescale(dat_item[dat_item$Code == "EST", ]$score, to = c(0, 19),
#                       from = range(0,17, na.rm = TRUE))
# dat_item[dat_item$Code == "EST", ]$score <- est_scores
# dat_item[dat_item$Code == "EST", ]$presbias <- NA
# dat_item[dat_item$Code == "EST", ]$subaddit <- NA

## We remove unnecessary columns and variables
dat_unique <- dat_item %>%
    distinct(ResponseId, .keep_all = TRUE) %>%
    select(-question, -delay_sooner, -delay_later, -amount_sooner, -amount_later, -question_num, -block, -choice, -choice_text)

rm(q, scores, presbias, absolmag, gainloss)

write.csv(dat_item, file = "1_3_dat_unique_item.csv")
