
#######################################
######### Stats-related functions #####
#######################################

get_scores <- function (x) {
  
  ## This function will compute the total temporal discount for each participant based on their responses to each block question
  ##
  ## Input (x): A block of questions (5 previous questions to the one parsed to the function)
  ##
  ## Output: Vector of individual sum responses per block
  
  f <- as.numeric(gsub("block", "", x$block))[1]
  
  if (f == 1) {
    x %>% 
      mutate(score = case_when(
        amount_later == 750 & choice == 0 ~ 5,
        amount_later == 750 & choice == 1 ~ 4,
        amount_later == 600 & choice == 1 ~ 3,
        amount_later == 510 & choice == 0 ~ 2,
        amount_later == 505 & choice == 0 ~ 1,
        amount_later == 505 & choice == 1 ~ 0
      ))
    
  } else if (f == 2) {
    x %>% 
      mutate(score = case_when(
        amount_later == 750 & choice == 1 ~ 0,
        amount_later == 750 & choice == 0 ~ 1,
        amount_later == 600 & choice == 0 ~ 2,
        amount_later == 510 & choice == 1 ~ 3,
        amount_later == 505 & choice == 0 ~ 5,
        amount_later == 505 & choice == 1 ~ 4
      ))
  } else {
    x %>% 
      mutate(score = case_when(
        amount_later == 7500 & choice == 0 ~ 5,
        amount_later == 7500 & choice == 1 ~ 4,
        amount_later == 6000 & choice == 1 ~ 3,
        amount_later == 5100 & choice == 0 ~ 2,
        amount_later == 5050 & choice == 0 ~ 1,
        amount_later == 5050 & choice == 1 ~ 0
      ))
  }
}

get_latest_answer <- function (x, y){
  ## This function will obtain the latest answer given to the initial baseline questions. 
  ## This function is necessary to control for the different possible options shown to each individual.
  ##
  ## Input: 
  ## (x) A data frame including choices to questions; 
  ## (y) a scalar identifying the next item following the item block. 
  ##
  ## Output: Vector of individual responses to the anomaly item
  
  x %>% 
    group_by(ResponseId) %>% 
    filter(as.numeric(gsub("Q", "", question)) < y) %>% 
    arrange(desc(as.numeric(gsub("Q", "", question)))) %>% 
    slice(1) %>% 
    ungroup() %>% 
    arrange(ResponseId) %>% 
    dplyr:::select(ResponseId, choice, amount_later, block) 
}

get_second_choices <- function (x, y) {
  ## This function will obtain the answer given to the anomalies questions. 
  ## This function necessary to control for the different possible options shown to each individual
  ##
  ## Input: (x) A data frame including choices to questions; (y) the anomaly item identifier. 
  ##
  ## Output: Vector of individual responses to the anomaly item
  
  x %>% 
    arrange(ResponseId) %>%   
    arrange(desc(as.numeric(gsub("Q", "", question)))) %>%   
    filter(question == y) %>% 
    dplyr:::select(choice) %>% 
    unlist() %>% 
    as.vector()
}


fixer_anom <- function (d, type = NULL){
  
  ## This function will compute whether an anomaly was observed 
  ##(and whether the initial answer was a sooner or later decision) 
  ##and if the possible anomaly was congruent with theory or not.
  ##
  ## Input: (x) A data frame including the first choice (fc) and the type of anomaly to be estimated. 
  ##
  ## Output: dataframe including the individual id and the variables response (possible anomaly and initial decision) and anomaly (whether an anomaly or a consistent decision was observed).
  
  if (type == "presbias" || type == "absolmag" || type == "delay") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "No anomaly sooner",
        fc == 1 & sc == 0 ~ "Anomaly later",
        fc == 0 & sc == 1 ~ "Anomaly sooner",
        fc == 1 & sc == 1 ~ "No anomaly later")) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly")) %>%
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>% 
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type) 
    
  } else if (type == "gainloss") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "Anomaly sooner",
        fc == 1 & sc == 0 ~ "No anomaly later",
        fc == 0 & sc == 1 ~ "No anomaly sooner",
        fc == 1 & sc == 1 ~ "Anomaly later")) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly")) %>% 
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>%
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type) 
    
  }  else if (type == "subaddit") {  
    
    d %>%
      mutate(response = case_when (
        fc == 0 & sc == 0 ~ "No anomaly sooner",
        fc == 0 & sc == 1 ~ "Anomaly sooner",
        fc == 1 & sc == 1 ~ "Inconsistent",
        fc == 1 & sc == 0 ~ "Inconsistent",
        fc == 2 & sc == 0 ~ "Anomaly later",
        fc == 2 & sc == 1 ~ "No anomaly later",
      )) %>% 
      mutate (response = as.factor(response)) %>% 
      mutate(anomaly = case_when(
        response == "Anomaly later" ~ "Possible anomaly",
        response == "Anomaly sooner" ~ "Possible anomaly",
        response == "No anomaly later" ~ "Consistent",
        response == "No anomaly sooner" ~ "Consistent",
        response ==  "Inconsistent" ~ "Inconsistent"
      )) %>% 
      mutate(anomaly = fct_relevel(anomaly, "Consistent", "Possible anomaly", "Inconsistent")) %>% 
      mutate(response = fct_recode(response, 
                                   "Congruent" = "Anomaly sooner", 
                                   "Not congruent" = "Anomaly later")) %>%
      mutate(type = rep(type, length(response))) %>% 
      dplyr:::select(ResponseId, response, anomaly, type)  
    
  }
} 
