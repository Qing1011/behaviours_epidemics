library(tidycensus)
library(tidyverse)
library(viridis)
library(data.table)

census_api_key("919b37e63df029d1420900f893770f49d4a03226") #,install = TRUE
v21 <- load_variables(2021, "acs5", cache = TRUE)
#View(v21)
#write_csv(v21,'../data/variable_namesv21.csv')

mod_counts <- read.csv('../results/counts/modzcta_zip_counts.csv')
select_zcta <- mod_counts[mod_counts$modzcta_count > 15, "ZCTA"]
zcta_ls <- select_zcta
#zcta_ls <- select_zcta[!select_zcta %in% c(10004,10007,10005,10271)]


###############**Population**################
race_group_popu <- c(
  'B01001A_001',
  'B01001B_001',
  'B01001C_001',
  'B01001D_001',
  'B01001E_001',
  'B01001F_001',
  'B01001G_001'
)
ls <- length(race_group_popu) + 1
population <- get_acs(geography = "zcta",
                      variables = race_group_popu,
                      year = 2021,
                      zcta = zcta_ls,
                      moe_level = 90,
                      survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls),na.rm=TRUE)) %>%
    select(GEOID, Population = Total_Estimate)


###############**BLACK**################
#B01001B_001 Estimate!!Total: SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)
black <- get_acs(geography = "zcta",
                    variables = 'B02001_003',
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, Black = estimate)


###############**HISPANIC**################
#B01001I_001
hispanic <- get_acs(geography = "zcta",
                    variables = 'B01001I_001',
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, Hispanic = estimate)


###############**NO HEALTH INSURANCE**################
rows_with_no_insurance <- v21[grep("No health insurance coverage", v21$label), ]
sub_no_insurance <- rows_with_no_insurance[rows_with_no_insurance$concept == "HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE",]
nohealthinsurance_groups <- sub_no_insurance$name
ls <- length(nohealthinsurance_groups)+1
no_health_insurance <- get_acs(geography = "zcta",
                    variables = nohealthinsurance_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, No_health_insurance = Total_Estimate)


###############**BACHELORS**################
bachelor <- get_acs(geography = "zcta",
                    variables = 'B06009_005',
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, Bachelor = estimate)


###############**HOUSEHOLD INCOME**################
householdincome <- get_acs(geography = "zcta",
                    variables = 'B19019_001',
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, Household_income = estimate)


###############**HOUSEHOLD NUM**################
householdnum <- get_acs(geography = "zcta",
                    variables = "B08201_001",
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, Household_num = estimate)


###############**NO VEHICLES**################  
no_vehicles <- get_acs(geography = "zcta",
                    variables = "B08201_002",
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select(GEOID, No_vehicles = estimate)


###############**Age**################
ageunder5_groups <- c(
  'B01001A_003',
  'B01001A_018',
  'B01001B_003',
  'B01001B_018',
  'B01001C_003',
  'B01001C_018',
  'B01001D_003',
  'B01001D_018',
  'B01001E_003',
  'B01001E_018',
  'B01001F_003',
  'B01001F_018',
  'B01001G_003',
  'B01001G_018')
ls <- length(ageunder5_groups)+1
ageunder5 <- get_acs(geography = "zcta",
                    variables = ageunder5_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, ageunder5 = Total_Estimate)


age5to9_groups <- c(
  'B01001A_004',
  'B01001A_019',
  'B01001B_004',
  'B01001B_019',
  'B01001C_004',
  'B01001C_019',
  'B01001D_004',
  'B01001D_019',
  'B01001E_004',
  'B01001E_019',
  'B01001F_004',
  'B01001F_019',
  'B01001G_004',
  'B01001G_019'
)
ls <- length(age5to9_groups) + 1
age5to9 <- get_acs(geography = "zcta",
                    variables = age5to9_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age5to9 = Total_Estimate)

age10to14_groups <- c(
  'B01001A_005',
  'B01001A_020',
  'B01001B_005',
  'B01001B_020',
  'B01001C_005',
  'B01001C_020',
  'B01001D_005',
  'B01001D_020',
  'B01001E_005',
  'B01001E_020',
  'B01001F_005',
  'B01001F_020',
  'B01001G_005',
  'B01001G_020')
ls <- length(age10to14_groups) + 1
age10to14 <- get_acs(geography = "zcta",
                    variables = age10to14_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age10to14 = Total_Estimate)

age15to17_groups <- c(
  'B01001A_006',
  'B01001A_021',
  'B01001B_006',
  'B01001B_021',
  'B01001C_006',
  'B01001C_021',
  'B01001D_006',
  'B01001D_021',
  'B01001E_006',
  'B01001E_021',
  'B01001F_006',
  'B01001F_021',
  'B01001G_006',
  'B01001G_021')######
ls <- length(age15to17_groups) + 1
age15to17 <- get_acs(geography = "zcta",
                    variables = age15to17_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age15to17 = Total_Estimate)

age18and19_groups <- c(
  'B01001A_007',
  'B01001A_022',
  'B01001B_007',
  'B01001B_022',
  'B01001C_007',
  'B01001C_022',
  'B01001D_007',
  'B01001D_022',
  'B01001E_007',
  'B01001E_022',
  'B01001F_007',
  'B01001F_022',
  'B01001G_007',
  'B01001G_022')####
ls <- length(age18and19_groups) + 1
age18and19 <- get_acs(geography = "zcta",
                    variables = age18and19_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age18and19 = Total_Estimate)

age20to24_groups <- c(
  'B01001A_008',
  'B01001A_023',
  'B01001B_008',
  'B01001B_023',
  'B01001C_008',
  'B01001C_023',
  'B01001D_008',
  'B01001D_023',
  'B01001E_008',
  'B01001E_023',
  'B01001F_008',
  'B01001F_023',
  'B01001G_008',
  'B01001G_023') 
ls <- length(age20to24_groups) + 1
age20to24 <- get_acs(geography = "zcta",
                    variables = age20to24_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age20to24 = Total_Estimate)

age25to29_groups <- c(
  'B01001A_009',
  'B01001A_024',
  'B01001B_009',
  'B01001B_024',
  'B01001C_009',
  'B01001C_024',
  'B01001D_009',
  'B01001D_024',
  'B01001E_009',
  'B01001E_024',
  'B01001F_009',
  'B01001F_024',
  'B01001G_009',
  'B01001G_024')
ls <- length(age25to29_groups) + 1
age25to29 <- get_acs(geography = "zcta",
                    variables = age25to29_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age25to29 = Total_Estimate)

age30to34_groups <- c(
  'B01001A_010',
  'B01001A_025',
  'B01001B_010',
  'B01001B_025',
  'B01001C_010',
  'B01001C_025',
  'B01001D_010',
  'B01001D_025',
  'B01001E_010',
  'B01001E_025',
  'B01001F_010',
  'B01001F_025',
  'B01001G_010',
  'B01001G_025')
ls <- length(age30to34_groups) + 1
age30to34 <- get_acs(geography = "zcta",
                    variables = age30to34_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age30to34 = Total_Estimate)


age35to44_groups <- c(
  'B01001A_011',
  'B01001A_026',
  'B01001B_011',
  'B01001B_026',
  'B01001C_011',
  'B01001C_026',
  'B01001D_011',
  'B01001D_026',
  'B01001E_011',
  'B01001E_026',
  'B01001F_011',
  'B01001F_026',
  'B01001G_011',
  'B01001G_026')
ls <- length(age35to44_groups) + 1
age35to44 <- get_acs(geography = "zcta",
                    variables = age35to44_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age35to44 = Total_Estimate)


age45to54_groups<-c(
  'B01001A_012',
  'B01001A_027',
  'B01001B_012',
  'B01001B_027',
  'B01001C_012',
  'B01001C_027',
  'B01001D_012',
  'B01001D_027',
  'B01001E_012',
  'B01001E_027',
  'B01001F_012',
  'B01001F_027',
  'B01001G_012',
  'B01001G_027')
ls <- length(age45to54_groups) + 1
age45to54 <- get_acs(geography = "zcta",
                    variables = age45to54_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age45to54 = Total_Estimate)

age55to64_groups <- c(
  'B01001A_013',
  'B01001A_028',
  'B01001B_013',
  'B01001B_028',
  'B01001C_013',
  'B01001C_028',
  'B01001D_013',
  'B01001D_028',
  'B01001E_013',
  'B01001E_028',
  'B01001F_013',
  'B01001F_028',
  'B01001G_013',
  'B01001G_028')
ls <- length(age55to64_groups) + 1
age55to64 <- get_acs(geography = "zcta",
                    variables = age55to64_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age55to64 = Total_Estimate)

age65to74_groups <- c(
  'B01001A_014',
  'B01001A_029',
  'B01001B_014',
  'B01001B_029',
  'B01001C_014',
  'B01001C_029',
  'B01001D_014',
  'B01001D_029',
  'B01001E_014',
  'B01001E_029',
  'B01001F_014',
  'B01001F_029',
  'B01001G_014',
  'B01001G_029')
ls <- length(age65to74_groups) + 1
age65to74 <- get_acs(geography = "zcta",
                    variables = age65to74_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age65to74 = Total_Estimate)

age75to84_groups<-c(
  'B01001A_015',
  'B01001A_030',
  'B01001B_015',
  'B01001B_030',
  'B01001C_015',
  'B01001C_030',
  'B01001D_015',
  'B01001D_030',
  'B01001E_015',
  'B01001E_030',
  'B01001F_015',
  'B01001F_030',
  'B01001G_015',
  'B01001G_030')
ls <- length(age75to84_groups) + 1
age75to84 <- get_acs(geography = "zcta",
                    variables = age75to84_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age75to84 = Total_Estimate)

age85plus_groups<-c(
  'B01001A_016',
  'B01001A_031',
  'B01001B_016',
  'B01001B_031',
  'B01001C_016',
  'B01001C_031',
  'B01001D_016',
  'B01001D_031',
  'B01001E_016',
  'B01001E_031',
  'B01001F_016',
  'B01001F_031',
  'B01001G_016',
  'B01001G_031')
ls <- length(age85plus_groups) + 1
age85plus <- get_acs(geography = "zcta",
                    variables = age85plus_groups,
                    year = 2021,
                    zcta = zcta_ls,
                    moe_level = 90,
                    survey = "acs5") %>%
    select('GEOID','variable','estimate') %>%
    pivot_wider(names_from = variable,
                values_from = estimate,
                names_sep = "_") %>%
    mutate(Total_Estimate = rowSums(select(.,2:ls), na.rm = TRUE)) %>%
    select(GEOID, age85plus = Total_Estimate)

age_data_frames <- list(ageunder5, age5to9, age10to14, age15to17, age18and19, age20to24,age25to29, age30to34, age35to44, age45to54, age55to64, age65to74, age75to84, age85plus)
age <- Reduce(function(x, y) merge(x, y, by = "GEOID", all = TRUE), age_data_frames)
age_midpoints <- c(2.5, 7, 12, 16, 18.5, 22, 27, 32, 39.5, 49.5, 59.5, 69.5, 79.5, 90)
calculate_median_age <- function(age_midpoints, row) {
  expanded_ages <- unlist(mapply(rep, age_midpoints, row))
  median(expanded_ages)
}
age$median_age <- apply(age[,2:15], 1, function(x) calculate_median_age(age_midpoints,x))
weighted_sum <- apply(age[,2:15], 1, function(row) sum(row * age_midpoints))
total_population <- rowSums(age[,2:15])
age$weighted_estimated_average_age <- weighted_sum / total_population
rm(ageunder5, age5to9, age10to14, age15to17, age18and19, age20to24,age25to29, age30to34, age35to44, age45to54, age55to64, age65to74, age75to84, age85plus)
age <- age %>% select(GEOID, weighted_estimated_average_age, median_age)

rows_with_na <- which(apply(age, 1, function(x) any(is.na(x))))

for (na_idx in rows_with_na){
  zip_i <- age[na_idx,"GEOID"] ## Finde the zipcode
  mod_i <- mod_counts[mod_counts$zip_code==zip_i,'MODZCTA']
  mod_idx <- which(age$GEOID==mod_i)
  age[na_idx,'median_age'] <- age[mod_idx,'median_age']
  age[na_idx,'weighted_estimated_average_age'] <-age[mod_idx, 'weighted_estimated_average_age']
  householdincome[na_idx,'Household_income'] <- householdincome[mod_idx,'Household_income']
}

##########**MERGE**##############
zipcodedataNY <- age
data_frames <- list(black, hispanic, householdincome, bachelor, householdnum, population, no_health_insurance, no_vehicles)
for (df in data_frames) {
  zipcodedataNY <- merge(zipcodedataNY, df, by = "GEOID")
}
fileName = c('../data/raw_soeco/tract_data2021.csv')
write.csv(zipcodedataNY, file = fileName, row.names=FALSE)




