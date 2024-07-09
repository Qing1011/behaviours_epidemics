if(!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")
if(!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(tidycensus)
library(tidyverse)
library(data.table)

census_api_key("919b37e63df029d1420900f893770f49d4a03226") #,install = TRUE
v21 <- load_variables(2021, "acs5", cache = TRUE)
View(v21)
write_csv(v21,'../data/variable_namesv21.csv')

# population: B25026_001, Estimate!!Total population in occupied housing units:
#population <- get_acs(geography = 'tract', state = '36', survey = "acs5",
                      #variables = c(population = "B25026_001"), 
                      #year = 2021)

#population <- population[,c('GEOID','estimate')]
#setnames(population, c('estimate'), c('Population'))


### there are other people not in the occupied housing units
### using race is more accurate estimation of population
race_group_popu <- c(
  'B01001B_001',
  'B01001C_001',
  'B01001D_001',
  'B01001E_001',
  'B01001F_001',
  'B01001G_001',
  'B01001H_001'
)
#B01001A_001

population <- get_acs(geography = 'tract', state = '36',
                      variables = c(employment = 'B01001I_001'),
                      year = 2021)
population<- population[,c('GEOID','estimate')]

for (a_i in race_group_popu)
{
  population_temp <- get_acs(geography = 'tract', state = '36',
                             variables = c(population_temp = a_i),
                             year = 2021)    
  population_temp <- population_temp[,c('GEOID','estimate')]
  population$estimate <- population$estimate + population_temp$estimate
}

setnames(population, c('estimate'), c('Population'))



###############*******################
#B01001B_001 Estimate!!Total: SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)
black <- get_acs(geography = "tract", state = '36',
                 variables = c(black = "B02001_003"), 
                 year = 2021)
black <- black[,c('GEOID','estimate')]
setnames(black, c('estimate'), c('Black'))

###############*******################
#Hispanic B01001I_001
# B03001_003, Estimate!!Total:!!SEX BY AGE (HISPANIC OR LATINO)
hispanic <- get_acs(geography = "tract", state = '36',
                    variables = c(hispanic = "B01001I_001"), 
                    year = 2021)

hispanic <- hispanic[,c('GEOID','estimate')]
setnames(hispanic, c('estimate'), c('Hispanic'))


###############*******################
#median household income
# Estimate!!Total:
#householdincome: MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2021 INFLATION-ADJUSTED DOLLARS) BY HOUSEHOLD SIZE
householdincome <- get_acs(geography = "tract", state = '36',
                           variables = c(householdincome = "B19019_001"), 
                           year = 2021)
householdincome <- householdincome[,c('GEOID','estimate')]
setnames(householdincome, c('estimate'), c('Householdincome'))

###############*******################
#bachelor’s degree
#bachelor: B06009_005, Estimate!!Total:!!Bachelor's degree
bachelor <- get_acs(geography = "tract", state = '36',
                    variables = c(bachelor = "B06009_005"), 
                    year = 2021)
bachelor <- bachelor[,c('GEOID','estimate')]
setnames(bachelor, c('estimate'), c('Bachelor'))

###############*******################
#household num: HOUSEHOLD SIZE BY VEHICLES AVAILABLE
household_num <- get_acs(geography = "block group", state = 'NY',
                      variables = c(households_num = "B11001_001"), 
                      year = 2021)


household_num <- household_num[,c('GEOID','estimate')]
setnames(household_num, c('estimate'), c('Household_num'))
###### this is 12 digits ####
####aggrated into 11 digits####
household_num$tract <- substr(household_num$GEOID, 1, 11)
# take the mean value of all the block group as the value of the tract.
household_num <- household_num %>%
  group_by(tract) %>%
  summarise(mean_householdnum = mean(Household_num, na.rm = TRUE))%>%
  rename(GEOID = tract)
# fill the na value with 0 
household_num$mean_householdnum[is.na(householdsize_num$mean_householdnum)] <- 0


###############*******################
#household size: HOUSEHOLD SIZE BY VEHICLES AVAILABLE
household_size <- get_acs(geography = "tract", state = '36',
                      variables = c(households_num = "B08201_001"), 
                      year = 2021)

household_size <- household_size[,c('GEOID','estimate')]
setnames(household_size, c('estimate'), c('Householdsize'))

###############*******################
#vehicles owned
#### Estimate!!Total:!!No vehicle available	HOUSEHOLD SIZE BY VEHICLES AVAILABLE
#### by occupied housing units
no_vehicle <- get_acs(geography = "tract", state = '36',
                      variables = c(no_vehicle = "B08201_002"), 
                      year = 2021)

no_vehicle <- no_vehicle[,c('GEOID','estimate')]
setnames(no_vehicle, c('estimate'), c('No_vehicle'))


#############********################
rows_with_no_insurance <- v21[grep("No health insurance coverage", v21$label), ]
sub_no_insurance <- rows_with_no_insurance[rows_with_no_insurance$concept == "HEALTH INSURANCE COVERAGE STATUS BY SEX BY AGE",]
nohealthinsurance_groups <- sub_no_insurance$name

health_insurance <- get_acs(geography = "tract", state = '36',
                      variables = c(health_insurance = "B27001_005"), 
                      year = 2021)
health_insurance <- health_insurance[,c('GEOID','estimate')]

for (a_i in nohealthinsurance_groups[2:18])
{
  insurance_temp <- get_acs(geography = 'tract', state = '36',
                             variables = c(population_temp = a_i),
                             year = 2021)    
  insurance_temp <- insurance_temp[,c('GEOID','estimate')]
  health_insurance$estimate <- health_insurance$estimate + insurance_temp$estimate
}

setnames(health_insurance, c('estimate'), c('no_health_insurance'))

#############********################

#combine
zipcodedataNY <- population

zipcodedataNY <- merge(zipcodedataNY, black, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, hispanic, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, householdincome, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, bachelor, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, household_num, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, ageunder5, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age5to9, by="GEOID")
zipcodedataNY  <- merge(zipcodedataNY, age10to14, by="GEOID")
zipcodedataNY  <- merge(zipcodedataNY, age15to24, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age25to44, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age45to64, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age65to74, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age75plus, by="GEOID")


zipcodedataNY <- merge(zipcodedataNY, no_vehicle, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, health_insurance, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, household_size, by="GEOID")

fileName = c('../data/tract_data2021.csv')
write.csv(zipcodedataNY, file = fileName, row.names=FALSE)
