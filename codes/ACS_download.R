if(!requireNamespace("tidycensus", quietly = TRUE)) install.packages("tidycensus")
if(!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

library(tidycensus)
library(tidyverse)
library(data.table)

census_api_key("919b37e63df029d1420900f893770f49d4a03226") #,install = TRUE
v19 <- load_variables(2019, "acs5", cache = TRUE)
View(v20)
write_csv(v19,'variable_namesv19.csv')

# population: B25026_001, Estimate!!Total population in occupied housing units:
population <- get_acs(geography = 'tract', state = '36', survey = "acs5",
                      variables = c(population = "B25026_001"), 
                      year = 2020)

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
  'B01001G_001'
)

population <- get_acs(geography = 'tract', state = '36',
                      variables = c(employment = 'B01001A_001'),
                      year = 2020)
population<- population[,c('GEOID','estimate')]

for (a_i in race_group_popu)
{
  population_temp <- get_acs(geography = 'tract', state = '36',
                             variables = c(population_temp = a_i),
                             year = 2020)    
  population_temp <- population_temp[,c('GEOID','estimate')]
  population$estimate <- population$estimate + population_temp$estimate
}

setnames(population, c('estimate'), c('Population'))


###############*******################
#no population density
## two categories: 1) 1.00 or less occupants per room, 
## 2) 1.01 or more occupants per room
##  add all the following groups together to get the total number of people in each category
# White alone
# Black or African American Alone
# American Indian and Alaska Native Alone
# Asian Alone
# Native Hawaiian and Other Pacific Islander Alone
# Some Other Race Alone
# Two or More Races
#### note this should not included(# White Alone, Not Hispanic or Latino
# Hispanic or Latino)
####  Estimate!!Total:!!1.00 or less occupants per room
person_per_room1 <- get_acs(geography = 'tract', state = '36',
                            variables = c(person_per_room1 = "B25014A_002"),
                            year = 2020)
person_per_room1 <- person_per_room1[,c('GEOID','estimate')]

for (race in c('B25014B_002', "B25014C_002","B25014D_002","B25014E_002", "B25014F_002", "B25014G_002"))
{
  person_per_room1_temp <- get_acs(geography = 'tract', state = '36',
                                   variables = c(person_per_room1_temp = race),
                                   year = 2020)    
  person_per_room1_temp <- person_per_room1_temp[,c('GEOID','estimate')]
  person_per_room1$estimate <- person_per_room1$estimate + person_per_room1_temp$estimate
}
setnames(person_per_room1, c('estimate'), c('person_per_room1'))

### Estimate!!Total:!!1.01 or more occupants per room
person_per_room2 <- get_acs(geography = 'tract', state = '36',
                            variables = c(person_per_room1 = "B25014A_003"),
                            year = 2020)
person_per_room2 <- person_per_room2[,c('GEOID','estimate')]

for (race in c('B25014B_003', "B25014C_003","B25014D_003","B25014E_003", "B25014F_003", "B25014G_003"))
{
  person_per_room2_temp <- get_acs(geography = 'tract', state = '36',
                                   variables = c(person_per_room1_temp = race),
                                   year = 2020)    
  person_per_room2_temp <- person_per_room2_temp[,c('GEOID','estimate')]
  person_per_room2$estimate <- person_per_room2$estimate + person_per_room2_temp$estimate
}
setnames(person_per_room2, c('estimate'), c('person_per_room2'))

###############*******################
#B01001B_001 Estimate!!Total: SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)
black <- get_acs(geography = "tract", state = '36',
                 variables = c(black = "B02001_003"), 
                 
                 year = 2020)
black <- black[,c('GEOID','estimate')]
setnames(black, c('estimate'), c('Black'))

###############*******################
#Hispanic B01001I_001
# B03001_003, Estimate!!Total:!!SEX BY AGE (HISPANIC OR LATINO)
hispanic <- get_acs(geography = "tract", state = '36',
                    variables = c(hispanic = "B01001I_001"), 
                    
                    year = 2020)

hispanic <- hispanic[,c('GEOID','estimate')]
setnames(hispanic, c('estimate'), c('Hispanic'))

###############*******################
#Asian 
# Estimate!!Total: is adding  all the following groups together to get the total number of people in each category
# Estimate!!Total:!!Asian Indian
# Estimate!!Total:!!Bangladeshi
# Estimate!!Total:!!Bhutanese
# Estimate!!Total:!!Burmese
# Estimate!!Total:!!Cambodian
# Estimate!!Total:!!Chinese, except Taiwanese
# Estimate!!Total:!!Filipino
# Estimate!!Total:!!Hmong
# Estimate!!Total:!!Indonesian
# Estimate!!Total:!!Japanese
# Estimate!!Total:!!Korean
# Estimate!!Total:!!Laotian
# Estimate!!Total:!!Malaysian
# Estimate!!Total:!!Mongolian
# Estimate!!Total:!!Nepalese
# Estimate!!Total:!!Okinawan
# Estimate!!Total:!!Pakistani
# Estimate!!Total:!!Sri Lankan
# Estimate!!Total:!!Taiwanese
# Estimate!!Total:!!Thai
# Estimate!!Total:!!Vietnamese
# Estimate!!Total:!!Other Asian, specified
# Estimate!!Total:!!Other Asian, not specified
# Estimate!!Total:!!Two or more Asian
asian <- get_acs(geography = 'tract', state = '36',
                 variables = c(Asian = "B02015_001"),
                 
                 year = 2020)
asian <- asian[,c('GEOID','estimate')]
setnames(asian, c('estimate'), c('Asian'))


###############*******################
#median household income
# Estimate!!Total:
#householdincome: MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2020 INFLATION-ADJUSTED DOLLARS) BY HOUSEHOLD SIZE
householdincome <- get_acs(geography = "tract", state = '36',
                           variables = c(householdincome = "B19019_001"), 
                           
                           year = 2020)
householdincome <- householdincome[,c('GEOID','estimate')]
setnames(householdincome, c('estimate'), c('Householdincome'))

###############*******################
#bachelor’s degree
#bachelor: B06009_005, Estimate!!Total:!!Bachelor's degree
bachelor <- get_acs(geography = "tract", state = '36',
                    variables = c(bachelor = "B06009_005"), 
                    
                    year = 2020)
bachelor <- bachelor[,c('GEOID','estimate')]
setnames(bachelor, c('estimate'), c('Bachelor'))

###############*******################
#household size
#householdsize: B25010_001, Estimate!!Average household size --!!Total:
## 2020 does not have this variable， we can use the population/the nunber of households to get the average household size  B08201_001 = B08202_001 adding up all the household size breakdowns.
households <- get_acs(geography = "tract", state = '36',
                      variables = c(householdsize = "B08202_001"), 
                      
                      year = 2020)

households <- households[,c('GEOID','estimate')]
setnames(households, c('estimate'), c('Households_num'))

#employment
## add up all the age groups of female and males.
## cilivian labor force: not inlude the army forces 
age_group <- c( 'B23001_014',  'B23001_021',
                'B23001_028',
                'B23001_035',
                'B23001_042',
                'B23001_049',
                'B23001_056',
                'B23001_063',
                'B23001_070',
                'B23001_075',
                'B23001_080',
                'B23001_085',
                'B23001_093',
                'B23001_100',
                'B23001_107',
                'B23001_114',
                'B23001_121',
                'B23001_128',
                'B23001_135',
                'B23001_142',
                'B23001_149',
                'B23001_156',
                'B23001_161',
                'B23001_166',
                'B23001_171')
employment <- get_acs(geography = 'tract', state = '36',
                      variables = c(employment = 'B23001_007'),
                      year = 2020)
employment<- employment[,c('GEOID','estimate')]

for (a_i in age_group)
{
  employment_temp <- get_acs(geography = 'tract', state = '36',
                             variables = c(employment_temp = a_i),
                             
                             year = 2020)    
  employment_temp <- employment_temp[,c('GEOID','estimate')]
  employment$estimate <- employment$estimate + employment_temp$estimate
}

setnames(employment, c('estimate'), c('Employment'))

#urban/rural(?)
##Estimate!!Total:!!Public transportation (excluding taxicab):
public_transportation <- get_acs(geography = "tract", state = '36',
                                 variables = c(householdsize = "B08006_008"), 
                                 
                                 year = 2020)

public_transportation <- public_transportation[,c('GEOID','estimate')]
setnames(public_transportation, c('estimate'), c('Public_transportation'))

###############*******################
#vehicles owned
#### Estimate!!Total:!!No vehicle available	HOUSEHOLD SIZE BY VEHICLES AVAILABLE
#### by occupied housing units
no_vehicle <- get_acs(geography = "tract", state = '36',
                      variables = c(householdsize = "B08201_002"), 
                      
                      year = 2020)

no_vehicle <- no_vehicle[,c('GEOID','estimate')]
setnames(no_vehicle, c('estimate'), c('No_vehicle'))

##### 1 or more vehicles available by occupied housing units
vehicle_owned <- get_acs(geography = 'tract', state = '36',
                         variables = c(vehicle_owned = "B08201_003"),
                         year = 2020)
vehicle_owned <- vehicle_owned[,c('GEOID','estimate')]

for (race in c('B08201_004', "B08201_005","B08201_006"))
{
  vehicle_owned_temp <- get_acs(geography = 'tract', state = '36',
                                variables = c(vehicle_owned_temp = race),
                                year = 2020)    
  vehicle_owned_temp <- vehicle_owned_temp[,c('GEOID','estimate')]
  vehicle_owned$estimate <- vehicle_owned$estimate + vehicle_owned_temp$estimate
}
setnames(vehicle_owned, c('estimate'), c('Vehicle_owned'))


###############*******################
# age groups
### ageunder5
ageunder5_groups <- c(
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
  'B01001G_018'
)
ageunder5 <- get_acs(geography = 'tract', state = '36',
                     variables = c(ageunder5 = "B01001A_003"),
                     year = 2020)
ageunder5 <- ageunder5[,c('GEOID','estimate')]

for (g in ageunder5_groups)
{
  ageunder5_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(ageunder5_temp = g),
                            year = 2020)    
  ageunder5_temp <- ageunder5_temp[,c('GEOID','estimate')]
  ageunder5$estimate <- ageunder5$estimate + ageunder5_temp$estimate
}
setnames(ageunder5, c('estimate'), c('Ageunder5'))

##age5to9
age5to9_groups <- c(
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
age5to9 <- get_acs(geography = 'tract', state = '36',
                   variables = c(age5to9 = "B01001A_004"),
                   
                   year = 2020)
age5to9 <- age5to9[,c('GEOID','estimate')]
for (g in age5to9_groups)
{
  age5to9_temp <- get_acs(geography = 'tract', state = '36',
                          variables = c(age5to9_temp = g),
                          
                          year = 2020)    
  age5to9_temp <- age5to9_temp[,c('GEOID','estimate')]
  age5to9$estimate <- age5to9$estimate + age5to9_temp$estimate
}
setnames(age5to9, c('estimate'), c('Age5to9'))

age10to14_groups <- c(
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
  'B01001G_020'
)
age10to14 <- get_acs(geography = 'tract', state = '36',
                     variables = c(age10to14 = 'B01001A_005'),
                     
                     year = 2020)
age10to14 <- age10to14[,c('GEOID','estimate')]
for (g in age10to14_groups)
{
  age10to14_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age10to14_temp = g),
                            
                            year = 2020)    
  age10to14_temp <- age10to14_temp[,c('GEOID','estimate')]
  age10to14$estimate <- age10to14$estimate + age10to14_temp$estimate
}
setnames(age10to14, c('estimate'), c('Age10to14'))

age15to24_groups <- c(
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
  'B01001G_021',
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
  'B01001G_022',
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

age15to24 <- get_acs(geography = 'tract', state = '36',
                     variables = c(age15to24 = 'B01001A_006'), #'B01001A_006' is the first group of age15to24
                     year = 2020)
age15to24 <- age15to24[,c('GEOID','estimate')]
for (g in age15to24_groups)
{
  age15to24_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age15to24_temp = g),
                            year = 2020)    
  age15to24_temp <- age15to24_temp[,c('GEOID','estimate')]
  age15to24$estimate <- age15to24$estimate + age15to24_temp$estimate
}
setnames(age15to24, c('estimate'), c('Age15to24'))

age25to44_groups <- c(
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
  'B01001G_024',
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
  'B01001G_025',
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
  'B01001G_026'
)
age25to44 <- get_acs(geography = 'tract', state = '36',
                     variables = c(age25to44 = 'B01001A_009'), #'B01001A_009' is the first group of age25to44
                     year = 2020)
age25to44 <- age25to44[,c('GEOID','estimate')]
for (g in age25to44_groups)
{
  age25to44_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age25to44_temp = g),
                            year = 2020)    
  age25to44_temp <- age25to44_temp[,c('GEOID','estimate')]
  age25to44$estimate <- age25to44$estimate + age25to44_temp$estimate
}
setnames(age25to44, c('estimate'), c('Age25to44'))

age45to64_groups<-c(
  'B01001A_013',
  'B01001A_027',
  'B01001A_028',
  'B01001B_012',
  'B01001B_013',
  'B01001B_027',
  'B01001B_028',
  'B01001C_012',
  'B01001C_013',
  'B01001C_027',
  'B01001C_028',
  'B01001D_012',
  'B01001D_013',
  'B01001D_027',
  'B01001D_028',
  'B01001E_012',
  'B01001E_013',
  'B01001E_027',
  'B01001E_028',
  'B01001F_012',
  'B01001F_013',
  'B01001F_027',
  'B01001F_028',
  'B01001G_012',
  'B01001G_013',
  'B01001G_027',
  'B01001G_028'
)
age45to64 <- get_acs(geography = 'tract', state = '36',
                     variables = c(age45to64 = 'B01001A_012'), #'B01001A_012' is the first group of age45to64
                     year = 2020)
age45to64 <- age45to64[,c('GEOID','estimate')]
for (g in age45to64_groups)
{
  age45to64_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age45to64_temp = g),
                            year = 2020)    
  age45to64_temp <- age45to64_temp[,c('GEOID','estimate')]
  age45to64$estimate <- age45to64$estimate + age45to64_temp$estimate
}
setnames(age45to64, c('estimate'), c('Age45to64'))

age65to74_groups <- c(
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
  'B01001G_029'
)
age65to74 <- get_acs(geography = 'tract', state = '36',
                     variables = c(age65to74 = 'B01001A_014'), #'B01001A_014' is the first group of age65to74
                     year = 2020)
age65to74 <- age65to74[,c('GEOID','estimate')]
for (g in age65to74_groups)
{
  age65to74_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age65to74_temp = g),
                            year = 2020)    
  age65to74_temp <- age65to74_temp[,c('GEOID','estimate')]
  age65to74$estimate <- age65to74$estimate + age65to74_temp$estimate
}
setnames(age65to74, c('estimate'), c('Age65to74'))

age75plus_groups<-c(
  'B01001A_016',
  'B01001A_030',
  'B01001A_031',
  'B01001B_015',
  'B01001B_016',
  'B01001B_030',
  'B01001B_031',
  'B01001C_015',
  'B01001C_016',
  'B01001C_030',
  'B01001C_031',
  'B01001D_015',
  'B01001D_016',
  'B01001D_030',
  'B01001D_031',
  'B01001E_015',
  'B01001E_016',
  'B01001E_030',
  'B01001E_031',
  'B01001F_015',
  'B01001F_016',
  'B01001F_030',
  'B01001F_031',
  'B01001G_015',
  'B01001G_016',
  'B01001G_030',
  'B01001G_031'
)
age75plus <- get_acs(geography = 'tract', state = '36',
                     variables = c(age75plus = 'B01001A_015'), #'B01001A_015' is the first group of age75plus
                     year = 2020)
age75plus <- age75plus[,c('GEOID','estimate')]
for (g in age75plus_groups)
{
  age75plus_temp <- get_acs(geography = 'tract', state = '36',
                            variables = c(age75plus_temp = g),
                            year = 2020)    
  age75plus_temp <- age75plus_temp[,c('GEOID','estimate')]
  age75plus$estimate <- age75plus$estimate + age75plus_temp$estimate
}
setnames(age75plus, c('estimate'), c('Age75plus'))



#combine
zipcodedataNY <- population

zipcodedataNY <- merge(zipcodedataNY, black, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, hispanic, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, asian, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, householdincome, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, bachelor, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, households, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, ageunder5, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age5to9, by="GEOID")
zipcodedataNY  <- merge(zipcodedataNY, age10to14, by="GEOID")
zipcodedataNY  <- merge(zipcodedataNY, age15to24, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age25to44, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age45to64, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age65to74, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, age75plus, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, employment, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, no_vehicle, by="GEOID")
zipcodedataNY <- merge(zipcodedataNY, vehicle_owned, by="GEOID")

zipcodedataNY <- merge(zipcodedataNY, public_transportation, by="GEOID")

# zipcodedataNY <- merge(zipcodedataNY, person_per_room1, by="GEOID")
# zipcodedataNY <- merge(zipcodedataNY, person_per_room2, by="GEOID")



path_out =  "~/OneDrive - Columbia University Irving Medical Center/0_behaviours_mobility_epidemics/Data/"

fileName = paste(path_out, 'tract_data2020.csv',sep = '')

write.csv(zipcodedataNY, file = fileName, row.names=FALSE)
