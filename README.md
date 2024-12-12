# behaviours_epidemics

## Description
Human mobility can go through dramatic changes. This may be a gradual process as technological advances shift travel needs or because of economic and natural environments change. However, they can also shift suddenly such as during public emergencies like the COVID-19 pandemic. The latter types of changes in movement play a crucial role in shaping critical outcomes such as disease transmission dynamics, economic trends, and access to critical services (such as employment, education, and healthcare). The well-documented impact of intervention policies on mobility and recent advancements in the appreciation of individual choice and decision agency indicate an opportunity to deepen our understanding of human mobility dynamics during major societal disruptions. Specifically, we can examine how particular behavioral patterns and decision-making agency influence mobility during such crises and how this may inform more effective policy responses during these major shifts. 
We approached this opportunity by exploring the interplay between decision preferences and mobility patterns during a recent, sudden, near-universal shift in human behavior: the COVID-19 pandemic in New York City (NYC). We surveyed N = 1049 residents from 17 intentionally selected zip codes, capturing three key behavioral and choice preferences in decision-making: temporal discounting, loss aversion, and agency. In tandem, population mobility in 2020 was measured for six different “place” categories (grocery stores and pharmacies, general retail, arts and entertainment venues, restaurants and bars, educational settings, healthcare facilities) using mobile phone-derived foot-traffic data. 
In this paper, we also introduce and validate a newly constructed agency score, which captures both individual perceived subjective autonomy and objective constraint. Though agency is increasingly considered in behavioral research that informs public policy, this distinction has not yet been widely considered in public policy despite its utility. 
Spatial distribution of the three behavioral preferences revealed distinct patterns across zip codes during the COVID-19 pandemic. In the novel agency score, we analyze what we classify as objective and subjective agency of nine services and identify neighborhoods with reduced access to certain services due to resource constraints. Extended regression models indicate that decision agency and choice preferences were significantly correlated with zip code-level per capita visits to different place categories in 2020, accounting for disease spread, policy stringency, and socioeconomic variables. Specifically, residents in zip codes with higher temporal discounting and loss aversion scores exhibited increased mobility across all six place categories, while those in zip codes with higher agency scores had reduced visitation to these places across NYC. For each model, except for the grocery and pharmacy place categories, the temporal discounting scores had the largest effect of the behavioral parameters while holding all other variables constant.
These results highlight the role of decision agency and choice preferences on human mobility, underscoring the importance of capturing these behavioral mechanisms in public health intervention strategies. These findings may also inform improved epidemic predictive models that integrate the feedback mechanism between disease transmission and behavior change.

## Requirements
This project requires both `python3` and `R` to be installed on your system.

## 1. Mobility Matrix   
The mobility matrix is a matrix that represents the movement of people between different locations. The mobility matrix is a square matrix of size `n x n` where `n` is the number of locations. The element `M[i][j]` of the matrix represents the number of people moving from location `i` to location `j`. The mobility matrix is a symmetric matrix, i.e., `M[i][j] = M[j][i]`. The diagonal elements of the matrix are zero, i.e., `M[i][i] = 0`. The sum of the elements of each row of the matrix is equal to the total number of people moving from location `i` to all other locations. The sum of the elements of each column of the matrix is equal to the total number of people moving to location `j` from all other locations. The mobility matrix is a non-negative matrix, i.e., `M[i][j] >= 0`.

Mobility matrix is extracted using Python script `mobility_matrix.py` from the SafeGraph dataset (Dewey market place). The SafeGraph dataset is a collection of anonymized location data from mobile devices. The dataset contains information about the number of people visiting different locations. The dataset is available in the form of CSV files.

The cleaned mobility matrices for seven categories of locations are stored in raw_mobility/move_7c_wk_raw_{}.csv. 

There three ways to construct the mobility matrix regarding how to map the census tract to the MODZCTA and how to deal with the loss visitors. In our main manuscript, we use the one to one mapping and ignore the number of visitors whose home cbg are unknown. It is called "1to1". Other two are: one census tract to multiple MODZCTA and the corresponding files end with "1tomore"; assigning the loss visitors to the known cbg with the known ratios and the corresponding files end with "loss_visitors".  


## 2. Behavioural Scores
Use scripts 1.1 to 1.3 to calculate the behavioral scores. Script 1.4 is used to check whether participants work in the same zip codes as their home zip codes and to count the number of the participants of each zip code.

For the temporal discounting scores, we returned "1_3_dat_unique_item.csv", loss aversion scores, we returned "dat_lossgain_scores.csv", and for the agency scores, we returned "agency_scores.csv". The *ResponseId* is the key to link the scores. They should be stored in the results folder.

The confirmation and item response theory model for the agency scores is applied in the script 1.3. 

## 3. Data integration
The data integration is done by Python script, see notebook 1.1_MODZCTA_SCORES.IPYNB, 1.2_Social_economic_census_data.inpyb and 2.3_mobility-regression_data_v5.ipynb in the folder mobility_data_analysis. 

The scripts merge the mobility matrix with the behavioural scores and social economic data (see the detail in **section 5.Other data**). The merged data is stored in the form of CSV file unpivot_merged_data_raw_{}.csv. {} is the constructing mobility matrix method.


## 4. GAM Model

The cleaned visit data merged with the score data is stored in the form of CSV unpivot_merged_data_raw_{}.csv 

The generalized additive model (GAM) is a flexible non-linear regression model that is used to model the relationship between the response variable and the predictor variables. The GAM model is an extension of the generalized linear model (GLM) where the linear predictor is replaced by a sum of smooth functions of the predictor variables. 

The main model is in the 3.2_GAM.R file. The model is used to fit the travelling out pattern by the behavioural scores, the disease progress and the social economics factors.

Before starting the fitting, script 3.3 can perform the basic data analysis to obtain the data summary table.


## 5. Other data
The social economic data is avaliable in the usa Census Bureau (https://www.census.gov/). To download the data, use the script 3.1. The data is at tract level. 
Use the mapping from mappings folder, we can group the tract data into the MODZCTA level. 

To obtain the average age of the zipcode, we use midpoint of the age range in the census data, the calculation is:
$$
\frac{\text{midpoint} \times \text{number of people in the range}}{\text{population}}
$$

The policy stringency data is avaliable in the Oxford COVID-19 Government Response Tracker (https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker).

Use python script/notebooks can merge the data.

<!-- ## 6. Random Forest Model
We use random forest model with permutation importance to evaluate the variable/feature importance of the variables we select. The model is in the 4.1_random_forest.R file. -->

 