# behaviours_epidemics

## Description
Please copy paste the abstract from the MS.

## Requirements
This project requires both `python3` and `R` to be installed on your system.

## 1. Mobility Matrix   
The mobility matrix is a matrix that represents the movement of people between different locations. The mobility matrix is a square matrix of size `n x n` where `n` is the number of locations. The element `M[i][j]` of the matrix represents the number of people moving from location `i` to location `j`. The mobility matrix is a symmetric matrix, i.e., `M[i][j] = M[j][i]`. The diagonal elements of the matrix are zero, i.e., `M[i][i] = 0`. The sum of the elements of each row of the matrix is equal to the total number of people moving from location `i` to all other locations. The sum of the elements of each column of the matrix is equal to the total number of people moving to location `j` from all other locations. The mobility matrix is a non-negative matrix, i.e., `M[i][j] >= 0`.

Mobility matrix is extracted using Python script `mobility_matrix.py` from the SafeGraph dataset (Dewey market place). The SafeGraph dataset is a collection of anonymized location data from mobile devices. The dataset contains information about the number of people visiting different locations. The dataset is available in the form of CSV files.

The cleaned mobility matrices for seven categories of locations are stored in move_7c_wk_raw.csv. 


## 2. Behavioural Scores
Use scripts 1.1 to 1.3 to calculate the behavioral scores. Script 1.4 is used to check whether participants work in the same zip codes as their home zip codes and to count the number of the participants of each zip code.

For the temporal discounting scores, we returned "1_3_dat_unique_item.csv", loss aversion scores, we returned "dat_lossgain_scores.csv", and for the agency scores, we returned "agency_scores.csv". The *ResponseId* is the key to link the scores.

The confirmation and item response theory model for the agency scores is applied in the script 1.3. 

## 3. Data integration
The data integration is done by Python script, see notebook Scores_individually_combination.ipynb in the folder mobility_data_analysis. 



The scripts merge the mobility matrix with the behavioural scores and social economic data (see the detail in **section 5.Other data**). The merged data is stored in the form of CSV file unpivot_merged_data_raw_v9.csv.


## 4. GAM Model

The cleaned visit data merged with the score data is stored in the form of CSV unpivot_merged_data_raw_v9.csv 

Before starting the fitting, script 3.2 can perform the basic data analysis to obtain the data summary table.

The generalized additive model (GAM) is a flexible non-linear regression model that is used to model the relationship between the response variable and the predictor variables. The GAM model is an extension of the generalized linear model (GLM) where the linear predictor is replaced by a sum of smooth functions of the predictor variables. 

The main model is in the regression.R file. The model is used to fit the travelling out pattern by the behavioural scores, the disease progress and the social economics factors.


## 5. Other data
The social economic data is avaliable in the usa Census Bureau (https://www.census.gov/). To download the data, use the script 3.1. The data is at tract level. 
Use the mapping from (), we can group the tract data into the zipcode level. 

To obtain the average age of the zipcode, we use midpoint of the age range in the census data, the calculation is:
$$
\frac{\text{midpoint} \times \text{number of people in the range}}{\text{population}}
$$

The policy stringency data is avaliable in the Oxford COVID-19 Government Response Tracker (https://www.bsg.ox.ac.uk/research/covid-19-government-response-tracker).

Use python script/notebooks can merge the data.

## 6. Random Forest Model
We use random forest model with permutation importance to evaluate the variable/feature importance of the variables we select. The model is in the 4_random_forest.R file.

 