# behaviours_epidemics

## Description

## Requirements
This project requires both `python3` and `R` to be installed on your system.

## 1. Mobility Matrix   
The mobility matrix is a matrix that represents the movement of people between different locations. The mobility matrix is a square matrix of size `n x n` where `n` is the number of locations. The element `M[i][j]` of the matrix represents the number of people moving from location `i` to location `j`. The mobility matrix is a symmetric matrix, i.e., `M[i][j] = M[j][i]`. The diagonal elements of the matrix are zero, i.e., `M[i][i] = 0`. The sum of the elements of each row of the matrix is equal to the total number of people moving from location `i` to all other locations. The sum of the elements of each column of the matrix is equal to the total number of people moving to location `j` from all other locations. The mobility matrix is a non-negative matrix, i.e., `M[i][j] >= 0`.

Mobility matrix is extracted using Python script `mobility_matrix.py` from the SafeGraph dataset. The SafeGraph dataset is a collection of anonymized location data from mobile devices. The dataset contains information about the number of people visiting different locations. The dataset is available in the form of CSV files. 

