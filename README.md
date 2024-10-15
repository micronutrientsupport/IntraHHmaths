# IntraHHmath calculations

## Using the package

This project provides functions for the analysis of gender related differences in intra-household food distribution in a Household Consumption Expenditure Survey (HCES).  

## 1. Installation

### Download the Github repository here at: https://github.com/micronutrientsupport/IntraHHmaths

#### 1.1. Click 'Code' (Green button) 
#### 1.2. Click 'Download ZIP'
#### 1.3. Extract the zip file

## 2. Configuration

#### 2.1. Create a data directory under IntraHH-maths
Now you should have two folders under the top level 'Intra-HH maths' directory:
- R 
- data 

#### 2.2. Place your HCES and FCT data in your data directory ("IntraHH-maths/data") 

## 3. Running the script in R/R Studio:

#### 3.1. Set the working directory to the R script location ("R/IntraHHMaths.R")
#### 3.2. Run the R script ("R/IntraHHMaths.R")

You should be able to run the beginnings of the model!


## Using the package

This project provides functions for the analysis of gender related differences in intra-household food distribution in a Household Consumption Expenditure Survey (HCES).  

## 1. Installation

### Download the Github repository here at: https://github.com/micronutrientsupport/IntraHHmaths

#### 1.1. Click 'Code' (Green button) 
#### 1.2. Click 'Download ZIP'
#### 1.3. Extract the zip file

## 2. Configuration

#### 2.1. Create a data directory under IntraHH-maths
Now you should have two folders under the top level 'Intra-HH maths' directory:
- R 
- data 

#### 2.2. Place your HCES and FCT data in your data directory ("IntraHH-maths/data") 

## 3. Running the script in R/R Studio:

#### 3.1. It is imperative that you provide the following files to run the mode; 
- Food composition table (FCT.csv)
- HCES (HCES.csv)
- roster_columns_required.csv
  This is a csv file provided in the "data" directory which requires you to fill in the "survey_variable_names" column ONLY. Please fill in this column with column names/variables in your survey which equal 
 'sex', 'age in years' (age_y) and 'age in months' (age_m). 

#### 3.2. Set the working directory to the R script location ("R/IntraHHMaths.R")
#### 3.3. Run the R script ("R/IntraHHMaths.R")

You should be able to run the beginnings of the model!
