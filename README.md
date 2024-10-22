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

#### 3.1. It is imperative that you provide the following files to run the model successfully: 
- Healthhold consumption survey data HCES (HCES.csv)
- Food composition table (FCT.csv) 
- Household roster (.dta file)
- hh_pregnant_ids.csv This is a csv file provided in the "data" directory which requires you to provide the household ids (HHID) of the household with pregnant individuals in it.
- roster_columns_required.csv. This is a csv file provided in the "data" directory which requires you to fill in the "survey_variable_names" column ONLY. Please fill in this first column with column names/variables which equal your survey which
equal 'sex', 'age in years' (age_y) and 'age in months' (age_m).
- IHS5_AME_FACTORS_vMAPS.csv (no need to modify)
- IHS5_AME_SPEC_vMAPS.csv (no need to modify)

#### 3.2. Set the working directory to the R script location ("R/IntraHHMaths.R")
#### 3.3. Run the R script ("R/IntraHHMaths.R")

```
You should be able to run the beginnings of the model!
```
