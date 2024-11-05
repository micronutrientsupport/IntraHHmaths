# IntraHHmath calculations
Hypothetical analysis on the impact of unequal intra-household food distribution on programmatic or policy decisions. 

## What is the model about? 

### Research questions: 
- what percentage of vulnerable groups are at risk, if the individual deviates from the equitable intra-household food distribution? 
- to what extent can the results of these analyses be generalised contexts within a country, and across different countries?
- to what extent does the assumption of equal intra-household food distribution result in incorrect policy or programmatic decisions for the vulnerable groups in question?

### The model: 

 - Step 1: Define our target group, i.e. adolescent girls or women of reproductive Age (WRA).
 - Step 2. Select eligible households that have a member from the target group of interest. Currently the model selects HHs that have consumed the particular food group that is being modelled along with selecting HHs which feature only member of the target group of interest (WRA) in the dataset. 
 - Step 3: Calculate the adult equivalent (AFE/AME) assuming equal intrahousehold food distribution. Since we are primarily focused on the female groups, the Adult Female Equivalent (AFE) has been used in the calculations, rather than the Adult Male Equivalent (AME). 
 - Step 4: Keeping the total amount of energy constant, we increase and/or decrease the amount of energy provided by a food sub-group (i.e. starchy staples) and compensate the remaining energy by one or all other food groups.
 
 ![alt text]( https://github.com/micronutrientsupport/IntraHHmaths/blob/main/increase_decrease_relationship.png) 
   
 - Step 5: Calculate the adjusted daily intakes of energy and nutrients

From here one can then use the output generated from the model to calculate the percentage of the target group with inadequate intakes, model different scenarios of interest and/or repeat the analyses for different vulnerable groups and contexts of interests. 

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

#### 2.2. It is imperative that you provide the following files in the data directory to run the model successfully: 
1. household roster file, i.e. "HH_MOD_B.dta.dta", which contains your survey data in .dta format. For example: https://microdata.worldbank.org/index.php/catalog/3818/data-dictionary/F5?file_name=HH_MOD_D.dta
2. household food consumption file, i.e. "IHS5_relabelled.csv" Please see the example entitled 'data_requirements_for_model.csv' in the data directory.
3. hh_pregnant_ids.csv, This is a csv file which requires you to provide the household ids (HHID) of the household with pregnant individuals in it. Please see the example in the data directory.
4. roster_columns_required.csv. This is a csv file provided in the "data" directory which requires you to fill in the "survey_variable_names" column ONLY. Please fill in this first column with column names/variables which equal your survey which
equal 'sex', 'age in years' (age_y) and 'age in months' (age_m).
5. IHS5_AME_FACTORS_vMAPS.csv (no need to modify)
6. IHS5_AME_SPEC_vMAPS.csv (no need to modify)

## 3. Running the script in R/R Studio:

#### 3.1. Place the aforementioned data files in the ("data") directory, see section 2.2.
#### 3.2. Set the working directory to the R script location ("R/IntraHHMaths.R")
#### 3.3. Run the R script ("R/IntraHHMaths.R")

You will be able to run the model!
