#' @name IntraHHMaths
#'
#' @title Mathematical model to assess the impact of different levels of unequal intra-household food distribution within a household
#'
#' @param HCES Household Consumption Survey
#'
#' @param FCT Food Composition Table
#'
#' @param target_group target group of interest: WRA, PSC, SAC, MEN
#'
#' @details This model has been simulated on hypothetical data.
#'
#' @import #packages
#'
#' @return model
#'
#' @export

# Function definitions (as used in main function)

## Data Ingestion and initial calculations
# DONE Step 1a: Input datasets (HCES & FCT)
# DONE Step 1b: Merge datasets together on food sub-group
# TODO Step 2a: Select eligible HHs
# TODO Step 2b: Calculate amount of food eaten according to AME/AFE
# DONE Step 3: Calculate total energy consumed from food per day
# DONE Step 4: Create energy_breakdown_table (shows how much energy comes from each food sub-group)

## Increase decrease foods
# DONE Step 5: Create increase/decrease matrix
# DONE 1) Increase food-group (Increase 1 food group a time (Max 1 input), rest stay the same)
# DONE 2) Decrease food-group (Decrease 1 food group (max 1 input), rest stays the same)
# DONE 3) Increase 1 food group AND decrease 1 food group, rest stays same - legumes/cereal example)
# DONE Step 6: Create food_portions_ratio matrix for all increase and decrease ratios (0 - 100%)
# DONE Crete parameter to calculate values (Equal, 10%, 20% ... 100%)
# DONE Step 7: Create adjusted_grams_food matrix
# Step 8: Create adjusted_dietary_intakes for each micronutrient
# Step 9: TO DISCUSS: calculate the number of HH deficient / the cut off point / curve

readData <- function(HCES, FCT) {
  HCES <- read.csv(HCES, fileEncoding = "UTF-8-BOM")
  FCT <- read.csv(FCT, fileEncoding = "UTF-8-BOM")
  dataset <- merge(HCES, FCT, by = "food_code")
  return(dataset)
}

#function to use AFE/AME based on target group.
# if WRA/PSC == F/ SAC == F use AFE,
# else if MEN, PSC == M/ SAC == M
# else (stop) "AFE/AME cannot be calculated for this target group"

# calcConsumedOne is called if one parameter is given: food_group_adjust
calcConsumedOne <- function(dataset, food_group_adjust) {
  dataset[ ,"energy_consumed_food_pd"] <- dataset[ ,"amount_eaten_per_day"] / 100 * dataset[ ,"energy_kcal"]
  total_energy <- sum(dataset[ ,"energy_consumed_food_pd"])
  energy_food_group_adjust <- sum(dataset[which(dataset[ ,"food_group"]== food_group_adjust), "energy_consumed_food_pd"])
  energy_food_group_compensate <- 0
  energy_remaining_food <- (total_energy - energy_food_group_adjust - energy_food_group_compensate)
  energy_breakdown_table <- data.frame(total_energy,
                                       energy_food_group_adjust,
                                       energy_food_group_compensate,
                                       energy_remaining_food)
  return(list(dataset = dataset, energy_breakdown_table = energy_breakdown_table))
}

# calcConsumedTwo is called if two parameters are given: food_group_adjust, food_group_compensate
calcConsumedTwo <- function(dataset, food_group_adjust, food_group_compensate) {
  dataset[ ,"energy_consumed_food_pd"] <- dataset[ ,"amount_eaten_per_day"] / 100 * dataset[ ,"energy_kcal"]
  total_energy <- sum(dataset[ ,"energy_consumed_food_pd"])
  energy_food_group_adjust <- sum(dataset[which(dataset[ ,"food_group"]== food_group_adjust), "energy_consumed_food_pd"])
  energy_food_group_compensate <- sum(dataset[which(dataset[ ,"food_group"]== food_group_compensate), "energy_consumed_food_pd"])
  energy_remaining_food <- (total_energy - energy_food_group_adjust - energy_food_group_compensate)
  energy_breakdown_table <- data.frame(total_energy,
                                       energy_food_group_adjust,
                                       energy_food_group_compensate,
                                       energy_remaining_food)
  return(list(dataset = dataset, energy_breakdown_table = energy_breakdown_table))
}

# ifelse depending on one or two parameters modelled
calcConsumed <- function(dataset, food_group_adjust, food_group_compensate) {
  if(is.null(food_group_compensate)){
    calcConsumedOne(dataset, food_group_adjust)
  } else if (!is.null(food_group_compensate)) {
    calcConsumedTwo(dataset, food_group_adjust, food_group_compensate)
  }
}

incdecGramsOne <- function(dataset, energy_breakdown_table, n, food_group_adjust, incdec){
  if (incdec == 'increase') {
    text <- paste0("Increase ")
    formula <- (1 + n/100)
  } else if (incdec == 'decrease') {
    text <- paste0("Decrease ")
    formula <- (1 - n/100)
  }
  column_text <- paste0(text, food_group_adjust, " by ", n, "%")
  increase_decrease <- energy_breakdown_table$energy_food_group_adjust * formula
  adjusted_energy <- energy_breakdown_table$total_energy - increase_decrease
  ratio <- adjusted_energy / energy_breakdown_table$energy_remaining_food
  adjusted_grams <- ifelse(dataset$food_group == food_group_adjust,
                           dataset$amount_eaten_per_day * formula,
                           dataset$amount_eaten_per_day * ratio)
  return(list(column_text = column_text, adjusted_grams = adjusted_grams))
}

incdecGramsTwo <- function(dataset, energy_breakdown_table, n, food_group_adjust, food_group_compensate, incdec){
  if (incdec == 'increase') {
    text <- paste0("Increase ")
    formula <-  (1 + n/100)
    column_text <- paste0(text, food_group_adjust, " by ", n, "% ", "& prop. decrease " , food_group_compensate)
  } else if (incdec == 'decrease') {
    text <- paste0("Decrease ")
    formula <- (1 - rev(n/100))
    column_text <- paste0(text, food_group_adjust, " by ", n, "% ", "& prop. increase " , food_group_compensate)
  }
  increase_decrease <- energy_breakdown_table$energy_food_group_adjust * formula
  adjusted_energy <- energy_breakdown_table$total_energy - increase_decrease - energy_breakdown_table$energy_remaining_food
  if (incdec == 'increase') {
    ratio <- adjusted_energy / energy_breakdown_table$energy_food_group_compensate
  } else if (incdec == 'decrease') {
    ratio <- adjusted_energy / energy_breakdown_table$energy_food_group_compensate
  }
  adjusted_grams <- ifelse(dataset$food_group == food_group_adjust,
                           dataset$amount_eaten_per_day * formula,
                           ifelse(dataset$food_group == food_group_compensate,
                                  dataset$amount_eaten_per_day * ratio,
                                  dataset$amount_eaten_per_day))
  return(list(column_text = column_text, adjusted_grams = adjusted_grams))
}

calcGramsInc <- function(dataset, energy_breakdown_table, n, food_group_adjust, food_group_compensate, incdec) {
  if(is.null(food_group_compensate)){
    lapply (n,
            incdecGramsOne,
            incdec = "increase",
            food_group_adjust = food_group_adjust,
            dataset = dataset,
            energy_breakdown_table = energy_breakdown_table)
  } else if (!is.null(food_group_compensate)) {
    lapply (n,
            incdecGramsTwo,
            incdec = "increase",
            food_group_adjust = food_group_adjust,
            food_group_compensate,
            dataset = dataset,
            energy_breakdown_table = energy_breakdown_table)
  }
}

calcGramsDec <- function(dataset, energy_breakdown_table, n, food_group_adjust, food_group_compensate, incdec) {
  if(is.null(food_group_compensate)){
    lapply (n,
            incdecGramsOne,
            incdec = "decrease",
            food_group_adjust = food_group_adjust,
            dataset = dataset,
            energy_breakdown_table = energy_breakdown_table)
  } else if (!is.null(food_group_compensate)) {
    lapply (n,
            incdecGramsTwo,
            incdec = "decrease",
            food_group_adjust = food_group_adjust,
            food_group_compensate,
            dataset = dataset,
            energy_breakdown_table = energy_breakdown_table)
  }
}

repTable <- function(list_result){
  list_result[[1]]$column_text <- "Equitable household food distribution"
  grams_calcs <- as.data.frame(matrix(data = unlist(list_result), nrow = length(list_result[[1]]$adjusted_grams) +1, ncol = length(list_result), byrow = FALSE, dimnames = NULL))
  names(grams_calcs) <- grams_calcs[1, ]
  grams_calcs <- grams_calcs[-1, ]
  return(grams_calcs)
}

gramsDF <- function(dataset, list_result_increase, list_result_decrease, increase_decrease){
if (increase_decrease == 'both'){
  grams_table_increase <- repTable(list_result_increase)
  grams_table_decrease <- repTable(list_result_decrease)
  grams_table_list <- merge(grams_table_increase, grams_table_decrease, by="Equitable household food distribution")
} else if (increase_decrease == 'increase'){
  grams_table_list <- repTable(list_result_increase)
}  else if (increase_decrease == 'decrease'){
  grams_table_list <- repTable(list_result_decrease)
} else {
  stop("increase_decrease parameter must be either 'increase', 'decrease' or 'both'")
}
grams_numeric <- sapply(grams_table_list, as.numeric)
grams_table_prep <- as.data.frame(grams_numeric)
food_description <- dataset[,c("food_item", "amount_eaten_per_day")]
grams_table_m <- merge(grams_table_prep, food_description , by.x = "Equitable household food distribution", by.y = "amount_eaten_per_day")
grams_table <- grams_table_m[,c(ncol(grams_table_m),1:(ncol(grams_table_m)-1))]
return(grams_table)
}

# Adjusted dietary intakes:
fct_dataset <- merge(grams_table, dataset, by = "food_item")

### apply function to each dataset
# one row

fct_dataset[1 , "Equitable household food distribution"] / 100 * fct_dataset[1 , micronutrient]
micronutrient <- "PROTEIN_G"
percentage <- "Equitable household food distribution"

test <- fct_dataset[ , c(2:4, 32:34)]

product <- function(micronutrient, percentage, fct_dataset){
  A <- fct_dataset[percentage]
  # accessing elements from first column
  B <- fct_dataset[micronutrient]
  # return product
  result <- sum((A / 100) * B)
  return(list(micronutrient, percentaresult)
}

sum(product(percentage, micronutrient, fct_dataset))


apply(df, 1 , product)









# Main IntraHHMaths function
IntraHHMaths <- function(HCES,
                         FCT,
                         # target_group
                         food_group_adjust,
                         food_group_compensate = NULL,
                         increase_decrease = 'both',
                         model_increments,
                         model_max_value,
                         ...) {

  dataset <- readData(HCES, FCT)
  initial_calculations <- calcConsumed(dataset, food_group_adjust, food_group_compensate)
  dataset <- initial_calculations$dataset
  energy_breakdown_table <-initial_calculations$energy_breakdown_table
  n <- seq(0, model_max_value, model_increments)
  list_result_increase <- calcGramsInc(dataset, energy_breakdown_table, n,
                                         food_group_adjust, food_group_compensate)
  list_result_decrease <- calcGramsDec(dataset, energy_breakdown_table, n,
                                         food_group_adjust, food_group_compensate)
  grams_table <- gramsDF(dataset, list_result_increase, list_result_decrease, increase_decrease)
  return(list(dataset = dataset,
              energy_breakdown_table = energy_breakdown_table,
              grams_table = grams_table))
}


# Function call examples:

# Scenario 1 - Starchy Staples (food_group_compensate parameter empty)
# model <- IntraHHMaths(HCES = "../data/HCES.csv",
#                       FCT = "../data/FCT_changed.csv",
#                       # target_group = "WRA"
#                       food_group_adjust = "Starchy Staples",
#                       increase_decrease = 'both',
#                       model_increments = 10,
#                       model_max_value = 1000)
#
# dataset <- model$dataset
# energy_breakdown_table <- model$energy_breakdown_table
# grams_table <- model$grams_table


# Scenario 2 - Legumes (food_group_compensate parameter empty)
# model <- IntraHHMaths(HCES = "../data/HCES.csv",
#                       FCT = "../data/FCT.csv",
#                       # target_group = "WRA"
#                       food_group_adjust = "Legumes",
#                       increase_decrease = 'both',
#                       model_increments = 10,
#                       model_max_value = 100)
#
# dataset <- model$dataset
# energy_breakdown_table <- model$energy_breakdown_table
# grams_table <- model$grams_table


# Scenario 3 - food_group_compensate two parameters given:
model <- IntraHHMaths(HCES = "../data/HCES.csv",
                      FCT = "../data/FCT.csv",
                      # target_group = "WRA"
                      food_group_adjust = "Legumes",
                      food_group_compensate = "Starchy Staples",
                      increase_decrease = 'both',
                      model_increments = 10,
                      model_max_value = 100)

dataset <- model$dataset
energy_breakdown_table <- model$energy_breakdown_table
grams_table <- model$grams_table


# # # Testing purposes - Scenario 1
# HCES = "../data/HCES.csv"
# FCT = "../data/FCT_changed.csv"
# food_group_adjust <- "Starchy Staples"
# food_group_compensate <- NULL
# increase_decrease = 'both'
# model_increments <- 10
# model_max_value <- 100

# # Testing purposes - Scenario 2
# HCES = "../data/HCES.csv"
# FCT = "../data/FCT.csv"
# food_group_adjust <- "Legumes"
# food_group_compensate <- NULL
# increase_decrease = 'both'
# model_increments <- 10
# model_max_value <- 100
#
# Testing purposes - Scenario 3
# HCES <- "../data/HCES.csv"
# FCT <- "../data/FCT.csv"
# food_group_adjust <- "Legumes"
# food_group_compensate <- "Starchy Staples"
# increase_decrease <- 'both'
# model_increments <- 10
# model_max_value <- 100
