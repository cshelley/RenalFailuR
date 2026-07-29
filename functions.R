# Dependencies
library(ucimlrepo) # extract_to_bronze()
library(jsonlite)  # extract_to_bronze()
library(dplyr)     # create_gt_table()
library(gt)        # create_gt_table()



#' Extract dataset from UCI Machine Learning Repository and archive
#'
#' @param name A string. References a dataset downloadable from UCI machine
#' learning repository
#' @returns A dataset, which is a subset of input list
#' @examples
#' extract_to_bronze('Chronic Kidney Disease')
extract_to_bronze <- function(name) {
  mlr_data <- fetch_ucirepo(name = name)

  just_data = mlr_data$data
  X = just_data$features
  Y = just_data$targets

  dataset = cbind(X, Y)

  return(dataset)
}


# unit test for extract_to_bronze
class(ckd)  # test that class equals "data.frame"
dim(ckd)    # test that dimensions equals (400   25)




#' Save a bronze dataset as a JSON file
#'
#' @param datafile A data.frame
#' @returns A .json file downloaded to /RenalFailuR/data
#' @example
#' bronze_file <- extract_to_bronze('Chronic Kidney Disease')
#' bronze_to_json(bronze_file)
bronze_to_json <- function(datafile) {
  write_json(datafile,
             path = system.file("data",
                                paste0("ckd_bronze_", date, ".json")),
             pretty = TRUE)
}



#' Ensure informative variable names and accurate data values
#'
#' `transform_to_silver()` removed two columns ("sodium" and "potassium") due
#' to excessive missingness (> 20%), replaces encoded variable names with
#' human-readable names, cleans variable values, and standardizes missing values
#' to NA.
#'
#' @param datafile A data.frame derived from `extract_to_bronze()` function.
#' @returns A data.frame with two columns removed compared to input.
#' @example
#' bronze_data <- extract_to_bronze("Chronic Kidney Disease")
#' transform_to_silver(bronze_data)
transform_to_silver <- function(datafile) {
  # transform names
  colnames(datafile) <- c("age", "blood_pressure", "specific_gravity",
                          "albumin", "sugar", "red_blood_cells", "pus_cell",
                          "pus_cell_clumps", "bacteria", "blood_glucose_random",
                          "blood_urea", "serum_creatinine", "sodium",
                          "potassium", "hemoglobin", "packed_cell_volume",
                          "white_blood_cell_count", "red_blood_cell_count",
                          "hypertension", "diabetes_mellitus",
                          "coronary_artery_disease", "appetite", "pedal_adema",
                          "anemia", "classification")

  # remove columns with too much missingness
  which_sodium_potassium <- c(which(names(datafile) == "sodium"),
                              which(names(datafile) == "potassium"))

  data <- datafile[,-which_sodium_potassium]

  # handle missingness
  data[data == ""] <- NA

  # set variable class
  data$packed_cell_volume <- as.numeric(data$packed_cell_volume)
  data$white_blood_cell_count <- as.numeric(data$white_blood_cell_count)
  data$red_blood_cell_count <- as.numeric(data$red_blood_cell_count)

  # clean variable values
  data$diabetes_mellitus[data$diabetes_mellitus == " yes"] <- "yes"
  data$diabetes_mellitus[data$diabetes_mellitus == "\tno"] <- "no"
  data$diabetes_mellitus[data$diabetes_mellitus == "\tyes"] <- "yes"
  data$coronary_artery_disease[data$coronary_artery_disease == "\tno"] <- "no"
  data$classification[data$classification == "ckd\t"] <- "ckd"

  return(data)
}


# unit tests for transform_to_silver()
names(ckd) # test that names changed to long_form
ncol(ckd)   # test that we removed two columns
sum(is.na(ckd))   # test that we increased number of NAs
class(ckd$packed_cell_volume)     # test that class equals "numeric"
class(ckd$white_blood_cell_count)  # test that class equals "numeric"
class(ckd$red_blood_cell_count)    # test that class equals "numeric"
unique(ckd$diabetes_mellitus)   # test that all values are "yes", "no", or NA
unique(ckd$coronary_artery_disease)  # test that all values are "yes", "no", or NA
unique(ckd$classification)      # test that all values are "ckd" or "notckd"


#' Save a silver dataset as a CSV file
#'
#' @param datafile A data.frame derived from transform_to_silver()
#' @returns A .csv file downloaded to /RenalFailuR/data
#' @example
#' silver_data <- extract_to_bronze("Chronic Kidney Disease") |>
#'   silver_to_csv()
silver_to_csv <- function(datafile) {
  write.csv(datafile,
            file = system.file("data",
                               paste0("ckd_silver_", date, ".csv")))
}


#' Create a numeric dichotomous outcome variable for use in a logistic
#' regression (glm) model
#'
#' `load_to_gold()` derives a 0/1 outcome variable from "classification", with
#' 0 = "nockd", no disease and 1 = "ckd", disease.
#'
#' @param datafile A data.frame derived from `transform_to_silver()` function.
#' @returns A data.frame with a new column, "outcome" with 0/1 values.
#' @example
#' bronze_data <- extract_to_bronze("Chronic Kidney Disease") |>
#'  transform_to_silver() |>
#'  load_to_gold()
load_to_gold <- function(dataset) {
  dataset$outcome = dataset$classification
  dataset$outcome[dataset$outcome == "ckd"] <- 1
  dataset$outcome[dataset$outcome == "notckd"] <- 0
  dataset$outcome <- as.numeric(dataset$outcome)

  return(dataset)
}


# unit tests for laod_to_gold()
sum(gold$outcome)    # test that ckd cases equals 250



#' Save a gold file as a csv
#'
#' @param dataset A data.frame derived from load_to_gold() function.
#' @returns A .csv file saved to /RenalFailuR/data.
#' @example
#' bronze_data <- extract_to_bronze("Chronic Kidney Disease") |>
#'  transform_to_silver() |>
#'  load_to_gold() |>
#'  gold_to_csv()
gold_to_csv <- function(dataset) {
  write.csv(datafile,
            file = system.file("data",
                               paste0("ckd_gold_", date, ".csv")))
}


#------------- create_gt_table() ---------------#
#' Summarize disease group comparability in a table
#'
#' Creates a table comparing variable means, sds, and number of non-missing
#' observations for numeric variables in a silver or gold datafile.
#'
#' @param A data.frame derived from transform_to_silver() or load_to_gold()
#' functions
#' @returns A table suitable for gt() function
#' @example
#' silver_data <- extract_to_bronze("Chronic Kidney Disease") |>
#'  transform_to_silver()
#' create_gt_table(silver_data)
create_gt_table <- function(datafile) {
  means <- datafile |>
    group_by(classification) |>
    summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))

  sds <- datafile |>
    group_by(classification) |>
    summarize(across(where(is.numeric), ~sd(.x, na.rm = TRUE)))

  ns <- datafile |>
    group_by(classification) |>
    summarize(across(where(is.numeric), list(n = ~sum(!is.na(.x)))))

  names <- sub("_mean", "", names(means))
  table = suppressWarnings(
    data.frame(Variable = names,
               Mean_ckd = as.numeric(means[1,]),
               sd_ckd = -as.numeric(sds[1,]),
               n_ckd = as.numeric(ns[1,]),
               perc_ckd = as.numeric(ns[1,])/sum(as.numeric(ns[1,]), na.rm = TRUE),
               Mean_nockd = as.numeric(means[2,]),
               sd_nockd = -as.numeric(sds[2,]),
               n_nockd = as.numeric(ns[2,]),
               perc_nockd = as.numeric(ns[2,])/sum(as.numeric(ns[2,]), na.rm = TRUE))
  )
  table <- table[3:14,]

  return(table)
}



