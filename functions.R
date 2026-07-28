```{r sessionSetup, echo = FALSE, message = FALSE}
library(here)
```


## ORIGINAL
```{r extract_to_bronze, include = FALSE}
## EXTRACT DATASET
# why:  Extract dataset from UCI Machine Learning Repository and archive
# what: Input dataset name. Output bronze dataset.
# how:  fetch_ucirepo()    # downloads dataset and metadata

#install.packages("ucimlrepo", repos = c('https://coatless-rpkg.r-universe.dev', 'https://cloud.r-project.org'))

library(ucimlrepo)
library(jsonlite)

chronic_kidney_disease <- fetch_ucirepo(name = "Chronic Kidney Disease")

# Save a bronze version as a JSON file
str(chronic_kidney_disease)
write_json(chronic_kidney_disease,
           path = here("data", paste0("ckd_bronze_", date, ".json")), pretty = TRUE)

# Follow UCI MLR website instructions to construct dataset
ckd_data = chronic_kidney_disease$data
X = ckd_data$features
Y = ckd_data$targets

ckd = cbind(X, Y)
class(ckd)  # test that class equals "data.frame"

dim(ckd)    # test that dimensions equals (400   25)
```


# Dependencies
library(ucimlrepo)
library(jsonlite)

#----------- extract_to_bronze() --------------#
# why:  Extract dataset from UCI Machine Learning Repository and archive
# what: Input dataset name. Output bronze dataset.

extract_to_bronze <- function(name) {
  mlr_data <- fetch_ucirepo(name = name)

  just_data = mlr_data$data
  X = just_data$features
  Y = just_data$targets

  dataset = cbind(X, Y)

  return(dataset)
}

# example
extract_to_bronze("Chronic Kidney Disease")

# unit test for extract_to_bronze
class(ckd)  # test that class equals "data.frame"
dim(ckd)    # test that dimensions equals (400   25)

# Save a bronze version as a JSON file
write_json(datafile,
            path = here("data", paste0("ckd_bronze_", date, ".json")), pretty = TRUE)



#------------ transform_to_silver() ----------------#
# why:  Ensure informative variable names and accurate data values
# what: Input a vector of column names. Output a vector of column names.

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

# example
bronze_data <- extract_to_bronze("Chronic Kidney Disease")
transform_to_silver(bronze_data)

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


# Save a silver file as a .csv
write.csv(ckd,
          file = here("data",
                      paste0("ckd_silver_", date, ".csv")))
```




```{r load_to_gold, include = FALSE}

ckd$outcome = ckd$classification
ckd$outcome[ckd$outcome == "ckd"] <- 1
ckd$outcome[ckd$outcome == "notckd"] <- 0
ckd$outcome <- as.numeric(ckd$outcome)

# Fit the model
glm1 <- glm(outcome ~ serum_creatinine, data = ckd,
            family = binomial)
summary(glm1)

creat_est = glm1$coef[[2]]
```




```{r gt_table, echo = FALSE, message = FALSE, warning = FALSE}
# CREATE TABLE 1
# why:  Summarize disease group comparability in a table
# what: Input a prepared data.frame. Output a table.
# how:


library(dplyr)
library(gt)
means <- ckd |>
            group_by(classification) |>
            summarize(across(where(is.numeric), list(mean = ~mean(.x, na.rm = TRUE))))

sds <- ckd |>
  group_by(classification) |>
  summarize(across(where(is.numeric), ~sd(.x, na.rm = TRUE)))

ns <- ckd |>
          group_by(classification) |>
          summarize(across(where(is.numeric), list(n = ~sum(!is.na(.x)))))

names <- sub("_mean", "", names(means))
table1 = data.frame(Variable = names,
                    Mean_ckd = as.numeric(means[1,]),
                    sd_ckd = -as.numeric(sds[1,]),
                    n_ckd = as.numeric(ns[1,]),
                    perc_ckd = as.numeric(ns[1,])/sum(as.numeric(ns[1,]), na.rm = TRUE),
                    Mean_nockd = as.numeric(means[2,]),
                    sd_nockd = -as.numeric(sds[2,]),
                    n_nockd = as.numeric(ns[2,]),
                    perc_nockd = as.numeric(ns[2,])/sum(as.numeric(ns[2,]), na.rm = TRUE))

table1 <- table1[3:14,]

table1 |>
  gt() |>
    tab_header(
    title = md("**Table 1: Laboratory Value Comparisons**"),
    subtitle = "Chronic Kidney Disease (CKD) Patients and Non-CKD Patients") |>
    tab_stubhead(
      label = "Variable") |>
    tab_spanner(
      label = "Chronic Kidney Disease",
      columns = c(Mean_ckd, sd_ckd, n_ckd, perc_ckd)) |>
    tab_spanner(
      label = "No Disease",
      columns = c(Mean_nockd, sd_nockd, n_nockd, perc_nockd)) |>
    cols_label(
      Mean_ckd = html("Mean"),
      sd_ckd = html("sd"),
      n_ckd = html("n"),
      perc_ckd = html("(%)"),
      Mean_nockd = html("Mean"),
      sd_nockd = html("sd"),
      n_nockd = html("n"),
      perc_nockd = html("(%)")) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      weight = px(1)),
    locations = cells_body(
      columns = Mean_nockd)) |>
  tab_style(
    style = cell_borders(
      sides = "left",
      weight = px(2)),
    locations = cells_body(
      columns = Mean_ckd)) |>
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(sd_ckd, sd_nockd))) |>
   fmt_number(
    columns = c(Mean_ckd, sd_ckd, perc_ckd, Mean_nockd, sd_nockd, perc_nockd),
    decimals = 2) |>
  fmt_percent(
    columns = c(perc_ckd, perc_nockd)) |>
  fmt_number(
    columns = c(sd_ckd, sd_nockd),
    accounting = TRUE)
```



