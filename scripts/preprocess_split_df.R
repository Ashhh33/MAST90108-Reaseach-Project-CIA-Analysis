library(readxl)
library(tidyverse)
library(dplyr)
library(xlsx)
library(readr)

# colnames for either not relevant or duplicated with others
FILTER_FROM_RAW_DATA_COL <- c(
  "ID",
  "Data_track",
  "Age",
  "Agediag_category",
  "Age_dagnosis",
  "Prg1_age",
  "Tumor_Size",
  "Nodal_Status",
  "CT_flg",
  "Chemoregim",
  "CMF"
)

# colnames for cols include identical vals
SINGLE_VALUE_COL <- c(
  "Chemo_Cat",
  "Neoadj_CT",
  "AC",
  "AC.CMF",
  "AC.T",
  "Taxel"
)

FILTERED_COL <- c(
  FILTER_FROM_RAW_DATA_COL,
  SINGLE_VALUE_COL
)

#OUTCOMES_1 <- c(
#  "Amen_ST3",
#  "Amen_ST6",
#  "Amen_ST12",
#  "Amen_ST24",
#  "AmenST36",
#  "AmenST48",
#  "Amen_ST60"
#)

#OUTCOMES_2 <- c(
#  grep('menstatus', raw_colnames, value=TRUE)
#)


getPreprocessedData <- function(file_path) {
  SAVE_PATH_FEATURE = "./preprocessed-data/feature-data.csv"
  SAVE_PATH_OUTCOME_AMEN = "./preprocessed-data/Amen-outcome-data.csv"
  SAVE_PATH_OUTCOME_MENSTATUS = "./preprocessed-data/Menstatus-outcome-data.csv"
  
  RAW_DATA = data.frame(read_xlsx(file_path, col_names = TRUE))
  RAW_COL_NAMES = names(RAW_DATA)
  PATIENT_ID = RAW_DATA$ID
  
  OUTCOMES_1 = c(grep('Amen', RAW_COL_NAMES, value = TRUE))
  OUTCOMES_2 = c(grep('menstatus', RAW_COL_NAMES, value = TRUE))
  
  filtered_data = RAW_DATA[setdiff(RAW_COL_NAMES, FILTERED_COL)]
  
  feature_cols_names = setdiff(names(filtered_data), c(OUTCOMES_1, OUTCOMES_2))
  
  feature_df = filtered_data[feature_cols_names]
  feature_df$ID = PATIENT_ID
  outcome_df1 = filtered_data[OUTCOMES_1]
  outcome_df1$ID = PATIENT_ID
  outcome_df2 = filtered_data[OUTCOMES_2]
  outcome_df2$ID = PATIENT_ID
  
  write_csv(feature_df, SAVE_PATH_FEATURE)
  write_csv(outcome_df1, SAVE_PATH_OUTCOME_AMEN)
  write_csv(outcome_df2, SAVE_PATH_OUTCOME_MENSTATUS)
  
  return(c(feature_df_path =  SAVE_PATH_FEATURE, 
           outcome_df1_path = SAVE_PATH_OUTCOME_AMEN,
           outcome_df2_path = SAVE_PATH_OUTCOME_MENSTATUS))
}

#getPreprocessedData("./dataset_d.xlsx")

