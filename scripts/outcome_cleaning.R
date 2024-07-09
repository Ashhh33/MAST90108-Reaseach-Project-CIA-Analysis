library(tidyverse)


removeOutcomeNaRecords <- function(data) {
  removed_index = numeric(0)
  for (i in 1:(nrow(data)-1)) {
    if (sum(is.na(data[i, ])) > ((length(data[i, ]) - 1) / 2)) {
      removed_index = c(removed_index, i)
    }
  }
  return(removed_index)
}


replaceNaWithLast <- function(vec){
  if (is.na(vec[1])) 
    vec[1] = na.omit(vec)[1]
  for (i in 2:length(vec)) {
    if (is.na(vec[i])) 
      vec[i] = vec[i - 1]
    }
  return(vec)
}


replaceSingleDifference <- function(vec) {
  for (i in 2:(length(vec)-1)) {
    if (vec[i] != vec[i - 1] & vec[i - 1] == vec[i + 1]) {
      vec[i] = vec[i - 1]
    }
  }
  return(vec)
}

combineAcrossTime <- function(df) {
  survive = vector(length=nrow(df))
  time = vector(length=nrow(df))
  time_vec = c(3, 6, 12, 24, 36, 48, 60)
  for (i in 1:nrow(df)){
    for (j in 1:ncol(df)) {
      if (df[i, j] == 0) {
        survive[i] = 1
        time[i] = time_vec[j]
        break
      } else {
        time[i] = time_vec[7]
      }
    }
  }
  return(data.frame(survive, time))
}

cleanOutcomeData <- function(file_path) {
  SAVE_PATH_ORIGINAL = './preprocessed-data/Amen-outcome-data_cleaned.csv'
  SAVE_PATH_SURV = './preprocessed-data/Amen-outcome-data_surv.csv'
  
  raw_data = read_csv(file_path)
  removed_index = removeOutcomeNaRecords(raw_data)
  df = data.frame(raw_data[-removed_index, ])
  
  ProcessOutcomes(df)
  
  ID = df[, ncol(df)]
  
  outcomes = df[, -c(ncol(df))]
  outcomes = as.data.frame(t(apply(outcomes, 1, replaceNaWithLast)))
  outcomes = as.data.frame(t(apply(outcomes, 1, replaceSingleDifference)))
  
  surv_df = combineAcrossTime(outcomes)
    
  
  outcomes$ID = ID
  surv_df$ID = ID
  
  write_csv(outcomes, SAVE_PATH_ORIGINAL)
  write_csv(surv_df, SAVE_PATH_SURV)
  
  return(c(outcome_df_cleand_path = SAVE_PATH_ORIGINAL,
           surv_df_cleand_path = SAVE_PATH_SURV))
}

#(cleanOutcomeData('./preprocessed-data/Amen-outcome-data.csv'))

