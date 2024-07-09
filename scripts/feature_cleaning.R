library(tidyverse)

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

replace_na_by_mean_mode <- function(x) {
  if(is.factor(x)) {
    replace(x, is.na(x), mode(na.omit(x)))
  }
  else if(is.numeric(x)) {
    replace(x, is.na(x), mean(x, na.rm=TRUE))
  }
}

cleanFeatureData <- function(file_path) {
  SAVE_PATH = "./preprocessed-data/feature-data_cleaned.csv"
  df = read_csv(file_path)
  ID = as.vector(df[, ncol(df)])

  predictors = df[, -c(ncol(df))]
  
  predictors = replace_na(predictors, list(rep(99, 5))) # ???
  predictors[, c(2:4)] = lapply(predictors[, c(2:4)], factor)
  
  # Missing values in CMF_Cycles means no CMF recieved
  predictors$CMF_Cycles[is.na(predictors$CMF_Cycles)] = 0
  predictors[predictors == 99] = NA
  
  # replace other NAs by mode
  predictors_naRemoved = data.frame(sapply(predictors, replace_na_by_mean_mode))
  predictors_naRemoved[, c(2:4)] = lapply(predictors_naRemoved[, c(2:4)], factor, 
                                          levels = c(1, 2), 
                                          labels = c("No", "Yes"))
  
  predictors_naRemoved$CMF_Cycles = factor(predictors_naRemoved$CMF_Cycles)
  predictors_naRemoved = cbind(predictors_naRemoved, ID)
  write_csv(predictors_naRemoved, SAVE_PATH)
  
  return (c(predictors_df_cleand_path = SAVE_PATH))
}

#cleanFeatureData("./preprocessed-data/feature-data.csv")
